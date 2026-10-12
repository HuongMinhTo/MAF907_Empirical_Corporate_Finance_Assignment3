# Robustness of the pecking order slope b to the data-handling choices.
# Each row re-estimates dD = a + b DEF for F&G's two sub-periods and for
# 1971-1998 and 1999-2025.

source("code/00_config.R")
load_packages(c("data.table", "fixest", "sandwich"))
setFixest_notes(FALSE)

d   <- readRDS(file.path(PROC_DIR, "panel.rds"))
raw <- readRDS(file.path(PROC_DIR, "panel_untrimmed.rds"))
all <- readRDS(file.path(PROC_DIR, "panel_allcompustat.rds"))
TAB <- file.path(OUT_DIR, "tables"); MD <- file.path(OUT_DIR, "tables_md")
f3 <- function(x) formatC(x, format = "f", digits = 3)

periods <- c("ssm", "nineties", "orig", "ext")
est <- function(x, label, y = "nd_na", xv = "def_na", fe = FALSE) {
  rbindlist(lapply(periods, function(p) {
    z <- x[fyear >= PERIODS[[p]][1] & fyear <= PERIODS[[p]][2] & !is.na(get(y)) & !is.na(get(xv))]
    f <- as.formula(paste(y, "~", xv, if (fe) "| gvkey" else ""))
    m <- feols(f, z, cluster = ~gvkey)
    data.table(spec = label, period = PERIOD_LABEL[[p]], b = coef(m)[[xv]], se = se(m)[[xv]],
               r2 = if (fe) r2(m, "wr2") else r2(m, "r2"), N = nobs(m))
  }))
}
base <- d[in_reg == TRUE]

# Alternative outlier treatments, starting from the untrimmed panel
trim_vars <- c("def_na", "nd_na", "ne_na")
alt <- function(x, mode = c("none", "winsor", "annual")) {
  mode <- match.arg(mode)
  x <- x[!is.na(net_assets) & net_assets > 0 & !is.na(def_na) & !is.na(nd_na) & !is.na(ne_na)]
  if (mode == "none") return(x)
  for (v in trim_vars) {
    grp <- if (mode == "annual") x$fyear else fifelse(x$fyear <= 1998, 1L, 2L)
    z <- x[[v]]
    q_lo <- ave(z, grp, FUN = function(u) quantile(u, TRIM_P)); q_hi <- ave(z, grp, FUN = function(u) quantile(u, 1 - TRIM_P))
    if (mode == "winsor") set(x, j = v, value = pmin(pmax(z, q_lo), q_hi))
    else x <- x[z >= q_lo & z <= q_hi]
  }
  x
}

# Fama-MacBeth: mean of annual slopes, Newey-West (2 lags) standard error
fmb <- function(x) rbindlist(lapply(periods, function(p) {
  z <- x[fyear >= PERIODS[[p]][1] & fyear <= PERIODS[[p]][2]]
  bt <- z[, .(b = coef(lm(nd_na ~ def_na))[2], r2 = summary(lm(nd_na ~ def_na))$r.squared), by = fyear]
  nw <- sqrt(NeweyWest(lm(b ~ 1, bt), lag = 2, prewhite = FALSE)[1, 1])
  data.table(spec = "Fama-MacBeth (mean of annual b_t, Newey-West se)", period = PERIOD_LABEL[[p]],
             b = mean(bt$b), se = nw, r2 = mean(bt$r2), N = nrow(z))
}))

# Conventional (iid) standard errors, as in F&G
iid <- rbindlist(lapply(periods, function(p) {
  z <- base[fyear >= PERIODS[[p]][1] & fyear <= PERIODS[[p]][2]]
  m <- feols(nd_na ~ def_na, z, vcov = "iid")
  data.table(spec = "Baseline, conventional OLS standard errors (as in F&G)", period = PERIOD_LABEL[[p]],
             b = coef(m)[2], se = se(m)[2], r2 = r2(m, "r2"), N = nobs(m))
}))

all_reg <- all[in_reg == TRUE]
rob <- rbind(
  est(base, "Baseline: CCM firms, 0.5% trim within sub-period, net-asset scaling"),
  iid,
  est(alt(raw, "none"),   "No trimming"),
  est(alt(raw, "winsor"), "Winsorise at 0.5% / 99.5% instead of trimming"),
  est(alt(raw, "annual"), "Trim 0.5% within each fiscal year"),
  est(all_reg, "All Compustat firms (no CRSP/CCM link required)"),
  est(all_reg[in_ccm == FALSE], "Compustat firms WITHOUT a CCM link only"),
  est(d[!is.na(def_at) & !is.na(nd_at) & !is.na(ne_at)], "Scale by total assets instead of net assets", y = "nd_at", xv = "def_at"),
  est(base[fyear > first_fyear], "Drop each firm's first Compustat year (IPO year)"),
  est(base[scf == 7L | fyear <= 1987], "Format code 7 only after 1987"),
  est(base, "Firm fixed effects (within R2)", fe = TRUE),
  fmb(base))
fwrite(rob, file.path(TAB, "robustness_raw.csv"))

wide <- dcast(rob, spec ~ period, value.var = c("b", "se", "r2", "N"))
wide <- wide[match(unique(rob$spec), spec)]
out <- data.table(Specification = wide$spec)
for (p in PERIOD_LABEL[periods]) {
  out[[paste0(p, " b (se)")]] <- paste0(f3(wide[[paste0("b_", p)]]), " (", f3(wide[[paste0("se_", p)]]), ")")
  out[[paste0(p, " R2")]] <- f3(wide[[paste0("r2_", p)]])
}
out[[paste0("N 1971-1989")]] <- formatC(wide$`N_1971-1989`, format = "d", big.mark = ",")
hdr <- paste0("| ", paste(names(out), collapse = " | "), " |")
sep <- paste0("|", paste(c(":---", rep("---:", ncol(out) - 1)), collapse = "|"), "|")
body <- apply(out, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
writeLines(c("**Robustness of the pecking order slope to data-handling choices**", "", hdr, sep, body, "",
             "*F&G benchmarks: 1971-1989 b = 0.283, R2 = 0.265, N = 89,883; 1990-1998 b = 0.148, R2 = 0.120, N = 57,687. Firm-clustered SEs unless stated.*"),
           file.path(MD, "table_robustness.md"))
fwrite(out, file.path(TAB, "table_robustness.csv"))

# Accounting identity dD + dE = DEF in the regression sample
id <- base[, .(share_exact_1e3 = mean(abs(nd_na + ne_na - def_na) < 1e-3),
               mean_abs_gap = mean(abs(nd_na + ne_na - def_na))),
           by = .(period = fifelse(fyear <= 1998, "1971-1998", "1999-2025"), fmt = fifelse(scf == 7L, "format 7", "formats 1-3"))]
fwrite(id[order(period, fmt)], file.path(TAB, "identity_check.csv"))
print(id[order(period, fmt)])
print(out)
