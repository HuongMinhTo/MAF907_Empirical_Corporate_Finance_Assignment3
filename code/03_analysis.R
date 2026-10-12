# All tables and figures for Questions 1-4.
# Reads data/processed/panel.rds (built by 02_build_sample.R) and writes CSV tables to
# output/tables/, Markdown versions to output/tables_md/ and PNG figures to
# output/figures/.

source("code/00_config.R")
load_packages(c("data.table", "fixest", "sandwich", "ggplot2", "strucchange"))
source("code/paper_values.R")

d <- readRDS(file.path(PROC_DIR, "panel.rds"))
TAB <- file.path(OUT_DIR, "tables");    dir.create(TAB, showWarnings = FALSE)
MD  <- file.path(OUT_DIR, "tables_md"); dir.create(MD,  showWarnings = FALSE)
FIG <- file.path(OUT_DIR, "figures");   dir.create(FIG, showWarnings = FALSE)
setFixest_notes(FALSE)

in_period <- function(x, p) x$fyear >= PERIODS[[p]][1] & x$fyear <= PERIODS[[p]][2]
f3  <- function(x) ifelse(is.na(x), "", formatC(x, format = "f", digits = 3))
fN  <- function(x) ifelse(is.na(x), "", formatC(x, format = "d", big.mark = ","))
stars <- function(p) ifelse(is.na(p), "", ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .10, "*", ""))))

# Pipe-table writer for the report
write_md <- function(df, name, caption = NULL, note = NULL) {
  df <- as.data.frame(df)
  df[] <- lapply(df, function(z) { z <- as.character(z); z[is.na(z)] <- ""; z })
  hdr <- paste0("| ", paste(names(df), collapse = " | "), " |")
  sep <- paste0("|", paste(c(":---", rep("---:", ncol(df) - 1)), collapse = "|"), "|")
  body <- apply(df, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
  out <- c(if (!is.null(caption)) c(paste0("**", caption, "**"), ""), hdr, sep, body,
           if (!is.null(note)) c("", paste0("*", note, "*")))
  writeLines(out, file.path(MD, paste0(name, ".md")))
  fwrite(df, file.path(TAB, paste0(name, ".csv")))
  invisible(df)
}

# Pooled OLS with firm-clustered SEs. Returns a one-row summary.
po_reg <- function(data, y = "nd_na", x = "def_na", label = "") {
  data <- data[!is.na(get(y)) & !is.na(get(x))]
  m <- feols(as.formula(paste(y, "~", x)), data = data, cluster = ~gvkey)
  ct <- coeftable(m)
  b <- ct[x, 1]; se <- ct[x, 2]
  data.table(label = label, a = ct[1, 1], a_se = ct[1, 2], a_p = ct[1, 4],
             b = b, b_se = se, b_p = ct[x, 4], t_b1 = (b - 1) / se,
             N = nobs(m), firms = uniqueN(data$gvkey), r2 = r2(m, "r2"))
}

# Wald test R b = r with the clustered covariance matrix
wald <- function(m, R, r) {
  b <- coef(m); V <- vcov(m)
  diff <- R %*% b - r
  stat <- as.numeric(t(diff) %*% solve(R %*% V %*% t(R)) %*% diff)
  c(chi2 = stat, df = nrow(R), p = pchisq(stat, nrow(R), lower.tail = FALSE))
}

# Out-of-sample R2 (F&G p. 223): regress actual on predicted, report R2
holdout_r2 <- function(m, newdata, y, x = "def_na") {
  hd <- as.data.frame(newdata)[, c(y, x)]
  hd <- hd[complete.cases(hd), , drop = FALSE]
  if (nrow(hd) < 10) return(NA_real_)
  pred <- coef(m)[1] + coef(m)[2] * hd[[x]]
  cor(pred, hd[[y]])^2
}

base <- d[in_reg == TRUE]
pal <- c(def = "#2a78d6", nd = "#eb6834", ne = "#1baf7a", other = "#52514e")
theme_fg <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(colour = "#e6e5e1", linewidth = .3),
        legend.position = "bottom", legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 12), plot.background = element_rect(fill = "white", colour = NA))

# =============================================================================
# Q1. Tables 1 and 2, descriptive statistics, structural breaks
# =============================================================================

## Table 1: common-size balance sheet --------------------------------------
t1_items <- data.table(
  var = c("bs_ch", "bs_ivst", "bs_rect", "bs_invt", "bs_aco", "bs_act", "bs_ppent", "bs_ivaeq",
          "bs_ivao", "bs_intan", "bs_ao", "bs_dlc", "bs_ap", "bs_txp", "bs_lco", "bs_lct",
          "bs_dltt", "bs_lo", "bs_txditc", "bs_mib", "bs_lt", "bs_pstk", "bs_ceq", "bs_seq"),
  item = c("+Cash (#162)", "+Short-term investment (#193)", "+Receivables-total (#2)",
           "+Inventories-total (#3)", "+Current assets-other (#68)", "=Current assets-total (#4)",
           "+Net PP&E (#8)", "+Investments & advances-equity (#31)", "+Investments & advances-other (#32)",
           "+Intangibles (#33)", "+Assets-other (#69)", "+Debt in current liabilities (#34)",
           "+Accounts payable (#70)", "+Income taxes payable (#71)", "+Current liabilities-other (#72)",
           "=Current liabilities-total (#5)", "+Long-term debt-total (#9)", "+Liabilities-other (#75)",
           "+Deferred taxes and ITC (#35)", "+Minority interest (#38)", "=Liabilities-total (#181)",
           "+Preferred stock (#130)", "+Common equity-total (#60)", "=Stockholders' equity (#216)"))
t1_sample <- d[!is.na(at) & at > 0]
t1_year <- t1_sample[, c(list(N = .N), lapply(.SD, mean, na.rm = TRUE)), by = fyear, .SDcols = t1_items$var]
t1_per  <- rbind(
  t1_sample[in_period(t1_sample, "orig"), c(list(fyear = "1971-98", N = .N), lapply(.SD, mean, na.rm = TRUE)), .SDcols = t1_items$var],
  t1_sample[in_period(t1_sample, "ext"),  c(list(fyear = "1999-25", N = .N), lapply(.SD, mean, na.rm = TRUE)), .SDcols = t1_items$var],
  t1_sample[in_period(t1_sample, "full"), c(list(fyear = "1971-25", N = .N), lapply(.SD, mean, na.rm = TRUE)), .SDcols = t1_items$var])
fwrite(t1_year, file.path(TAB, "table1_by_year_all.csv"))

make_t1 <- function(years) {
  rows <- lapply(c("N", t1_items$var), function(v) {
    lab <- if (v == "N") "Number of observations" else t1_items[var == v, item]
    vals <- sapply(years, function(y) { z <- t1_year[fyear == y][[v]]; if (length(z) == 0) NA else z })
    c(item = lab, setNames(if (v == "N") fN(vals) else f3(vals), years))
  })
  as.data.table(do.call(rbind, rows))
}
# Side-by-side with the paper: replication / paper for each year
t1_orig <- make_t1(T12_YEARS_ORIG)
for (y in as.character(T12_YEARS_ORIG)) {
  pv <- PAPER_T1[[y]]
  t1_orig[[y]] <- paste0(t1_orig[[y]], " (", c(fN(pv[1]), f3(pv[-1])), ")")
}
write_md(t1_orig, "table1_orig",
         "Table 1 (replicated): Common-size balance sheet, 1971-1998. Replication (Frank & Goyal 2003 in parentheses)",
         "Average of each item / total assets across firm-years in the year. CRSP/Compustat Merged firms; financials, utilities, AB mergers and format codes 4-6 excluded; items trimmed at 0.5% per tail.")
t1_ext <- make_t1(T12_YEARS_EXT)
per_cols <- sapply(c("N", t1_items$var), function(v) t1_per[[v]])
t1_ext[["1971-98 avg"]] <- c(fN(per_cols[1, 1]), f3(per_cols[1, -1]))
t1_ext[["1999-25 avg"]] <- c(fN(per_cols[2, 1]), f3(per_cols[2, -1]))
t1_ext[["1971-25 avg"]] <- c(fN(per_cols[3, 1]), f3(per_cols[3, -1]))
write_md(t1_ext, "table1_ext", "Table 1 (extended): Common-size balance sheet, 2000-2025, and period averages",
         "Period columns are pooled firm-year averages; N is firm-years.")

## Table 2: corporate cash flows --------------------------------------------
t2_sample <- d[!is.na(div_at) & !is.na(inv_at) & !is.na(dwc_at) & !is.na(cf_at) & !is.na(nd_at) & !is.na(ne_at)]
t2_vars <- c("div_at", "inv_at", "dwc_at", "cf_at", "def_at", "nd_at", "ne_at")
t2_sample[, ext_at := nd_at + ne_at]
t2_sample[, def_at2 := div_at + inv_at + dwc_at - cf_at]  # rows add up on the common sample
t2_year <- t2_sample[, c(list(N = .N), lapply(.SD, mean)), by = fyear, .SDcols = c(t2_vars, "ext_at", "def_at2")]
t2_lab <- c(N = "Number of observations", div_at = "Cash dividends", inv_at = "Investments",
            dwc_at = "Delta working capital", cf_at = "Internal cash flow",
            def_at2 = "Financing deficit (a+b+c-d)", nd_at = "Net debt issues (#111-#114)",
            ne_at = "Net equity issues (#108-#115)", ext_at = "Net external financing")
make_t2 <- function(years) {
  rows <- lapply(names(t2_lab), function(v) {
    vals <- sapply(years, function(y) { z <- t2_year[fyear == y][[v]]; if (length(z) == 0) NA else z })
    c(item = t2_lab[[v]], setNames(if (v == "N") fN(vals) else f3(vals), years))
  })
  as.data.table(do.call(rbind, rows))
}
t2_orig <- make_t2(T12_YEARS_ORIG)
for (y in as.character(T12_YEARS_ORIG)) {
  pv <- PAPER_T2[[y]]
  t2_orig[[y]] <- paste0(t2_orig[[y]], " (", c(fN(pv[1]), f3(pv[-1])), ")")
}
write_md(t2_orig, "table2_orig",
         "Table 2 (replicated): Corporate cash flows as a fraction of total assets, 1971-1998. Replication (Frank & Goyal 2003 in parentheses)",
         "Common sample with all components non-missing after trimming, so the rows add up. Financing deficit = dividends + investment + change in working capital - internal cash flow.")
t2_per <- rbind(
  t2_sample[in_period(t2_sample, "orig"), c(list(N = .N), lapply(.SD, mean)), .SDcols = c(t2_vars, "ext_at", "def_at2")],
  t2_sample[in_period(t2_sample, "ext"),  c(list(N = .N), lapply(.SD, mean)), .SDcols = c(t2_vars, "ext_at", "def_at2")],
  t2_sample[in_period(t2_sample, "full"), c(list(N = .N), lapply(.SD, mean)), .SDcols = c(t2_vars, "ext_at", "def_at2")])
t2_ext <- make_t2(T12_YEARS_EXT)
t2_ext[["1971-98 avg"]] <- c(fN(t2_per$N[1]), f3(unlist(t2_per[1, names(t2_lab)[-1], with = FALSE])))
t2_ext[["1999-25 avg"]] <- c(fN(t2_per$N[2]), f3(unlist(t2_per[2, names(t2_lab)[-1], with = FALSE])))
t2_ext[["1971-25 avg"]] <- c(fN(t2_per$N[3]), f3(unlist(t2_per[3, names(t2_lab)[-1], with = FALSE])))
write_md(t2_ext, "table2_ext", "Table 2 (extended): Corporate cash flows as a fraction of total assets, 2000-2025, and period averages")
fwrite(t2_year, file.path(TAB, "table2_by_year_all.csv"))

## Descriptive statistics of the regression variables by period (Table 9 style)
desc_vars <- c(div_na = "Cash dividends / net assets", inv_na = "Investment / net assets",
               dwc_na = "Delta working capital / net assets", cf_na = "Internal cash flow / net assets",
               def_na = "Financing deficit / net assets", gd_na = "Gross debt issued / net assets",
               nd_na = "Net debt issued / net assets", ne_na = "Net equity issued / net assets",
               cmltd_na = "Current maturity of LTD / net assets", dltd_ratio = "Change in LTD / net assets ratio",
               tang = "Tangibility (PPE / assets)", mtb = "Market-to-book assets", prof = "Profitability (OIBDP / assets)",
               rd_at = "R&D / assets", cash_at = "Cash & ST inv. / assets", log_at = "log total assets ($m)")
desc <- rbindlist(lapply(c("orig", "ext"), function(p) {
  x <- base[in_period(base, p)]
  rbindlist(lapply(names(desc_vars), function(v) {
    z <- x[[v]]; z <- z[is.finite(z)]
    data.table(period = PERIOD_LABEL[[p]], variable = desc_vars[[v]], N = length(z), mean = mean(z), sd = sd(z),
               p1 = quantile(z, .01), p25 = quantile(z, .25), median = median(z), p75 = quantile(z, .75), p99 = quantile(z, .99))
  }))
}))
# KS test and difference in means between the two periods
ks <- rbindlist(lapply(names(desc_vars), function(v) {
  a <- base[in_period(base, "orig")][[v]]; b <- base[in_period(base, "ext")][[v]]
  a <- a[is.finite(a)]; b <- b[is.finite(b)]
  k <- suppressWarnings(ks.test(a, b))
  data.table(variable = desc_vars[[v]], mean_71_98 = mean(a), mean_99_25 = mean(b),
             median_71_98 = median(a), median_99_25 = median(b), KS_D = unname(k$statistic), KS_p = k$p.value)
}))
desc_wide <- dcast(desc, variable ~ period, value.var = c("N", "mean", "sd", "median"))
desc_wide <- desc_wide[match(desc_vars, variable)]
paper_t9 <- PAPER_T9[match(desc_wide$variable, names(PAPER_T9))]
desc_md <- data.table(Variable = desc_wide$variable,
                      `FG mean (sd) 1971-98` = ifelse(is.na(paper_t9), "", paper_t9),
                      `N 1971-98` = fN(desc_wide$`N_1971-1998`),
                      `Mean (sd) 1971-98` = paste0(f3(desc_wide$`mean_1971-1998`), " (", f3(desc_wide$`sd_1971-1998`), ")"),
                      `Median 1971-98` = f3(desc_wide$`median_1971-1998`),
                      `N 1999-25` = fN(desc_wide$`N_1999-2025`),
                      `Mean (sd) 1999-25` = paste0(f3(desc_wide$`mean_1999-2025`), " (", f3(desc_wide$`sd_1999-2025`), ")"),
                      `Median 1999-25` = f3(desc_wide$`median_1999-2025`),
                      `KS D` = f3(ks$KS_D[match(desc_wide$variable, ks$variable)]))
write_md(desc_md, "table_desc_periods",
         "Descriptive statistics of the regression variables, 1971-1998 vs 1999-2025 (regression sample)",
         "FG = Frank & Goyal (2003) Table 9 (1971-1998). KS D = two-sample Kolmogorov-Smirnov statistic; every p-value is < 0.001.")
fwrite(desc, file.path(TAB, "desc_long.csv")); fwrite(ks, file.path(TAB, "ks_tests.csv"))

## Annual series for the structural-break analysis
ann <- base[, .(
  N = .N, def_mean = mean(def_na), def_med = median(def_na), def_p10 = quantile(def_na, .1),
  def_p25 = quantile(def_na, .25), def_p75 = quantile(def_na, .75), def_p90 = quantile(def_na, .9),
  nd_mean = mean(nd_na), ne_mean = mean(ne_na), nd_med = median(nd_na), ne_med = median(ne_na),
  div_mean = mean(div_na, na.rm = TRUE), inv_mean = mean(inv_na, na.rm = TRUE),
  dwc_mean = mean(dwc_na, na.rm = TRUE), cf_mean = mean(cf_na, na.rm = TRUE),
  loss_share = mean(loss, na.rm = TRUE), rd_share = mean(rd_pos), rd_mean = mean(pmin(rd_at, 1), na.rm = TRUE),
  cash_mean = mean(cash_at, na.rm = TRUE), payer_share = mean(payer, na.rm = TRUE),
  tang_mean = mean(tang, na.rm = TRUE), neg_cf_share = mean(cf_na < 0, na.rm = TRUE),
  eq_issuer_share = mean(ne_na > 0.05), repurch_share = mean(ne_na < -0.01),
  scf7_share = mean(scf == 7L, na.rm = TRUE),
  corr_def_nd = cor(def_na, nd_na), corr_def_ne = cor(def_na, ne_na)
), by = fyear][order(fyear)]

# Year-by-year cross-sectional regressions b_t and c_t (Q4 figure, break tests)
yr <- rbindlist(lapply(sort(unique(base$fyear)), function(y) {
  x <- base[fyear == y]
  md <- feols(nd_na ~ def_na, x, vcov = "hetero"); me <- feols(ne_na ~ def_na, x, vcov = "hetero")
  data.table(fyear = y, N = nrow(x),
             b = coef(md)[2], b_se = se(md)[2], b_r2 = r2(md, "r2"),
             c = coef(me)[2], c_se = se(me)[2], c_r2 = r2(me, "r2"))
}))
ann <- merge(ann, yr[, .(fyear, b, b_se, b_r2, c, c_se, c_r2)], by = "fyear")
fwrite(ann, file.path(TAB, "annual_series.csv"))

# Bai-Perron breaks in the mean of each annual series (minimum segment 5 years)
bp_series <- c(def_mean = "Mean DEF / net assets", def_med = "Median DEF / net assets",
               ne_mean = "Mean net equity / net assets", nd_mean = "Mean net debt / net assets",
               cf_mean = "Mean internal cash flow / net assets", loss_share = "Share of loss firms",
               rd_mean = "Mean R&D / assets", b = "Annual slope b_t (debt)", c = "Annual slope c_t (equity)",
               b_r2 = "Annual R2 (debt regression)")
bp <- rbindlist(lapply(names(bp_series), function(v) {
  y <- ts(ann[[v]], start = min(ann$fyear))
  bpm <- breakpoints(y ~ 1, h = 5)
  nb <- length(na.omit(bpm$breakpoints))
  brk <- if (nb > 0) paste(ann$fyear[bpm$breakpoints], collapse = ", ") else "none"
  supf <- sctest(Fstats(y ~ 1, from = 0.15), type = "supF")
  # Chow test at 1998/1999
  chow <- sctest(y ~ 1, type = "Chow", point = which(ann$fyear == 1998))
  data.table(series = bp_series[[v]], bai_perron_breaks_BIC = brk,
             supF = unname(supf$statistic), supF_p = supf$p.value,
             chow_1999_F = unname(chow$statistic), chow_1999_p = chow$p.value,
             mean_71_98 = mean(ann[fyear <= 1998][[v]]), mean_99_25 = mean(ann[fyear >= 1999][[v]]))
}))
write_md(bp[, .(Series = series, `Mean 1971-98` = f3(mean_71_98), `Mean 1999-25` = f3(mean_99_25),
                `Chow F (break 1999)` = paste0(formatC(chow_1999_F, format = "f", digits = 1), stars(chow_1999_p)),
                `Andrews sup-F` = paste0(formatC(supF, format = "f", digits = 1), stars(supF_p)),
                `Bai-Perron break years (BIC)` = bai_perron_breaks_BIC)],
         "table_breaks", "Structural-break tests on annual cross-sectional series, 1971-2025",
         "Each series is the annual cross-sectional statistic (55 years). Chow test at a known break in 1999; Andrews (1993) sup-F with 15% trimming; Bai-Perron (2003) break dates chosen by BIC with a 5-year minimum segment. Break year = last year of a regime. *** p<0.01, ** p<0.05, * p<0.10.")

# Pooled Chow-type test for the deficit slope (firm-clustered)
base[, post98 := as.integer(fyear >= 1999)]
chow_pool <- feols(nd_na ~ def_na * post98, base, cluster = ~gvkey)
chow_pool_e <- feols(ne_na ~ def_na * post98, base, cluster = ~gvkey)
fwrite(rbind(data.table(dep = "net debt", coeftable(chow_pool), keep.rownames = "term"),
             data.table(dep = "net equity", coeftable(chow_pool_e), keep.rownames = "term"), fill = TRUE),
       file.path(TAB, "chow_pooled.csv"))

## Figures for Q1
fig_dist <- ggplot(ann, aes(fyear)) +
  geom_ribbon(aes(ymin = def_p10, ymax = def_p90), fill = pal[["def"]], alpha = .12) +
  geom_ribbon(aes(ymin = def_p25, ymax = def_p75), fill = pal[["def"]], alpha = .25) +
  geom_line(aes(y = def_med, colour = "Median"), linewidth = .8) +
  geom_line(aes(y = def_mean, colour = "Mean"), linewidth = .8, linetype = "22") +
  geom_vline(xintercept = 1998.5, colour = "#8a8984", linewidth = .4) +
  scale_colour_manual(values = c(Median = pal[["def"]], Mean = "#0b0b0b")) +
  labs(title = "Distribution of the financing deficit / net assets, 1971-2025",
       subtitle = "Bands: 25th-75th and 10th-90th percentiles. Vertical line: end of the original sample (1998).",
       x = NULL, y = "DEF / net assets") + theme_fg
ggsave(file.path(FIG, "fig_q1_def_distribution.png"), fig_dist, width = 8, height = 4.5, dpi = 200)

comp_long <- melt(ann[, .(fyear, `Cash dividends` = div_mean, Investment = inv_mean,
                          `Delta working capital` = dwc_mean, `Internal cash flow` = cf_mean, DEF = def_mean)],
                  id.vars = "fyear")
fig_comp <- ggplot(comp_long, aes(fyear, value, colour = variable)) +
  geom_hline(yintercept = 0, colour = "#8a8984", linewidth = .3) +
  geom_line(linewidth = .7) + geom_vline(xintercept = 1998.5, colour = "#8a8984", linewidth = .4) +
  scale_colour_manual(values = c("#eda100", "#2a78d6", "#1baf7a", "#eb6834", "#0b0b0b")) +
  labs(title = "Mean components of the financing deficit (scaled by net assets)", x = NULL, y = "Mean / net assets") + theme_fg
ggsave(file.path(FIG, "fig_q1_def_components.png"), fig_comp, width = 8, height = 4.5, dpi = 200)

char_long <- melt(ann[, .(fyear, `Loss firms (share)` = loss_share, `R&D reporters (share)` = rd_share,
                          `Dividend payers (share)` = payer_share, `Negative internal cash flow (share)` = neg_cf_share,
                          `Cash & ST inv. / assets (mean)` = cash_mean, `PPE / assets (mean)` = tang_mean)], id.vars = "fyear")
fig_char <- ggplot(char_long, aes(fyear, value)) +
  geom_line(colour = pal[["def"]], linewidth = .7) + geom_vline(xintercept = 1998.5, colour = "#8a8984", linewidth = .4) +
  facet_wrap(~variable, ncol = 3, scales = "free_y") +
  labs(title = "Changing composition of the sample, 1971-2025", x = NULL, y = NULL) + theme_fg
ggsave(file.path(FIG, "fig_q1_composition.png"), fig_char, width = 9, height = 5, dpi = 200)

fig_n <- ggplot(ann, aes(fyear, N)) + geom_col(fill = pal[["def"]], width = .75) +
  geom_vline(xintercept = 1998.5, colour = "#8a8984", linewidth = .4) +
  labs(title = "Firm-years in the regression sample by fiscal year", x = NULL, y = "Firms") + theme_fg
ggsave(file.path(FIG, "fig_q1_n_firms.png"), fig_n, width = 8, height = 3.5, dpi = 200)

# Figure 1 replication: annual means of DEF, net debt and net equity over net assets
f1 <- melt(ann[, .(fyear, `Financing deficit/net assets` = def_mean, `Net debt issued/net assets` = nd_mean,
                   `Net equity issued/net assets` = ne_mean)], id.vars = "fyear")
fig1 <- ggplot(f1, aes(fyear, value, colour = variable)) +
  geom_hline(yintercept = 0, colour = "#8a8984", linewidth = .3) +
  geom_line(linewidth = .8) + geom_point(size = 1.2) +
  geom_vline(xintercept = 1998.5, colour = "#8a8984", linewidth = .4) +
  scale_colour_manual(values = unname(pal[c("def", "nd", "ne")])) +
  scale_x_continuous(breaks = seq(1971, 2025, 6)) +
  labs(title = "Figure 1 (replicated and extended): deficit, net debt and net equity, 1971-2025",
       x = "Year", y = "Mean / net assets") + theme_fg
ggsave(file.path(FIG, "fig1_replicated.png"), fig1, width = 8, height = 4.5, dpi = 200)
fig1o <- fig1; fig1o$data <- f1[fyear <= 1998]; fig1o <- fig1o + scale_x_continuous(breaks = seq(1971, 1998, 3)) +
  labs(title = "Figure 1 (replicated): 1971-1998, the paper's window")
ggsave(file.path(FIG, "fig1_replicated_1971_1998.png"), fig1o, width = 8, height = 4.5, dpi = 200)

corr_tab <- rbindlist(lapply(c("orig", "ext", "full"), function(p) {
  x <- base[in_period(base, p)]
  data.table(Period = PERIOD_LABEL[[p]], `corr(DEF, net equity)` = f3(cor(x$def_na, x$ne_na)),
             `corr(DEF, net debt)` = f3(cor(x$def_na, x$nd_na)))
}))
corr_tab <- rbind(data.table(Period = "Frank & Goyal (1971-1998)", `corr(DEF, net equity)` = "0.800", `corr(DEF, net debt)` = "0.480"), corr_tab)
write_md(corr_tab, "table_correlations", "Correlation of the financing deficit with net equity and net debt issues")

# =============================================================================
# Q2. Table 3: pecking order regression
# =============================================================================
# No-gaps panel: firms with deficit and net debt in the regression sample in every year of a window
no_gap_firms <- function(p) {
  yrs <- PERIODS[[p]][1]:PERIODS[[p]][2]
  base[fyear %in% yrs, .N, by = gvkey][N == length(yrs), gvkey]
}
ng_ssm <- no_gap_firms("ssm"); ng_90s <- no_gap_firms("nineties")

t3 <- rbind(
  po_reg(base[in_period(base, "full")], label = "(a) 1971-2025"),
  po_reg(base[in_period(base, "orig")], label = "(b) 1971-1998"),
  po_reg(base[in_period(base, "ext")],  label = "(c) 1999-2025"),
  po_reg(base[in_period(base, "ssm")],  label = "1971-1989, gaps permitted"),
  po_reg(base[in_period(base, "nineties")], label = "1990-1998, gaps permitted"),
  po_reg(base[in_period(base, "ssm") & gvkey %in% ng_ssm], label = "1971-1989, no gaps"),
  po_reg(base[in_period(base, "nineties") & gvkey %in% ng_90s], label = "1990-1998, no gaps"))
fwrite(t3, file.path(TAB, "table3_raw.csv"))
fmt_po <- function(t, paper = NULL) {
  out <- data.table(
    ` ` = c("Constant", "", "Financing deficit (b)", "", "t-stat for H0: b = 1", "N (firm-years)", "Firms", "R2"))
  for (i in seq_len(nrow(t))) {
    r <- t[i]
    out[[r$label]] <- c(paste0(f3(r$a), stars(r$a_p)), paste0("(", f3(r$a_se), ")"),
                        paste0(f3(r$b), stars(r$b_p)), paste0("(", f3(r$b_se), ")"),
                        formatC(r$t_b1, format = "f", digits = 1), fN(r$N), fN(r$firms), f3(r$r2))
  }
  out
}
write_md(fmt_po(t3[1:3]), "table3_main",
         "Table 3 (replicated and extended): Pooled OLS of net debt issued on the financing deficit",
         "Dependent variable: net debt issued / net assets. Pooled OLS; standard errors clustered by firm in parentheses. *** p<0.01, ** p<0.05, * p<0.10.")
cmp <- data.table(
  Specification = c("1971-1989, gaps permitted", "1990-1998, gaps permitted", "1971-1989, no gaps", "1990-1998, no gaps"),
  `FG table / column` = c("Table 3, col. 4", "Table 4, col. 4", "Table 3, col. 1", "Table 4, col. 1"),
  `FG b (se)` = c("0.283 (0.002)", "0.148 (0.002)", "0.748 (0.004)", "0.325 (0.004)"),
  `FG R2` = c("0.265", "0.120", "0.708", "0.283"),
  `FG N` = c("89,883", "57,687", "14,592", "18,225"))
cmp[, `Replication b (clustered se)` := paste0(f3(t3$b[4:7]), " (", f3(t3$b_se[4:7]), ")")]
cmp[, `Replication R2` := f3(t3$r2[4:7])]
cmp[, `Replication N` := fN(t3$N[4:7])]
cmp[, `Replication firms` := fN(t3$firms[4:7])]
write_md(cmp, "table3_compare", "Direct comparison with Frank & Goyal's own sub-periods",
         "F&G report conventional OLS standard errors; replication standard errors are clustered by firm.")

# Full Table 3 / Table 4 layout (net debt, gross debt, change in debt ratio; holdout R2)
fg_layout <- function(est, hold, ng_firms, ng_hold_firms) {
  specs <- list(c("nd_na", "Net debt issued"), c("gd_na", "Gross debt issued"), c("dltd_ratio", "Change in debt ratio"))
  res <- list()
  for (gap in c("no gaps", "gaps permitted")) for (s in specs) {
    x <- base[in_period(base, est)]
    if (gap == "no gaps") x <- x[gvkey %in% ng_firms]
    x <- x[!is.na(get(s[1]))]
    m <- feols(as.formula(paste(s[1], "~ def_na")), x, cluster = ~gvkey)
    h_full <- base[fyear >= hold[1] & fyear <= hold[2]]
    h_rest <- h_full[gvkey %in% ng_firms]
    ct <- coeftable(m)
    res[[length(res) + 1]] <- data.table(sample = gap, dep = s[2], a = ct[1, 1], a_se = ct[1, 2], b = ct[2, 1], b_se = ct[2, 2],
                                         b_p = ct[2, 4], N = nobs(m), r2 = r2(m, "r2"),
                                         r2_hold_full = holdout_r2(m, h_full, s[1]),
                                         r2_hold_rest = if (gap == "no gaps") holdout_r2(m, h_rest, s[1]) else NA_real_)
  }
  rbindlist(res)
}
t3_full <- fg_layout("ssm", c(1990L, 1994L), ng_ssm)
t4_full <- fg_layout("nineties", c(1985L, 1989L), ng_90s)
lay_md <- function(t, paper) {
  out <- data.table(` ` = c("Constant", "", "Financing deficit", "", "N", "R2", "R2 holdout (full)", "R2 holdout (restricted)"))
  for (i in seq_len(nrow(t))) {
    r <- t[i]; pp <- paper[[i]]
    out[[paste0("(", i, ") ", r$dep, ", ", r$sample)]] <- c(
      paste0(f3(r$a), " [", pp[1], "]"), paste0("(", f3(r$a_se), ")"),
      paste0(f3(r$b), stars(r$b_p), " [", pp[2], "]"), paste0("(", f3(r$b_se), ")"),
      paste0(fN(r$N), " [", pp[3], "]"), paste0(f3(r$r2), " [", pp[4], "]"),
      paste0(f3(r$r2_hold_full), " [", pp[5], "]"), ifelse(is.na(r$r2_hold_rest), "", paste0(f3(r$r2_hold_rest), " [", pp[6], "]")))
  }
  out
}
write_md(lay_md(t3_full, PAPER_T3), "table3_fg_layout",
         "Frank & Goyal Table 3 layout, 1971-1989: replication [paper in brackets]",
         "Holdout sample 1990-1994. Restricted holdout = no-gap firms only. Clustered standard errors in parentheses.")
write_md(lay_md(t4_full, PAPER_T4), "table4_fg_layout",
         "Frank & Goyal Table 4 layout, 1990-1998: replication [paper in brackets]",
         "Holdout sample 1985-1989.")

# =============================================================================
# Q3. Table 5 (disaggregated deficit) and size heterogeneity
# =============================================================================
comp_reg <- function(x, label, extra = NULL) {
  rhs <- c("div_na", "inv_na", "dwc_na", "cf_na", extra)
  x <- x[complete.cases(x[, c("nd_na", rhs), with = FALSE])]
  m <- feols(as.formula(paste("nd_na ~", paste(rhs, collapse = " + "))), x, cluster = ~gvkey)
  k <- length(coef(m))
  # H0 (pecking order): div = inv = dwc = 1 and cf = -1
  R1 <- cbind(0, diag(4), matrix(0, 4, k - 5)); r1 <- c(1, 1, 1, -1)
  # H0 (aggregation only): div = inv = dwc = -cf
  R2 <- rbind(c(0, 1, -1, 0, 0), c(0, 0, 1, -1, 0), c(0, 0, 0, 1, 1)); R2 <- cbind(R2, matrix(0, 3, k - 5))
  list(m = m, label = label, w1 = wald(m, R1, r1), w2 = wald(m, R2, c(0, 0, 0)), firms = uniqueN(x$gvkey))
}
t5 <- list(
  comp_reg(base[in_period(base, "full")], "(a) 1971-2025"),
  comp_reg(base[in_period(base, "orig")], "(b) 1971-1998"),
  comp_reg(base[in_period(base, "ext")],  "(c) 1999-2025"),
  comp_reg(base[in_period(base, "ssm") & gvkey %in% ng_ssm], "1971-1989 no gaps [FG col. 1]"),
  comp_reg(base[in_period(base, "ssm")], "1971-1989 gaps [FG col. 4]"),
  comp_reg(base[in_period(base, "ssm")], "1971-1989 + current maturities [FG col. 7]", extra = "cmltd_na"))
t5_rows <- c("(Intercept)", "div_na", "inv_na", "dwc_na", "cf_na", "cmltd_na")
t5_lab  <- c("Constant", "Cash dividends", "Investments", "Delta working capital", "Internal cash flow", "Current maturity of LTD")
t5_md <- data.table(` ` = c(as.vector(rbind(t5_lab, "")), "N", "Firms", "R2",
                            "Wald: all = PO values (chi2)", "Wald: equal magnitudes (chi2)"))
t5_raw <- list()
for (r in t5) {
  ct <- coeftable(r$m)
  col <- unlist(lapply(t5_rows, function(v) if (v %in% rownames(ct))
    c(paste0(f3(ct[v, 1]), stars(ct[v, 4])), paste0("(", f3(ct[v, 2]), ")")) else c("", "")))
  t5_md[[r$label]] <- c(col, fN(nobs(r$m)), fN(r$firms), f3(r2(r$m, "r2")),
                        paste0(formatC(r$w1["chi2"], format = "f", digits = 0), stars(r$w1["p"])),
                        paste0(formatC(r$w2["chi2"], format = "f", digits = 0), stars(r$w2["p"])))
  t5_raw[[r$label]] <- data.table(spec = r$label, term = rownames(ct), ct, N = nobs(r$m), r2 = r2(r$m, "r2"),
                                  wald_po = r$w1["chi2"], wald_po_p = r$w1["p"], wald_agg = r$w2["chi2"], wald_agg_p = r$w2["p"])
}
fwrite(rbindlist(t5_raw), file.path(TAB, "table5_raw.csv"))
paper5 <- data.table(` ` = t5_md[[1]],
  `FG col. 1 (no gaps)` = c("-0.006", "(0.001)", "0.884", "(0.020)", "0.774", "(0.005)", "0.723", "(0.005)", "-0.739", "(0.005)", "", "", "14,565", "", "0.705", "", ""),
  `FG col. 4 (gaps)`    = c("-0.030", "(0.001)", "0.372", "(0.018)", "0.393", "(0.002)", "0.246", "(0.002)", "-0.190", "(0.002)", "", "", "88,892", "", "0.289", "", ""),
  `FG col. 7 (+CMLTD)`  = c("-0.020", "(0.001)", "0.339", "(0.018)", "0.431", "(0.002)", "0.296", "(0.002)", "-0.245", "(0.002)", "-0.167", "(0.005)", "81,588", "", "0.339", "", ""))
write_md(t5_md, "table5_main",
         "Table 5 (replicated and extended): Disaggregating the financing deficit",
         "Dependent variable: net debt issued / net assets. Regressors scaled by net assets; internal cash flow enters with its own sign, so the pecking order predicts +1, +1, +1 and -1. Wald tests use the clustered covariance: 'PO values' tests all four = (1, 1, 1, -1); 'equal magnitudes' tests the aggregation restriction div = inv = dWC = -CF. Firm-clustered SEs. *** p<0.01.")
write_md(paper5, "table5_paper", "Frank & Goyal (2003) Table 5, net debt columns (1971-1989), for comparison")

## Size: annual market-cap terciles -----------------------------------------
size_lab <- c(`1` = "Small (bottom tercile)", `2` = "Medium", `3` = "Large (top tercile)")
sz <- rbindlist(lapply(c("orig", "ext", "full"), function(p) rbindlist(lapply(1:3, function(g) {
  r <- po_reg(base[in_period(base, p) & size_terc == g], label = size_lab[[as.character(g)]])
  r[, period := PERIOD_LABEL[[p]]]
}))))
# Large - small difference in b (interaction, clustered) and change across periods
sz_diff <- rbindlist(lapply(c("orig", "ext", "full"), function(p) {
  x <- base[in_period(base, p) & size_terc %in% c(1, 3)][, large := as.integer(size_terc == 3)]
  m <- feols(nd_na ~ def_na * large, x, cluster = ~gvkey)
  ct <- coeftable(m)
  data.table(period = PERIOD_LABEL[[p]], diff = ct["def_na:large", 1], diff_se = ct["def_na:large", 2], diff_p = ct["def_na:large", 4])
}))
x <- base[size_terc %in% c(1, 3)][, `:=`(large = as.integer(size_terc == 3), post = as.integer(fyear >= 1999))]
m3 <- feols(nd_na ~ def_na * large * post, x, cluster = ~gvkey)
ddd <- coeftable(m3)
fwrite(sz, file.path(TAB, "size_terciles_raw.csv")); fwrite(sz_diff, file.path(TAB, "size_diff.csv"))
fwrite(data.table(term = rownames(ddd), ddd), file.path(TAB, "size_triple_interaction.csv"))

sz_md <- data.table(` ` = c("Financing deficit (b)", "", "R2", "N", "Firms"))
for (p in c("1971-1998", "1999-2025", "1971-2025")) for (g in size_lab) {
  r <- sz[period == p & label == g]
  sz_md[[paste0(p, ": ", sub(" \\(.*", "", g))]] <- c(paste0(f3(r$b), stars(r$b_p)), paste0("(", f3(r$b_se), ")"), f3(r$r2), fN(r$N), fN(r$firms))
}
diff_row <- c("Large - small (b)", rep("", ncol(sz_md) - 1))
for (j in 1:3) diff_row[1 + (j - 1) * 3 + 3] <- paste0(f3(sz_diff$diff[j]), stars(sz_diff$diff_p[j]), " (", f3(sz_diff$diff_se[j]), ")")
sz_md <- rbind(sz_md, as.list(diff_row))
write_md(sz_md, "table_size_terciles",
         "Pecking order regression by market-capitalisation tercile",
         paste0("Terciles formed each fiscal year on CRSP market capitalisation at the fiscal year end (Compustat price x shares if CRSP is missing). ",
                "Large - small: interaction coefficient def x large, firm-clustered SE. Triple interaction (change of the large-small gap after 1998): ",
                f3(ddd["def_na:large:post", 1]), " (se ", f3(ddd["def_na:large:post", 2]), ", p = ", formatC(ddd["def_na:large:post", 4], format = "f", digits = 3), ")."))

## F&G Table 6 replication: total-asset quartiles
q_lab <- c("Smallest", "Medium small", "Medium large", "Largest")
t6 <- rbindlist(lapply(c("ssm", "nineties", "ext"), function(p) rbindlist(lapply(1:4, function(g) {
  r <- po_reg(base[in_period(base, p) & size_quart == g], label = q_lab[g]); r[, period := PERIOD_LABEL[[p]]]
}))))
t6_md <- t6[, .(Period = period, Quartile = label, `b (se)` = paste0(f3(b), stars(b_p), " (", f3(b_se), ")"),
                R2 = f3(r2), N = fN(N))]
t6_md[, `FG b` := c(PAPER_T6$b, rep("", 4))]
t6_md[, `FG R2` := c(PAPER_T6$r2, rep("", 4))]
t6_md[, `FG N` := c(PAPER_T6$N, rep("", 4))]
write_md(t6_md, "table6_quartiles",
         "Frank & Goyal Table 6 (cols. 4-11) replicated: quartiles of total assets, plus 1999-2025",
         "Quartiles formed each fiscal year on book assets. Firm-clustered SEs.")

# Annual b_t by size group
yr_sz <- rbindlist(lapply(sort(unique(base$fyear)), function(y) rbindlist(lapply(c(1, 3), function(g) {
  x <- base[fyear == y & size_terc == g]
  m <- feols(nd_na ~ def_na, x, vcov = "hetero")
  data.table(fyear = y, group = size_lab[[as.character(g)]], b = coef(m)[2], se = se(m)[2])
}))))
fwrite(yr_sz, file.path(TAB, "annual_b_by_size.csv"))
fig_sz <- ggplot(yr_sz, aes(fyear, b, colour = group, fill = group)) +
  geom_ribbon(aes(ymin = b - 1.96 * se, ymax = b + 1.96 * se), alpha = .15, colour = NA) +
  geom_line(linewidth = .8) + geom_vline(xintercept = 1998.5, colour = "#8a8984", linewidth = .4) +
  scale_colour_manual(values = c("#2a78d6", "#eb6834")) + scale_fill_manual(values = c("#2a78d6", "#eb6834")) +
  labs(title = "Annual pecking order slope b_t, large vs small firms (market-cap terciles)",
       subtitle = "Year-by-year cross-sectional OLS; bands are 95% intervals (heteroskedasticity-robust).",
       x = NULL, y = "b_t") + theme_fg
ggsave(file.path(FIG, "fig_q3_b_by_size.png"), fig_sz, width = 8, height = 4.5, dpi = 200)

# =============================================================================
# Q4. Debt vs equity horse race
# =============================================================================
hr <- rbindlist(lapply(c("full", "orig", "ext"), function(p) {
  x <- base[in_period(base, p)]
  rd <- po_reg(x, "nd_na", label = "debt"); re <- po_reg(x, "ne_na", label = "equity")
  # Stacked system: same firm-years twice, firm-clustered, test c - b
  st <- rbind(x[, .(gvkey, y = nd_na, def = def_na, eq = 0L)], x[, .(gvkey, y = ne_na, def = def_na, eq = 1L)])
  ms <- feols(y ~ def * eq, st, cluster = ~gvkey)
  ct <- coeftable(ms)
  dif <- ct["def:eq", 1]; dse <- ct["def:eq", 2]
  data.table(period = PERIOD_LABEL[[p]],
             b = rd$b, b_se = rd$b_se, b_r2 = rd$r2, a_d = rd$a, a_d_se = rd$a_se,
             c = re$b, c_se = re$b_se, c_r2 = re$r2, a_e = re$a, a_e_se = re$a_se, N = rd$N, firms = rd$firms,
             diff = dif, diff_se = dse, t_diff = dif / dse, p_one_sided = pnorm(dif / dse, lower.tail = FALSE),
             b_plus_c = rd$b + re$b, share_identity_exact = mean(abs(x$nd_na + x$ne_na - x$def_na) < 1e-6))
}))
fwrite(hr, file.path(TAB, "horse_race_raw.csv"))
hr_md <- data.table(` ` = c("Constant", "", "Financing deficit", "", "R2", "N", "Firms"))
for (i in seq_len(nrow(hr))) {
  r <- hr[i]
  hr_md[[paste0(r$period, ": debt (b)")]] <- c(f3(r$a_d), paste0("(", f3(r$a_d_se), ")"), paste0(f3(r$b), "***"), paste0("(", f3(r$b_se), ")"), f3(r$b_r2), fN(r$N), fN(r$firms))
  hr_md[[paste0(r$period, ": equity (c)")]] <- c(f3(r$a_e), paste0("(", f3(r$a_e_se), ")"), paste0(f3(r$c), "***"), paste0("(", f3(r$c_se), ")"), f3(r$c_r2), fN(r$N), fN(r$firms))
}
write_md(hr_md, "table_horse_race",
         "Debt vs equity: net debt and net equity issues regressed on the financing deficit",
         "Same firm-years in the debt and equity columns. Pooled OLS, firm-clustered SEs. *** p<0.01.")
hr_test <- hr[, .(Period = period, `c - b` = f3(diff), `Clustered se` = f3(diff_se), t = formatC(t_diff, format = "f", digits = 1),
                  `One-sided p (H1: c > b)` = formatC(p_one_sided, format = "e", digits = 1),
                  `b + c` = f3(b_plus_c), `Share of obs with dD + dE = DEF exactly` = f3(share_identity_exact))]
write_md(hr_test, "table_horse_race_test", "Formal test that the equity-deficit sensitivity exceeds the debt-deficit sensitivity",
         "Stacked regression of [dD; dE] on DEF, an equity dummy and their interaction, clustered by firm; c - b is the interaction coefficient.")

# Year-by-year b_t and c_t
yr_long <- rbind(yr[, .(fyear, coef = b, se = b_se, series = "Debt: b_t")], yr[, .(fyear, coef = c, se = c_se, series = "Equity: c_t")])
fig_bc <- ggplot(yr_long, aes(fyear, coef, colour = series, fill = series)) +
  geom_hline(yintercept = c(0, 1), colour = "#8a8984", linewidth = .3) +
  geom_hline(yintercept = .5, colour = "#8a8984", linewidth = .3, linetype = "22") +
  geom_ribbon(aes(ymin = coef - 1.96 * se, ymax = coef + 1.96 * se), alpha = .15, colour = NA) +
  geom_line(linewidth = .8) + geom_point(size = 1.2) +
  geom_vline(xintercept = 1998.5, colour = "#8a8984", linewidth = .4) +
  scale_colour_manual(values = unname(pal[c("nd", "ne")])) + scale_fill_manual(values = unname(pal[c("nd", "ne")])) +
  scale_x_continuous(breaks = seq(1971, 2025, 6)) +
  labs(title = "Year-by-year sensitivity of net debt (b_t) and net equity (c_t) to the deficit",
       subtitle = "Annual cross-sectional OLS; 95% bands. Above the dashed 0.5 line, equity absorbs more of the deficit than debt.",
       x = NULL, y = "Slope on DEF / net assets") + theme_fg
ggsave(file.path(FIG, "fig_q4_bt_ct.png"), fig_bc, width = 8, height = 4.8, dpi = 200)

# Binned scatter: deficit vs debt and equity, by period
bins <- base[in_period(base, "full")][, period := fifelse(fyear <= 1998, "1971-1998", "1999-2025")]
bins[, bin := cut(frank(def_na) / .N, 40, labels = FALSE), by = period]
bs <- bins[, .(def = mean(def_na), `Net debt` = mean(nd_na), `Net equity` = mean(ne_na)), by = .(period, bin)]
bs <- melt(bs, id.vars = c("period", "bin", "def"))
fig_bs <- ggplot(bs, aes(def, value, colour = variable)) +
  geom_abline(slope = 1, intercept = 0, colour = "#8a8984", linewidth = .3, linetype = "22") +
  geom_point(size = 1.6) + geom_smooth(method = "lm", se = FALSE, linewidth = .7, formula = y ~ x) +
  facet_wrap(~period) + scale_colour_manual(values = unname(pal[c("nd", "ne")])) +
  labs(title = "Binned scatter: net debt and net equity against the deficit (40 quantile bins)",
       subtitle = "Dashed line: 45 degrees (dollar-for-dollar). Lines: OLS through the bin means.",
       x = "Financing deficit / net assets", y = "Issue / net assets") + theme_fg
ggsave(file.path(FIG, "fig_q4_binned_scatter.png"), fig_bs, width = 8.5, height = 4.5, dpi = 200)

saveRDS(list(t3 = t3, hr = hr, sz = sz, sz_diff = sz_diff, ddd = ddd, ann = ann, bp = bp, ks = ks,
             t3_full = t3_full, t4_full = t4_full, t6 = t6, chow = coeftable(chow_pool),
             n_noGap_ssm = length(ng_ssm), n_noGap_90s = length(ng_90s)),
        file.path(OUT_DIR, "results.rds"))
cat("Done: tables in", TAB, "and figures in", FIG, "\n")
