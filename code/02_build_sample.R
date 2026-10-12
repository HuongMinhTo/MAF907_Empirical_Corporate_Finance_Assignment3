# Build the firm-year panel: sample screens, F&G variable definitions,
# missing-value recoding and trimming.
#
# Writes data/processed/panel.rds (analysis panel, one row per gvkey-fyear) and
# output/sample_construction.csv (observation counts after each screen).

source("code/00_config.R")
load_packages(c("data.table"))

funda   <- readRDS(file.path(RAW_DIR, "raw_funda.rds"))
company <- readRDS(file.path(RAW_DIR, "raw_company.rds"))
ccm     <- readRDS(file.path(RAW_DIR, "raw_ccm.rds"))
me      <- readRDS(file.path(RAW_DIR, "raw_crsp_me.rds"))

steps <- list()
log_step <- function(dt, label) {
  in_win <- dt[fyear >= FIRST_YEAR & fyear <= LAST_YEAR]
  steps[[length(steps) + 1L]] <<- data.table(
    step = label, firm_years = nrow(in_win), firms = uniqueN(in_win$gvkey),
    fy_1971_1998 = nrow(in_win[fyear <= 1998]), fy_1999_2025 = nrow(in_win[fyear >= 1999]))
  cat(sprintf("%-60s %9d\n", label, nrow(in_win)))
}

d <- copy(funda)
d[, datadate := as.Date(datadate)]
num_cols <- setdiff(names(d), c("gvkey", "datadate", "fyear", "scf", "compst", "sich", "fic", "curcd"))
d[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]
d[, `:=`(fyear = as.integer(fyear), scf = as.integer(scf), sich = as.integer(sich))]
log_step(d, "Compustat annual (INDL/STD/D/C)")

# One record per firm and fiscal year (a fiscal-year-end change can create two)
setorder(d, gvkey, fyear, -datadate)
d <- unique(d, by = c("gvkey", "fyear"))
log_step(d, "Unique gvkey-fyear")

# SIC: historical SIC, header SIC when missing
company[, sic := as.integer(sic)]
d <- merge(d, company[, .(gvkey, conm, sic_hdr = sic)], by = "gvkey", all.x = TRUE)
d[, sic := fifelse(is.na(sich), sic_hdr, sich)]

# CRSP/Compustat Merged: keep firm-years with a valid link at the fiscal year end
ccm[, `:=`(linkdt = as.Date(linkdt),
           linkenddt = as.Date(fifelse(is.na(linkenddt), as.Date("2099-12-31"), as.Date(linkenddt))),
           permno = as.integer(permno))]
lk <- ccm[d[, .(gvkey, fyear, datadate)], on = .(gvkey, linkdt <= datadate, linkenddt >= datadate),
          nomatch = NULL, .(gvkey, fyear, permno, linkprim)]
setorder(lk, gvkey, fyear, -linkprim)  # "P" before "C"
lk <- unique(lk, by = c("gvkey", "fyear"))
d <- merge(d, lk[, .(gvkey, fyear, permno)], by = c("gvkey", "fyear"), all.x = TRUE)
d[, in_ccm := !is.na(permno)]

# Market cap at fiscal year end: CRSP (CIZ, $000) for the linked permno,
# Compustat prcc_f * csho when CRSP is missing. In $ millions.
me[, `:=`(permno = as.integer(permno), yyyymm = as.integer(yyyymm), mthcap = as.numeric(mthcap))]
d[, yyyymm := as.integer(format(datadate, "%Y%m"))]
d <- merge(d, me, by = c("permno", "yyyymm"), all.x = TRUE)
d[, mcap := fifelse(!is.na(mthcap), mthcap / 1000, prcc_f * csho)]

# Keep the full Compustat universe in a separate file for the CCM robustness check
d_all <- copy(d)

# Screens (F&G p. 225), applied in the paper's order, plus the CCM requirement
# of the assignment.
d <- d[in_ccm == TRUE];                                  log_step(d, "Linked in CRSP/Compustat Merged (LC/LU, P/C)")
d <- d[!(sic >= 6000 & sic <= 6999) | is.na(sic)];       log_step(d, "Drop financial firms (SIC 6000-6999)")
d <- d[!(sic >= 4900 & sic <= 4999) | is.na(sic)];       log_step(d, "Drop regulated utilities (SIC 4900-4999)")
d <- d[is.na(compst) | compst != "AB"];                  log_step(d, "Drop major mergers (comparability status AB)")
d <- d[!is.na(at) & at > 0];                             log_step(d, "Drop missing (or zero) book assets")
d <- d[is.na(scf) | !scf %in% 4:6];                      log_step(d, "Drop format codes 4, 5, 6")

# ---- Variable construction -------------------------------------------------
build_vars <- function(x) {
  x <- copy(x)
  # Table 1 cash rule: combined cash and ST investments go to cash
  x[is.na(ch) & !is.na(che), `:=`(ch = che, ivst = 0)]
  x[is.na(ivst), ivst := fifelse(!is.na(che) & !is.na(ch), pmax(che - ch, 0), 0)]
  for (v in c(RECODE_BS, RECODE_CF)) set(x, which(is.na(x[[v]])), v, 0)

  f13 <- x$scf %in% 1:3
  f1  <- x$scf %in% 1L
  f23 <- x$scf %in% 2:3
  f7  <- x$scf %in% 7L

  # Investments (Table 2 note b)
  x[, inv := NA_real_]
  x[f13, inv := capx + ivch + aqc + fuseo - sppe - siv]
  x[f7,  inv := capx + ivch + aqc - sppe - siv - ivstch - ivaco]
  # Change in net working capital (Table 2 note c)
  x[, dwc := NA_real_]
  x[f1,  dwc := wcapc + chech + dlcch]
  x[f23, dwc := -wcapc + chech - dlcch]
  x[f7,  dwc := -recch - invch - apalch - txach - aoloch + chech - fiao - dlcch]
  # Internal cash flow after interest and taxes (Table 2 note d)
  x[, cf := NA_real_]
  x[f13, cf := ibc + xidoc + dpc + txdc + esubc + sppiv + fopo + fsrco]
  x[f7,  cf := ibc + xidoc + dpc + txdc + esubc + sppiv + fopo + exre]
  x[, div := fifelse(f13 | f7, dv, NA_real_)]
  # Financing deficit, eq. (1)
  x[, def := div + inv + dwc - cf]
  # Net debt, gross debt, net equity issues
  x[, nd := fifelse(f13 | f7, dltis - dltr, NA_real_)]
  x[, gd := fifelse(f13 | f7, dltis, NA_real_)]
  x[, ne := fifelse(f13 | f7, sstk - prstkc, NA_real_)]
  x[, identity_gap := nd + ne - def]

  # Scaling: net assets = total assets - current liabilities (p. 222)
  x[, net_assets := at - lct]
  ok_na <- !is.na(x$net_assets) & x$net_assets > 0
  for (v in c("def", "div", "inv", "dwc", "cf", "nd", "gd", "ne")) {
    set(x, j = paste0(v, "_na"), value = fifelse(ok_na, x[[v]] / x$net_assets, NA_real_))
  }
  # Change in long-term debt / net assets ratio and beginning-of-year current maturities
  setorder(x, gvkey, fyear)
  x[, ltd_na := fifelse(ok_na, dltt / net_assets, NA_real_)]
  x[, `:=`(lag_fyear = shift(fyear), lag_ltd_na = shift(ltd_na), lag_dd1 = shift(dd1)), by = gvkey]
  x[, dltd_ratio := fifelse(lag_fyear == fyear - 1L, ltd_na - lag_ltd_na, NA_real_)]
  x[, cmltd_na := fifelse(lag_fyear == fyear - 1L & ok_na, lag_dd1 / net_assets, NA_real_)]

  # Flows over total assets (Table 2 is scaled by total assets)
  for (v in c("def", "div", "inv", "dwc", "cf", "nd", "ne")) {
    set(x, j = paste0(v, "_at"), value = x[[v]] / x$at)
  }
  # Balance sheet over total assets (Table 1)
  bs <- c("ch", "ivst", "rect", "invt", "aco", "act", "ppent", "ivaeq", "ivao", "intan", "ao",
          "dlc", "ap", "txp", "lco", "lct", "dltt", "lo", "txditc", "mib", "lt", "pstk", "ceq", "seq")
  for (v in bs) set(x, j = paste0("bs_", v), value = x[[v]] / x$at)

  # Other characteristics (structural-break diagnostics, Table 9 style)
  x[, `:=`(
    rd_at       = fifelse(is.na(xrd), 0, xrd) / at,
    rd_pos      = !is.na(xrd) & xrd > 0,
    loss        = !is.na(ib) & ib < 0,
    cash_at     = che / at,
    tang        = ppent / at,
    prof        = oibdp / at,
    mtb         = (at - ceq + mcap) / at,
    book_lev    = (dlc + dltt) / at,
    log_at      = log(at),
    payer       = dv > 0
  )]
  x
}

d <- build_vars(d)
d <- d[fyear >= FIRST_YEAR & fyear <= LAST_YEAR]

# ---- Trimming (0.5% in each tail, within 1971-1998 and within 1999-2025) ----
trim_vars <- c(paste0(c("def", "div", "inv", "dwc", "cf", "nd", "gd", "ne"), "_na"),
               "dltd_ratio", "cmltd_na",
               paste0(c("def", "div", "inv", "dwc", "cf", "nd", "ne"), "_at"),
               grep("^bs_", names(d), value = TRUE))
trim_panel <- function(x, vars, blocks = TRIM_BLOCKS, mode = c("trim", "winsor", "none")) {
  mode <- match.arg(mode)
  x <- copy(x)
  if (mode == "none") return(x)
  for (b in blocks) {
    rows <- which(x$fyear >= b[1] & x$fyear <= b[2])
    for (v in vars) {
      z <- x[[v]][rows]
      q <- quantile(z, c(TRIM_P, 1 - TRIM_P), na.rm = TRUE, names = FALSE)
      if (mode == "trim") z[!is.na(z) & (z < q[1] | z > q[2])] <- NA
      else z <- pmin(pmax(z, q[1]), q[2])
      set(x, rows, v, z)
    }
  }
  x
}
raw_untrimmed <- copy(d)
d <- trim_panel(d, trim_vars)

# Baseline regression sample: deficit, net debt and net equity all present
# after trimming (the same firm-years for the debt and the equity regression)
d[, in_reg := !is.na(def_na) & !is.na(nd_na) & !is.na(ne_na)]
d[, in_reg_comp := in_reg & !is.na(div_na) & !is.na(inv_na) & !is.na(dwc_na) & !is.na(cf_na)]
log_step(d[!is.na(net_assets) & net_assets > 0], "Positive net assets")
log_step(d[in_reg == TRUE], "Regression sample: DEF, net debt, net equity non-missing after trim")

# Size groups: annual market-cap terciles within the regression sample (Q3),
# and annual total-asset quartiles (F&G Table 6)
d[in_reg == TRUE & !is.na(mcap) & mcap > 0,
  size_terc := as.integer(cut(frank(mcap, ties.method = "first") / .N, c(0, 1/3, 2/3, 1), labels = FALSE)),
  by = fyear]
d[in_reg == TRUE,
  size_quart := as.integer(cut(frank(at, ties.method = "first") / .N, c(0, .25, .5, .75, 1), labels = FALSE)),
  by = fyear]

# First year a firm appears on Compustat (IPO-year robustness check)
first_year <- funda[, .(first_fyear = min(as.integer(fyear))), by = gvkey]
d <- merge(d, first_year, by = "gvkey", all.x = TRUE)
raw_untrimmed <- merge(raw_untrimmed, first_year, by = "gvkey", all.x = TRUE)

saveRDS(d, file.path(PROC_DIR, "panel.rds"), compress = "xz")
saveRDS(raw_untrimmed, file.path(PROC_DIR, "panel_untrimmed.rds"), compress = "xz")

# Full-Compustat version (no CCM requirement) for the robustness check
a <- d_all[!(sic >= 6000 & sic <= 6999 | sic >= 4900 & sic <= 4999) | is.na(sic)]
a <- a[(is.na(compst) | compst != "AB") & !is.na(at) & at > 0 & (is.na(scf) | !scf %in% 4:6)]
a <- trim_panel(build_vars(a)[fyear >= FIRST_YEAR & fyear <= LAST_YEAR], trim_vars)
a[, in_reg := !is.na(def_na) & !is.na(nd_na) & !is.na(ne_na)]
saveRDS(a[, .(gvkey, fyear, in_ccm, in_reg, def_na, nd_na, ne_na)], file.path(PROC_DIR, "panel_allcompustat.rds"), compress = "xz")

steps <- rbindlist(steps)
fwrite(steps, file.path(OUT_DIR, "sample_construction.csv"))
print(steps)

# Missing-value audit (before recoding) for the key items, by period
key_items <- c("at", "lct", "ibc", "sstk", "dltis", "dltr", "prstkc", "dv", "capx", "chech", "scf")
audit <- funda[as.integer(fyear) >= FIRST_YEAR,
               c(list(n = .N), lapply(.SD, function(z) mean(is.na(z)))),
               by = .(period = fifelse(as.integer(fyear) <= 1998, "1971-1998", "1999-2025")),
               .SDcols = key_items]
fwrite(audit, file.path(OUT_DIR, "missing_audit.csv"))
print(audit)
