# Tables in the layout of the original paper, plus data-quality tables
# (missing values, trimming).
# Reads the outputs of 02-04 and writes Markdown tables to output/tables_md/.

source("code/00_config.R")
load_packages(c("data.table"))
source("code/paper_values.R")

TAB <- file.path(OUT_DIR, "tables"); MD <- file.path(OUT_DIR, "tables_md")
f3 <- function(x) ifelse(is.na(x), "", formatC(x, format = "f", digits = 3))
fN <- function(x) ifelse(is.na(x), "", formatC(round(x), format = "d", big.mark = ","))
pct <- function(x) formatC(100 * x, format = "f", digits = 1)
stars <- function(p) ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .10, "*", "")))

# Markdown pipe table: column widths proportional to content (set by the
# dashes of the separator line), true minus signs, caption and note.
write_md <- function(df, name, caption, note = NULL, left = 1) {
  df <- as.data.frame(df, check.names = FALSE)
  df[] <- lapply(df, function(z) { z <- as.character(z); z[is.na(z)] <- ""
    gsub("(^|[ (])-(?=[0-9.])", "\\1\u2212", z, perl = TRUE) })
  w <- pmax(nchar(names(df)), sapply(df, function(z) max(nchar(z), 1)))
  w <- pmax(3, round(60 * w / sum(w)))
  al <- ifelse(seq_along(df) %in% left, ":", "")
  sep <- paste0("|", paste0(al, strrep("-", w), ifelse(seq_along(df) %in% left, "", ":"), collapse = "|"), "|")
  hdr <- paste0("| ", paste(names(df), collapse = " | "), " |")
  body <- apply(df, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
  writeLines(c(hdr, sep, body, "", paste0("Table: ", caption),
               if (!is.null(note)) c("", paste("Note:", note))),
             file.path(MD, paste0(name, ".md")))
  fwrite(df, file.path(TAB, paste0(name, ".csv")))
}

# Convert a table written by 03/04/05 (bold caption line, pipe table, italic note)
convert_md <- function(name, new = paste0("fg_", name), caption = NULL, left = 1) {
  l <- readLines(file.path(MD, paste0(name, ".md")), warn = FALSE)
  cap <- sub("^\\*\\*(.*)\\*\\*$", "\\1", l[grepl("^\\*\\*.*\\*\\*$", l)][1])
  nt <- l[grepl("^\\*[^*].*\\*$", l)]; nt <- if (length(nt)) sub("^\\*(.*)\\*$", "\\1", nt[length(nt)]) else NULL
  tb <- l[grepl("^\\|", l)]
  cells <- lapply(tb[-2], function(r) trimws(strsplit(sub("^\\|(.*)\\|$", "\\1", r), "|", fixed = TRUE)[[1]]))
  df <- as.data.frame(do.call(rbind, cells[-1]), stringsAsFactors = FALSE)
  names(df) <- cells[[1]]
  if (names(df)[1] == "") names(df)[1] <- " "
  write_md(df, new, if (is.null(caption)) cap else caption, nt, left)
}

# ---- Tables 1 and 2: replication and F&G in adjacent columns ----------------
split_pair <- function(file, years) {
  x <- fread(file, colClasses = "character", header = TRUE)
  out <- data.table(Item = x[[1]])
  for (y in years) {
    v <- x[[y]]
    out[[y]] <- sub(" \\(.*$", "", v)
    out[[paste0("F&G ", y)]] <- sub("^.*\\((.*)\\)$", "\\1", v)
  }
  setnames(out, paste0("F&G ", years), rep("F&G", length(years)))
  out
}
yrs <- as.character(T12_YEARS_ORIG)
write_md(split_pair(file.path(TAB, "table1_orig.csv"), yrs), "fg_table1",
         "Table 1 replicated: common-size balance sheet, 1971–1998 (replication | Frank and Goyal 2003)",
         "Each item divided by total assets, averaged across firms in the year. For each year, the first column is the replication (CCM firms) and the second is F&G's Table 1. Items are trimmed at 0.5% per tail; items F&G recode from missing to zero are recoded the same way.")
write_md(split_pair(file.path(TAB, "table2_orig.csv"), yrs), "fg_table2",
         "Table 2 replicated: corporate cash flows as a fraction of total assets, 1971–1998 (replication | Frank and Goyal 2003)",
         "Common sample of firm-years with every component non-missing after trimming, so that the deficit equals net external financing. Definitions as in F&G Table 2, notes a–d.")
for (nm in c("table1_ext", "table2_ext")) {
  x <- fread(file.path(TAB, paste0(nm, ".csv")), colClasses = "character", header = TRUE)
  setnames(x, c("item", "1971-98 avg", "1999-25 avg", "1971-25 avg"), c("Item", "1971–1998", "1999–2025", "1971–2025"), skip_absent = TRUE)
  write_md(x, paste0("fg_", nm),
           if (nm == "table1_ext") "Table 1 extended: common-size balance sheet, selected years 2000–2025 and period averages"
           else "Table 2 extended: corporate cash flows, selected years 2000–2025 and period averages",
           "Period columns are pooled firm-year means; N is firm-years. Same definitions, screens and trimming as the 1971–1998 table.")
}

# ---- Table 3: replication next to F&G's Tables 3 and 4 --------------------
t3 <- fread(file.path(TAB, "table3_raw.csv"))
cell <- function(r) c(paste0(f3(r$a), stars(r$a_p)), paste0("(", f3(r$a_se), ")"), paste0(f3(r$b), stars(r$b_p)),
                      paste0("(", f3(r$b_se), ")"), formatC(r$t_b1, format = "f", digits = 1), fN(r$N), fN(r$firms), f3(r$r2))
fg <- list(T3c4 = c("−0.002***", "(<0.001)", "0.283***", "(0.002)", "", "89,883", "", "0.265"),
           T4c4 = c("−0.007***", "(0.001)", "0.148***", "(0.002)", "", "57,687", "", "0.120"),
           T3c1 = c("0.001*", "(<0.001)", "0.748***", "(0.004)", "", "14,592", "768", "0.708"))
t3v <- data.table(Statistic = c("Constant", "", "Financing deficit (b)", "", "t-stat, H0: b = 1", "N (firm-years)", "Firms", "R²"))
lab <- c("(a) 1971-2025" = "(a) 1971–2025", "(b) 1971-1998" = "(b) 1971–1998", "(c) 1999-2025" = "(c) 1999–2025")
for (l in names(lab)) t3v[[lab[[l]]]] <- cell(t3[label == l])
t3v[["71–89 gaps"]] <- cell(t3[label == "1971-1989, gaps permitted"]); t3v[["F&G T3 (4)"]] <- fg$T3c4
t3v[["90–98 gaps"]] <- cell(t3[label == "1990-1998, gaps permitted"]); t3v[["F&G T4 (4)"]] <- fg$T4c4
t3v[["71–89 no gaps"]] <- cell(t3[label == "1971-1989, no gaps"]);     t3v[["F&G T3 (1)"]] <- fg$T3c1
write_md(t3v, "fg_table3_sbs",
         "Table 3 replicated and extended: pooled OLS of net debt issued on the financing deficit (replication | Frank and Goyal 2003)",
         "Columns (a)–(c) answer Q2.1. F&G do not report a pooled 1971–1998 regression; the last six columns put the replication beside F&G's own windows: Table 3 column (4) (1971–1989, gaps permitted), Table 4 column (4) (1990–1998, gaps permitted) and Table 3 column (1) (1971–1989, firms with no reporting gaps). Dependent variable: net debt issued / net assets. Replication: firm-clustered standard errors in parentheses; F&G: conventional OLS standard errors. *** p < 0.01, ** p < 0.05, * p < 0.10.")

# ---- Table 5: replication next to F&G's columns -------------------------
t5 <- fread(file.path(TAB, "table5_main.csv"), colClasses = "character", header = TRUE)
p5 <- fread(file.path(TAB, "table5_paper.csv"), colClasses = "character", header = TRUE)
t5v <- data.table(Variable = t5[[1]],
                  `(a) 1971–2025` = t5[[2]], `(b) 1971–1998` = t5[[3]], `(c) 1999–2025` = t5[[4]],
                  `71–89 no gaps` = t5[[5]], `F&G (1)` = p5[[2]],
                  `71–89 gaps` = t5[[6]], `F&G (4)` = p5[[3]],
                  `71–89 + CMLTD` = t5[[7]], `F&G (7)` = p5[[4]])
t5v[Variable == "Wald: all = PO values (chi2)", Variable := "Wald χ²: coefficients = (1, 1, 1, −1)"]
t5v[Variable == "Wald: equal magnitudes (chi2)", Variable := "Wald χ²: equal magnitudes"]
write_md(t5v, "fg_table5",
         "Table 5 replicated and extended: disaggregating the financing deficit (dependent variable: net debt issued / net assets)",
         "Columns (a)–(c) answer Q3.1. The last six columns put the replication next to F&G's Table 5 net-debt columns (1), (4) and (7) for 1971–1989. Internal cash flow enters with its own sign, so the pecking order predicts +1, +1, +1, −1. Firm-clustered standard errors in parentheses (F&G: conventional). *** p < 0.01, ** p < 0.05, * p < 0.10.")

# ---- Size terciles (Q3.2) -----------------------------------------------
sz <- fread(file.path(TAB, "size_terciles_raw.csv")); sd_ <- fread(file.path(TAB, "size_diff.csv"))
grp <- c("Small (bottom tercile)" = "Small", "Medium" = "Medium", "Large (top tercile)" = "Large")
szv <- data.table(Statistic = c("Financing deficit (b)", "", "t-stat, H0: b = 1", "R²", "N (firm-years)", "Firms", "Large − small (b)", ""))
for (p in c("1971-1998", "1999-2025", "1971-2025")) for (g in names(grp)) {
  r <- sz[period == p & label == g]; dd <- sd_[period == p]
  szv[[paste(sub("-", "–", p), grp[[g]])]] <- c(
    paste0(f3(r$b), stars(r$b_p)), paste0("(", f3(r$b_se), ")"), formatC(r$t_b1, format = "f", digits = 1),
    f3(r$r2), fN(r$N), fN(r$firms),
    if (grp[[g]] == "Large") c(paste0(f3(dd$diff), stars(dd$diff_p)), paste0("(", f3(dd$diff_se), ")")) else c("", ""))
}
ddd <- fread(file.path(TAB, "size_triple_interaction.csv"))
tr <- ddd[term == "def_na:large:post"]
write_md(szv, "fg_size",
         "Pecking order regression by market-capitalisation tercile, 1971–1998 vs 1999–2025",
         sprintf("Terciles formed each fiscal year on market capitalisation at the fiscal year end (CRSP; Compustat price × shares when CRSP is missing). Large − small = coefficient on DEF × Large in a pooled regression on the two extreme terciles, firm-clustered. Change in the large − small gap after 1998 (DEF × Large × Post): %s (se %s, p %s). *** p < 0.01.",
                 f3(tr$Estimate), f3(tr$`Std. Error`), ifelse(tr$`Pr(>|t|)` < .001, "< 0.001", f3(tr$`Pr(>|t|)`))))

# ---- Table 6 in the paper's layout --------------------------------------
res <- readRDS(file.path(OUT_DIR, "results.rds"))
t6 <- as.data.table(res$t6)
qn <- c("Smallest", "Medium small", "Medium large", "Largest")
t6v <- data.table(Statistic = c("Replication: b", "", "R²", "N", "F&G: b", "F&G: R²", "F&G: N"))
k <- 0
for (p in c("1971-1989", "1990-1998", "1999-2025")) for (q in qn) {
  r <- t6[period == p & label == q]
  fg <- if (p == "1999-2025") c("", "", "") else { k <- k + 1; c(PAPER_T6$b[k], PAPER_T6$r2[k], PAPER_T6$N[k]) }
  t6v[[paste(sub("-", "–", p), q)]] <- c(paste0(f3(r$b), stars(r$b_p)), paste0("(", f3(r$b_se), ")"), f3(r$r2), fN(r$N), fg)
}
write_md(t6v, "fg_table6",
         "Table 6 replicated (columns 4–11) and extended: pecking order tests by quartile of total assets",
         "Quartiles of book assets formed each fiscal year, as in F&G. F&G report only 1971–1989 and 1990–1998. Firm-clustered standard errors in parentheses. *** p < 0.01.")

# ---- Missing values ------------------------------------------------------
raw <- readRDS(file.path(RAW_DIR, "raw_funda.rds"))
raw[, fyear := as.integer(fyear)]
pu <- readRDS(file.path(PROC_DIR, "panel_untrimmed.rds"))[, .(gvkey, fyear, scf, net_assets)]
m <- merge(pu, unique(raw, by = c("gvkey", "fyear")), by = c("gvkey", "fyear"), suffixes = c("", ".raw"))
m[, per := fifelse(fyear <= 1984, "1971–1984", fifelse(fyear <= 1998, "1985–1998", "1999–2025"))]
items <- data.table(
  item = c("ibc", "sstk", "dltis", "dltr", "prstkc", "dv", "capx", "aqc", "chech", "dlcch", "xidoc", "fopo", "at", "lct", "scf"),
  label = c("Income before extraordinary items (IBC)", "Sale of stock (SSTK)", "LT debt issuance (DLTIS)",
            "LT debt reduction (DLTR)", "Purchase of stock (PRSTKC)", "Cash dividends (DV)", "Capital expenditure (CAPX)",
            "Acquisitions (AQC)", "Change in cash (CHECH)", "Change in current debt (DLCCH)", "Extraordinary items (XIDOC)",
            "Funds from operations – other (FOPO)", "Total assets (AT)", "Current liabilities (LCT)", "Cash-flow format code (SCF)"),
  treatment = c("Kept missing: drops C and DEF", "Kept missing: drops ΔE", "Kept missing: drops ΔD", "Recoded to 0",
                "Recoded to 0", "Recoded to 0", "Recoded to 0", "Recoded to 0", "Recoded to 0 (judgement call)",
                "Recoded to 0", "Recoded to 0", "Recoded to 0", "Screen: firm-year dropped", "Kept missing: no net assets",
                "Kept missing: no flow variables"))
mt <- items[, .(Item = label, Treatment = treatment)]
for (p in c("1971–1984", "1985–1998", "1999–2025")) {
  z <- m[per == p]
  mt[[paste0(p, " % missing")]] <- sapply(items$item, function(v) pct(mean(is.na(z[[if (v == "scf") "scf.raw" else v]]))))
}
write_md(mt, "fg_missing",
         "Missing values in key Compustat items (screened sample, before recoding) and their treatment",
         left = 1:2, note = "Share of screened firm-years (CCM, non-financial, non-utility, positive assets) with the item missing in Compustat. Recoding follows F&G (footnote 3; Tables 1 and 8): items that firms leave blank or combine with another line are set to zero. Items F&G did not recode are left missing and the firm-year leaves the regression that needs them.")

# Observation flow: where firm-years are lost
pt <- readRDS(file.path(PROC_DIR, "panel.rds"))
flow <- rbindlist(lapply(list(c(1971, 1998), c(1999, 2025)), function(b) {
  u <- readRDS(file.path(PROC_DIR, "panel_untrimmed.rds"))[fyear >= b[1] & fyear <= b[2]]
  t <- pt[fyear >= b[1] & fyear <= b[2]]
  pos <- u[!is.na(net_assets) & net_assets > 0]
  have <- pos[!is.na(def_na) & !is.na(nd_na) & !is.na(ne_na)]
  data.table(Period = paste0(b[1], "–", b[2]),
             `After screens` = nrow(u), `Positive net assets` = nrow(pos),
             `DEF, ΔD, ΔE non-missing` = nrow(have),
             `After 0.5% trim (regression sample)` = sum(t$in_reg),
             `Lost to missing items` = nrow(pos) - nrow(have),
             `Lost to trimming` = nrow(have) - sum(t$in_reg))
}))
flow[, (names(flow)[-1]) := lapply(.SD, fN), .SDcols = names(flow)[-1]]
write_md(flow, "fg_obs_flow", "From screened firm-years to the regression sample: losses to missing data and to trimming",
         "Lost to missing items = firm-years with positive net assets but a missing DEF, ΔD or ΔE (mostly a missing IBC, SSTK or DLTIS, or no format code).")

# ---- Outliers: cut-offs and effect of trimming ---------------------------
u <- readRDS(file.path(PROC_DIR, "panel_untrimmed.rds"))[!is.na(net_assets) & net_assets > 0]
vars <- c(def_na = "Financing deficit", nd_na = "Net debt issued", ne_na = "Net equity issued",
          inv_na = "Investment", cf_na = "Internal cash flow", dwc_na = "Δ working capital", div_na = "Dividends")
ot <- rbindlist(lapply(names(vars), function(v) rbindlist(lapply(list(c(1971, 1998), c(1999, 2025)), function(b) {
  z <- u[fyear >= b[1] & fyear <= b[2]][[v]]; z <- z[is.finite(z)]
  q <- quantile(z, c(.005, .995)); k <- z[z >= q[1] & z <= q[2]]
  data.table(Variable = vars[[v]], Period = paste0(b[1], "–", substr(b[2], 3, 4)),
             `0.5% cut-off` = f3(q[1]), `99.5% cut-off` = f3(q[2]),
             `Raw min` = formatC(min(z), format = "f", digits = 1), `Raw max` = formatC(max(z), format = "f", digits = 1),
             `Mean (sd), untrimmed` = paste0(f3(mean(z)), " (", formatC(sd(z), format = "f", digits = 2), ")"),
             `Mean (sd), trimmed` = paste0(f3(mean(k)), " (", f3(sd(k)), ")"))
}))))
write_md(ot, "fg_outliers", left = 1:2,
         "Outlier treatment: trimming cut-offs and the effect of trimming on the scaled variables",
         "All variables are scaled by net assets (total assets − current liabilities). Untrimmed = firm-years with positive net assets. Values outside the cut-offs are set to missing (F&G p. 225). Cut-offs are computed separately for 1971–1998 and 1999–2025.")

# ---- Identity check ------------------------------------------------------
idc <- fread(file.path(TAB, "identity_check.csv"))
write_md(idc[, .(Period = period, Format = fmt, `Share with abs(ΔD + ΔE − DEF) < 0.001` = f3(share_exact_1e3),
                 `Mean absolute gap` = formatC(mean_abs_gap, format = "e", digits = 1))],
         "fg_identity", "Accounting identity check in the regression sample (variables scaled by net assets)",
         "If the recoding respects the cash-flow statement, net debt plus net equity equals the deficit.")
cat("Paper-layout tables written to", MD, "\n")

# ---- Older tables in the same format -------------------------------------
# Sample construction, straight from the CSV written by 02_build_sample.R
sc <- fread(file.path(OUT_DIR, "sample_construction.csv"))
write_md(sc[, .(Step = step, `Firm-years` = fN(firm_years), Firms = fN(firms),
                `1971–1998` = fN(fy_1971_1998), `1999–2025` = fN(fy_1999_2025))],
         "fg_sample_construction", "Sample construction: firm-years remaining after each screen (fiscal years 1971–2025)")

for (nm in c("table_correlations", "table_desc_periods", "table_breaks",
             "table3_main", "table3_compare", "table_robustness", "table3_fg_layout", "table4_fg_layout",
             "table_horse_race", "table_horse_race_test")) convert_md(nm, left = if (nm %in% c("table3_compare", "table_breaks")) 1:2 else 1)
ht <- file.path(MD, "fg_table_horse_race_test.md"); l <- readLines(ht, warn = FALSE)
l <- gsub("[0-9.]+e[+−-][0-9]+", "< 0.001", l); l <- sub("Share of obs with dD + dE = DEF exactly", "Identity holds (share)", l, fixed = TRUE)
writeLines(l, ht)
cat("converted older tables\n")
