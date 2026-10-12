# Settings for the Frank & Goyal (2003) replication, extended to 1971-2025.
# Run every script from the project folder (the one with code/, data/, output/).

if (!file.exists("code/00_config.R")) {
  stop("Set the working directory to the project folder first (open FG2003_replication.Rproj)")
}

# Install any missing packages from CRAN, then load them.
load_packages <- function(pkgs) {
  have <- function(p) requireNamespace(p, quietly = TRUE)
  missing <- pkgs[!vapply(pkgs, have, logical(1))]
  if (length(missing) > 0) {
    repos <- getOption("repos")
    if (is.null(repos) || any(repos %in% c("@CRAN@", ""))) repos <- "https://cloud.r-project.org"
    message("Installing missing packages: ", paste(missing, collapse = ", "))
    install.packages(missing, repos = repos)
  }
  for (p in pkgs) suppressPackageStartupMessages(library(p, character.only = TRUE))
  invisible(TRUE)
}

DATA_DIR <- Sys.getenv("FG_DATA_DIR", unset = "data")
RAW_DIR  <- file.path(DATA_DIR, "raw")        # WRDS extracts, never modified
PROC_DIR <- file.path(DATA_DIR, "processed")  # panels built from the raw data
OUT_DIR  <- Sys.getenv("FG_OUT_DIR",  unset = "output")
dir.create(RAW_DIR,  showWarnings = FALSE, recursive = TRUE)
dir.create(PROC_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(OUT_DIR,  showWarnings = FALSE, recursive = TRUE)

# Sample period. 1970 is downloaded only to build lags (change in debt ratio,
# beginning-of-year current maturities).
FIRST_YEAR <- 1971L
LAST_YEAR  <- 2025L

# Periods used throughout. "orig" is F&G's sample period; "ext" is the extension.
PERIODS <- list(
  full    = c(1971L, 2025L),
  orig    = c(1971L, 1998L),
  ext     = c(1999L, 2025L),
  ssm     = c(1971L, 1989L),   # F&G Table 3 / Table 5 (Shyam-Sunder & Myers period)
  nineties = c(1990L, 1998L)   # F&G Table 4
)
PERIOD_LABEL <- c(full = "1971-2025", orig = "1971-1998", ext = "1999-2025",
                  ssm = "1971-1989", nineties = "1990-1998")

# Outliers: F&G trim the most extreme 0.5% in each tail (p. 225). The cut-offs
# are computed separately within 1971-1998 and 1999-2025, so the 1971-1998
# sample does not depend on whether the extension is added.
TRIM_P <- 0.005
TRIM_BLOCKS <- list(c(1971L, 1998L), c(1999L, 2025L))

# Selected years for Tables 1 and 2 (the paper's years, then the extension)
T12_YEARS_ORIG <- c(1971L, 1975L, 1980L, 1985L, 1990L, 1995L, 1998L)
T12_YEARS_EXT  <- c(2000L, 2005L, 2010L, 2015L, 2020L, 2025L)

# Compustat items recoded from missing to zero before trimming (F&G fn. 3,
# Table 1 notes and the starred items of Table 8). Issuance items (sstk,
# dltis) and income before extraordinary items (ibc) are NOT recoded: in
# F&G's Table 9 their counts are lower than those of the recoded items.
# chech is recoded although Table 8 does not star it: before 1985 it is
# missing for ~90% of format 1-3 firm-years, and without the recode almost
# all 1971-84 deficits would be lost (F&G's deficit has the largest N in Table 9).
RECODE_BS <- c("ivaeq", "ivao", "intan", "ao", "dlc", "txp", "lco", "lo", "txditc", "mib")
RECODE_CF <- c("dv", "capx", "ivch", "aqc", "fuseo", "sppe", "siv", "ivstch", "ivaco",
               "wcapc", "chech", "dlcch", "recch", "invch", "apalch", "txach", "aoloch",
               "fiao", "xidoc", "dpc", "txdc", "esubc", "sppiv", "fopo", "fsrco", "exre",
               "prstkc", "dltr")
