# Download the raw data from WRDS into data/raw/.
#
# Login: put this line in ~/.pgpass and run `chmod 600 ~/.pgpass`
#   wrds-pgdata.wharton.upenn.edu:9737:wrds:<username>:<password>
# The username is read from that file, the password is picked up by the driver.
#
# Files written (all raw, unfiltered except for the standard Compustat screens):
#   data/raw/raw_funda.rds    Compustat annual fundamentals, 1970-2025
#   data/raw/raw_company.rds  Compustat company header (header SIC)
#   data/raw/raw_ccm.rds      CRSP/Compustat Merged link history
#   data/raw/raw_crsp_me.rds  CRSP monthly market capitalisation (CIZ msf_v2)

source("code/00_config.R")
load_packages(c("data.table", "DBI", "RPostgres"))

WRDS_HOST <- "wrds-pgdata.wharton.upenn.edu"
WRDS_PORT <- 9737L

pgpass <- path.expand("~/.pgpass")
if (!file.exists(pgpass)) stop("~/.pgpass not found")
pg_line <- grep(paste0("^", WRDS_HOST, ":", WRDS_PORT, ":wrds:"), readLines(pgpass, warn = FALSE), value = TRUE)
if (length(pg_line) == 0) stop("~/.pgpass has no line for ", WRDS_HOST, ":", WRDS_PORT, ":wrds")
wrds_user <- strsplit(pg_line[1], ":", fixed = TRUE)[[1]][4]

wrds <- dbConnect(Postgres(), host = WRDS_HOST, port = WRDS_PORT, dbname = "wrds",
                  user = wrds_user, sslmode = "require")
cat("Connected to WRDS as", wrds_user, "\n")

# 1. Compustat annual. Legacy item numbers used by F&G in brackets.
items <- c(
  # identifiers and screens
  "gvkey", "datadate", "fyear", "scf", "compst", "sich", "fic", "curcd",
  # balance sheet (Table 1)
  "at",     # #6   total assets
  "che", "ch", "ivst",  # #1 cash & ST investments, #162 cash, #193 ST investments
  "rect", "invt", "aco", "act", "ppent", "ivaeq", "ivao", "intan", "ao",
  "dlc", "ap", "txp", "lco", "lct", "dltt", "lo", "txditc", "mib", "mibt", "lt",
  "pstk", "ceq", "seq",
  # flow of funds (Table 2, Appendix A)
  "dv",      # #127 cash dividends
  "capx",    # #128
  "ivch",    # #113 increase in investments
  "aqc",     # #129 acquisitions
  "fuseo",   # #219 uses of funds - other
  "sppe",    # #107 sale of PP&E
  "siv",     # #109 sale of investments
  "ivstch",  # #309 change in ST investments
  "ivaco",   # #310 investing activities - other
  "wcapc",   # #236 working capital change - other
  "chech",   # #274 cash and equivalents - increase (decrease)
  "dlcch",   # #301 change in current debt
  "recch", "invch", "apalch", "txach", "aoloch",  # #302-#305, #307
  "fiao",    # #312 financing activities - other
  "ibc",     # #123 income before extraordinary items (CF statement)
  "xidoc",   # #124 extraordinary items and discontinued operations
  "dpc",     # #125 depreciation and amortisation
  "txdc",    # #126 deferred taxes
  "esubc",   # #106 equity in net loss (earnings)
  "sppiv",   # #213 (gain) loss on sale of PP&E and investments
  "fopo",    # #217 funds from operations - other
  "fsrco",   # #218 sources of funds - other
  "exre",    # #314 exchange rate effect
  "sstk",    # #108 sale of common and preferred stock
  "prstkc",  # #115 purchase of common and preferred stock
  "dltis",   # #111 long-term debt issuance
  "dltr",    # #114 long-term debt reduction
  "dd1",     # #44  long-term debt due in one year
  # other characteristics
  "sale", "oibdp", "csho", "prcc_f", "xrd", "ib", "oancf"
)
funda <- setDT(dbGetQuery(wrds, sprintf("
  SELECT %s FROM comp.funda
  WHERE indfmt = 'INDL' AND datafmt = 'STD' AND popsrc = 'D' AND consol = 'C'
    AND fyear BETWEEN 1970 AND %d", paste(items, collapse = ", "), LAST_YEAR)))
saveRDS(funda, file.path(RAW_DIR, "raw_funda.rds"), compress = "xz")
cat(sprintf("Compustat funda: %d firm-years, %d firms\n", nrow(funda), uniqueN(funda$gvkey)))

# 2. Company header (header SIC when the historical SIC is missing)
company <- setDT(dbGetQuery(wrds, "SELECT gvkey, conm, sic, fic, loc FROM comp.company"))
saveRDS(company, file.path(RAW_DIR, "raw_company.rds"), compress = "xz")

# 3. CRSP/Compustat Merged links (standard WRDS link filters)
ccm <- setDT(dbGetQuery(wrds, "
  SELECT gvkey, lpermno AS permno, linktype, linkprim, linkdt, linkenddt
  FROM crsp.ccmxpf_lnkhist
  WHERE linktype IN ('LU', 'LC') AND linkprim IN ('P', 'C')"))
saveRDS(ccm, file.path(RAW_DIR, "raw_ccm.rds"), compress = "xz")
cat(sprintf("CCM links: %d\n", nrow(ccm)))

# 4. CRSP monthly market cap (CIZ; legacy msf stops in 12/2024)
me <- setDT(dbGetQuery(wrds, sprintf("
  SELECT permno, yyyymm, mthcap FROM crsp.msf_v2
  WHERE mthcaldt BETWEEN '1969-01-01' AND '%d-12-31' AND mthcap IS NOT NULL", LAST_YEAR + 1L)))
saveRDS(me, file.path(RAW_DIR, "raw_crsp_me.rds"), compress = "xz")
cat(sprintf("CRSP monthly caps: %d\n", nrow(me)))

dbDisconnect(wrds)
