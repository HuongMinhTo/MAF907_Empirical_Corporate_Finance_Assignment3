# Export the final firm-year sample as a compressed CSV (the data sample that
# accompanies the report): data/processed/fg_sample.csv.gz.

source("code/00_config.R")
load_packages(c("data.table"))

p <- readRDS(file.path(PROC_DIR, "panel.rds"))
keep <- c("gvkey", "permno", "conm", "fyear", "datadate", "sic", "scf", "at", "lct", "net_assets", "mcap",
          "div", "inv", "dwc", "cf", "def", "nd", "gd", "ne", "def_na", "div_na", "inv_na", "dwc_na", "cf_na",
          "nd_na", "gd_na", "ne_na", "dltd_ratio", "cmltd_na", "in_reg", "in_reg_comp", "size_terc", "size_quart")
fwrite(p[, ..keep], file.path(PROC_DIR, "fg_sample.csv.gz"))
cat("Sample written to", file.path(PROC_DIR, "fg_sample.csv.gz"), "\n")
