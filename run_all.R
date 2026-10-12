# Frank & Goyal (2003) replication, 1971-2025. Run from the project folder:
#   source("run_all.R")   or   Rscript run_all.R
steps <- c("code/01_get_data.R",      # WRDS download (skipped if the raw files exist)
           "code/02_build_sample.R",  # screens, variables, missing values, trimming
           "code/03_analysis.R",      # Q1-Q4 tables, tests and figures
           "code/04_robustness.R",    # robustness table, identity check
           "code/05_figures_bw.R",    # black-and-white figures
           "code/06_paper_tables.R",  # tables in the paper's layout, data-quality tables
           "code/07_export_sample.R") # final sample as CSV
for (s in steps) {
  if (s == "code/01_get_data.R" && file.exists("data/raw/raw_funda.rds")) { message("Raw data found, skipping download"); next }
  message("Running ", s); source(s, local = new.env())
}
