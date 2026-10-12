# Frank & Goyal (2003) replication, extended to 1971-2025

Author: Minh Huong To

Replicates Tables 1-6 and Figure 1 of Frank, M.Z., Goyal, V.K. (2003), "Testing the pecking order theory of capital structure", *JFE* 67, 217-248, on CRSP/Compustat Merged data, and extends the sample to fiscal 2025.

**Report:** submitted separately (not in the repository). The code produces every table and figure used in it.

## Run it

1.  Open `FG2003_replication.Rproj` (or set the working directory to this folder).
2.  `source("run_all.R")`. Missing packages (`data.table`, `fixest`, `sandwich`, `ggplot2`, `strucchange`, `DBI`, `RPostgres`) are installed automatically.

The WRDS download (step 1) needs `~/.pgpass` with `wrds-pgdata.wharton.upenn.edu:9737:wrds:<username>:<password>`. If `data/raw/raw_funda.rds` is present, the download is skipped.

## Folder structure

```         
Assignment3/
├── README.md                  this file
├── run_all.R                  runs the whole pipeline, raw data -> tables, figures, sample file
├── FG2003_replication.Rproj   RStudio project (sets the working directory)
├── code/                      all code (run in the numbered order by run_all.R)
├── data/
│   ├── raw/                   WRDS extracts, never modified (raw_funda, raw_company, raw_ccm, raw_crsp_me)
│   └── processed/             analysis panels and the final sample (fg_sample.csv.gz)
├── output/
│   ├── tables/                every table as CSV
│   ├── tables_md/             the same tables as Markdown (fg_* = Frank & Goyal's layout)
│   ├── figures/               colour figures (03_analysis.R)
│   ├── figures_bw/            black-and-white figures (05_figures_bw.R)
│   └── *.csv, results.rds     sample-construction counts, missing-value audit, saved estimates
├── paper/                     Frank & Goyal (2003) PDF and the cropped original Figure 1
└── report/                    the written report
```

`data/`, `paper/` and `report/` are not in the Git repository (see `.gitignore`): the WRDS data are licensed, the paper is copyrighted, and the report is submitted separately.

| Script (`code/`) | What it does |
|----|----|
| `00_config.R` | Paths, periods, trimming rule (0.5% per tail within 1971-98 and 1999-2025), items recoded to zero |
| `01_get_data.R` | WRDS download into `data/raw/`: Compustat `funda` 1970-2025, company header, CCM link history, CRSP CIZ monthly market cap |
| `02_build_sample.R` | Screens (CCM link, SIC 6000-6999 and 4900-4999, `compst = 'AB'`, missing assets, format codes 4-6), F&G variable definitions, missing-value recoding, trimming, size groups; writes `data/processed/` |
| `03_analysis.R` | Q1 Tables 1-2, descriptives, structural-break tests; Q2 Tables 3-4; Q3 Table 5, size terciles, Table 6; Q4 debt vs equity, b_t/c_t |
| `04_robustness.R` | Sensitivity of b to universe, trimming, scaling, IPO years, estimator; identity check |
| `05_figures_bw.R` | Black-and-white versions of all figures |
| `06_paper_tables.R` | Tables in F&G's layout (replication beside the published numbers), missing-value and outlier tables |
| `07_export_sample.R` | Final firm-year sample as CSV: `data/processed/fg_sample.csv.gz` |
| `paper_values.R` | Published F&G numbers used in the comparisons |
