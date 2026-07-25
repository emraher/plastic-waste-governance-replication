# Replication package

**Governance Sorting in Plastic-Waste Trade after China's Import Restrictions**

Emrah Er (Ankara University) · Işıl Şirin Selçuk (Bolu Abant İzzet Baysal
University)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.21572495.svg)](https://doi.org/10.5281/zenodo.21572495)
[![Licence: MIT + CC BY 4.0](https://img.shields.io/badge/licence-MIT%20%2B%20CC%20BY%204.0-blue)](LICENSE.md)

Manuscript submitted to *Ecological Economics* (2026).
<!-- On acceptance, replace the line above with:
     Published in *Ecological Economics* (2026), https://doi.org/10.1016/j.ecolecon.YYYY.NNNNNN -->

Code, the analysis panel, and the Stata cross-check for every result and figure
reported in the manuscript and appendices. The pipeline is written in modern
tidyverse R (`dplyr` / `tidyr` / `readr` / `purrr` / `stringr`, native pipe,
`here` for paths, `fixest` for the PPML estimation).

## Two ways to run this

**Panel-only (no downloads).** The constructed analysis panel is shipped in
`output/plastic_corruption_panel.parquet`. With nothing else, the pipeline
reproduces the main PPML estimates, the event study, both figures, and the
headline audit. This is the fastest way to check the paper's central results.

**Full rebuild.** With the third-party inputs staged in `data/` alongside the
tables shipped here, the panel is rebuilt from BACI and every appendix result
runs as well.

`00_run_all.R` chooses between them automatically: it creates `data/` and
`output/` if they do not exist, skips the panel build when the shipped panel is
present, and skips any step whose raw inputs are absent rather than failing. At
the end it reports exactly what ran and what did not.

## What is in the package

| Path | Contents |
|------|----------|
| `code/00_run_all.R` | Master script; runs each step in its own R session |
| `code/01`–`06` | The pipeline (see the table below) |
| `code/R/` | Shared helpers and the heavier robustness sub-steps |
| `code/download/` | Data-acquisition helpers for the scriptable sources |
| `data/` | The authors' curated tables, plus the manifest for the third-party inputs |
| `output/` | The shipped analysis panel and product-class key; every result is written here |
| `stata/` | Independent Stata cross-check of the estimation results |
| `LICENSE.md` | MIT for the code, CC BY 4.0 for the authors' tables and derived files |

## What data ships, and what you must obtain

**Included here.** The tables the authors compiled, because they are not
available anywhere else: the HS 3915 treated-code crosswalk, the waste-category
codes, the Basel-ban, EU, and OECD membership panels, and the destination
policy-event table. They sit under `data/` in the paths the pipeline expects.

Also included, under `output/`: the analysis dataset behind every reported
estimate, and `product_class_codes.csv`, the treated/comparison HS6 key reported
as Appendix A. These are what let the analysis run without rebuilding from raw
trade data. Everything else in `output/` is produced by the scripts.

The analysis panel ships in two formats with identical contents — 2,306,556 rows
by 50 columns. `plastic_corruption_panel.parquet` is what the pipeline reads.
`plastic_corruption_panel.csv.gz` is a plain-text mirror for anyone without
`arrow` or Stata 18+:

```r
panel <- arrow::read_parquet("replication/output/plastic_corruption_panel.parquet")
panel <- readr::read_csv("replication/output/plastic_corruption_panel.csv.gz")
```

```python
import pandas as pd
panel = pd.read_parquet("replication/output/plastic_corruption_panel.parquet")
panel = pd.read_csv("replication/output/plastic_corruption_panel.csv.gz")
```

In Stata, `gunzip` the CSV first and `import delimited`, or use `import parquet`
on Stata 18 or newer.

**Not included.** The third-party sources: CEPII BACI bilateral trade and
distances, UN Comtrade mirror flows, the V-Dem political corruption index, the
World Bank's Worldwide Governance Indicators and GDP per capita, the
Transparency International CPI, and the Yale Environmental Performance Index.
All are publicly available, though BACI, Comtrade, and V-Dem require a free
registration. `data/README.md` lists each one with its source and expected path,
and the helpers in `code/download/` fetch the scriptable ones.

## Requirements

R 4.3 or newer (native pipe, `\(x)` lambdas). All packages are on CRAN:

```r
install.packages(c(
  "arrow", "cli", "dplyr", "fixest", "ggplot2", "glue", "haven", "here",
  "HonestDiD", "purrr", "readr", "rlang", "stringr", "tibble", "tidyr"
))
```

(`grid` and `stats` also ship with R.) The released results were produced with
R 4.4.3 and dplyr 1.2.1, tidyr 1.3.1, readr 2.2.0, stringr 1.6.0, purrr 1.2.0,
tibble 3.3.1, arrow 21.0.0, haven 2.5.5, here 1.0.2, cli 3.6.6, glue 1.8.0,
fixest 0.13.2, ggplot2 4.0.1, and HonestDiD 0.2.8. Step 06 records the stack it
actually ran with in `output/session_info.txt`. A UTF-8 locale is recommended
(`LC_ALL=en_US.UTF-8`).

## Running it

**The directory must be named `replication/`.** Every script resolves its paths
with `here("replication", ...)`, so this is the one setup detail that will break
the run if you get it wrong. Unpack the archive, confirm you have a folder called
`replication`, and run from the folder *above* it:

```sh
unzip replication_package.zip     # creates replication/
Rscript replication/code/00_run_all.R
```

If you renamed the folder or moved the contents up a level, rename it back to
`replication` before running.

A panel-only run takes roughly ten to twenty minutes, dominated by the PPML fits
in step 02. A full rebuild takes a few hours.

In a full rebuild the 500-draw product-randomization inference in step 05
accounts for most of the time (~1,500 model fits). To smoke-test one quickly,
cut the draw count:

```sh
RI_DRAWS=10 Rscript replication/code/00_run_all.R
```

That reproduces the structure of every output but not the published
randomization p-values.

## What a successful run looks like

The script announces which mode it chose, names any step it skips and the inputs
that step was missing, and ends with a summary. Results land in `output/`.

A **panel-only** run reports `Panel-only run: using the shipped analysis panel`,
skips steps 01, 03, and 05, and finishes with the audit reporting
**8 checks passed** and 7 not run. Those 8 verify the paper's headline numbers
directly: the China-excluded triple differences (1.263 / 0.663 / 0.800 for
V-Dem / WGI / CPI), the full-sample triples, the relative ratios 3.54 / 1.94 /
2.23, the observation counts 257,497 and 261,099, and the pre-trend p-values.
Both manuscript figures are regenerated in `output/` as PDF and PNG. The 7
skipped checks belong to steps 03 and 05.

A **full rebuild** runs all six steps and all 15 checks. Either way,
`output/model_audit_checks.csv` is the verification artifact: one row per check
with a PASS or FAIL, and the run aborts if any check fails.

## Pipeline

| Script | Produces | Needs third-party data |
|--------|----------|------------------------|
| `01_build_panel.R` | The analysis panel and the product-class code list | Yes — every source |
| `02_estimate_main.R` | Main PPML triple-difference, event study, pre-trend tests, robustness set | No |
| `03_robustness.R` | Rival destination characteristics, concentration, leave-one-out, trade margins, control baskets, mirror statistics, HonestDiD bounds | Yes — BACI, Comtrade, WGI, CEPII distances |
| `04_figures.R` | Figure 1 (raw trends) and Figure 2 (event study) | No |
| `05_tables.R` | Appendix HS6 list, product-class summaries, adjacent-product descriptives, randomization inference | Yes — BACI |
| `06_audit.R` | Reproducibility ledger (`model_audit_checks.csv`) asserting the headline numbers | No |

Steps 03 and 05 re-derive product baskets, mirror statistics, and
adjacent-product descriptives from raw BACI and Comtrade at HS6 level. The
analysis panel is already collapsed to the estimation sample, so it cannot
substitute for those inputs — which is why a panel-only run skips them. Step 06
audits whatever was produced and lists the checks it could not run.

## Getting the third-party data

Only needed for a full rebuild; the included panel covers the main results.

`data/README.md` splits the inputs into the tables shipped here and the
third-party sources you must obtain, listing each of the latter with its
contents, source, and expected path. UN Comtrade, World Bank WGI, CEPII gravity,
and the Transparency International CPI have helpers in `code/download/`; BACI,
V-Dem, the World Bank bulk GDP file, and the Yale EPI are manual downloads with
no stable machine endpoint. The published results use frozen vintages, recorded
in the paper's software-and-vintage appendix, so re-running the helpers can pull
newer releases than the ones behind the reported numbers.

## Stata cross-check

`stata/` holds an independent Stata re-implementation of the estimation results.
`export_panel.R` writes the analysis panel to `estimation_panel.dta`, and
`estimation.do` and `robustness.do` re-derive the headline and appendix
coefficients with `ppmlhdfe`/`reghdfe`, confirming they match the R `fixest`
estimates to machine precision.

**This path needs two third-party files.** Besides the shipped panel,
`export_panel.R` reads `data/gravity/dist_cepii.dta` and
`data/corruption/wgidataset.dta` for the importer attributes the robustness
specifications use, and neither is redistributed here. Obtain them first as
`data/README.md` describes; the R pipeline's panel-only path does not need them.

```sh
Rscript replication/stata/export_panel.R
cd replication/stata
stata-mp -b do estimation.do
stata-mp -b do robustness.do
```

The cross-check is optional and is not part of `00_run_all.R`. See
`stata/README.md` for what each do-file estimates.

## Randomness

The pipeline has one stochastic step. The product-randomization inference in
`05_tables.R` is a seeded Monte Carlo (`code/R/randomization_inference.R`, seed
20260705) whose draw count is set by `RI_DRAWS` (default 500). Under the shipped
seed it reproduces exactly. The original implementation used a different draw
stream, so these p-values match the paper's qualitative result (V-Dem far in the
placebo tail, CPI only marginal) rather than being byte-identical to the
first-submission numbers. Everything else is deterministic.

## Reproduction notes

- Fixed effects are written as `fixest` interactions (`class^exp^imp`, etc.).
  Every identified and reported coefficient reproduces to machine precision.
  Lower-order terms fully absorbed by the fixed effects are not point-identified
  and are not reported in the paper.
- Step 02 writes `output/model_objects.rds` (about 500 MB) holding the fitted
  models for downstream steps. It is regenerated on each run and is not shipped.

## Citation

If you use this package, please cite both the paper and the archive.

<!-- Article DOI to be added on acceptance. Keep this block in sync with the
     paper's reference list. -->

> Er, Emrah and Işıl Şirin Selçuk (2026). "Governance Sorting in Plastic-Waste
> Trade after China's Import Restrictions." Manuscript submitted to *Ecological
> Economics*.

> Er, Emrah and Işıl Şirin Selçuk (2026). *Replication Data for: Governance
> Sorting in Plastic-Waste Trade after China's Import Restrictions* [dataset].
> Zenodo. https://doi.org/10.5281/zenodo.21572496
