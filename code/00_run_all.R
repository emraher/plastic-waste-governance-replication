# Master script: runs the replication pipeline, each step in its own R session,
# and stops at the first failure. Run from the repository root with
#   Rscript replication/code/00_run_all.R
#
# The script picks one of two modes automatically.
#
# Full rebuild: with the raw inputs staged in replication/data/, every step runs
#   and the analysis panel is rebuilt from BACI.
#
# Panel-only: with no raw inputs but the shipped analysis panel present in
#   replication/output/, the steps that read only the panel still run — the main
#   PPML estimates, both figures, and the headline audit. Steps 03 and 05
#   re-derive product baskets, mirror statistics, and adjacent-product
#   descriptives from raw BACI and Comtrade, so they are skipped and reported.

library(here)
library(cli)

here::i_am("replication/code/00_run_all.R")

data_dir <- here("replication", "data")
out_dir <- here("replication", "output")
for (d in c(data_dir, out_dir)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

panel_path <- file.path(out_dir, "plastic_corruption_panel.parquet")

# Raw inputs each step needs, relative to replication/data/. Steps not listed
# here read only the analysis panel and the output of earlier steps.
raw_needs <- list(
  "01_build_panel.R" = c(
    "baci", "comtrade",
    "comtrade/comtrade_4707.rds", "comtrade/placebo_4707",
    "controls/epi.csv", "controls/gdp_per_capita.csv",
    "corruption/cpi.csv", "corruption/vdem.parquet", "corruption/wgidataset.dta",
    "gravity/dist_cepii.dta",
    "membership/panel_ban.csv", "membership/panel_eu.csv", "membership/panel_oecd.csv",
    "policy/plastic_policy_events.csv",
    "reference/plastic_hs6_crosswalk.csv", "reference/waste_categories.csv"
  ),
  "03_robustness.R" = c(
    "baci",                          # product-relabeling control baskets
    "comtrade",                      # mirror statistics
    "corruption/wgidataset.dta",
    "gravity/dist_cepii.dta",
    "reference/plastic_hs6_crosswalk.csv",
    "reference/waste_categories.csv"
  ),
  "05_tables.R" = "baci"             # adjacent-product descriptives
)

# Report which of `paths` are unusable. A broken symlink reports FALSE from
# file.exists(), which is what we want; a directory that exists but holds
# nothing is equally unusable.
missing_inputs <- function(paths) {
  if (length(paths) == 0L) return(character())
  full <- file.path(data_dir, paths)
  ok <- file.exists(full)
  is_dir <- ok & dir.exists(full)
  ok[is_dir] <- vapply(full[is_dir], \(p) length(list.files(p)) > 0L, logical(1))
  paths[!ok]
}

scripts <- c(
  "01_build_panel.R",     # BACI + corruption panel
  "02_estimate_main.R",   # main PPML, event study, robustness set
  "03_robustness.R",      # appendix robustness (rivals, LOO, HonestDiD, mirror, ...)
  "04_figures.R",         # Figures 1-2
  "05_tables.R",          # appendix descriptive tables + randomization inference
  "06_audit.R"            # reproducibility audit (asserts headline numbers)
)

have_panel <- file.exists(panel_path)
panel_only <- length(missing_inputs(raw_needs[["01_build_panel.R"]])) > 0L

if (panel_only && !have_panel) {
  cli_abort(c(
    "Nothing to run: no raw inputs and no analysis panel.",
    "x" = "{.path replication/data/} is missing the inputs step 01 needs.",
    "x" = "{.path replication/output/plastic_corruption_panel.parquet} is not present either.",
    "i" = "The raw data are not distributed with the code.",
    "i" = "{.file replication/data/README.md} lists every input and where it comes from.",
    "i" = "Helpers for the scriptable sources are in {.file replication/code/download/}."
  ))
}

if (panel_only) {
  cli_alert_info(c(
    "Panel-only run: using the shipped analysis panel; step 01 is not needed."
  ))
} else {
  cli_alert_info("Full rebuild: raw inputs found; the panel will be rebuilt from BACI.")
}

skipped <- list()

for (script in scripts) {
  gaps <- missing_inputs(raw_needs[[script]])

  if (script == "01_build_panel.R" && panel_only) {
    cli_alert_info("Skipping {.file 01_build_panel.R} — using the shipped panel.")
    next
  }
  if (length(gaps) > 0L) {
    skipped[[script]] <- gaps
    cli_alert_warning("Skipping {.file {script}} — needs {.file {gaps}}.")
    next
  }

  cli_h1(script)
  status <- system2("Rscript", shQuote(here("replication", "code", script)))
  if (status != 0L) {
    cli_abort("Step {.file {script}} failed (exit status {status}).")
  }
}

if (length(skipped) == 0L) {
  cli_alert_success("Replication pipeline complete. Outputs are in replication/output/.")
} else {
  cli_alert_success("Pipeline finished. Outputs are in replication/output/.")
  cli_inform(c(
    "!" = "{length(skipped)} step{?s} skipped for want of raw trade data: {.file {names(skipped)}}",
    "i" = "Those steps rebuild product baskets, mirror statistics, and adjacent-product
           descriptives from raw BACI and Comtrade, which the analysis panel cannot
           substitute for.",
    "i" = "To run them, populate {.path replication/data/} as described in
           {.file replication/data/README.md} and rerun this script."
  ))
}
