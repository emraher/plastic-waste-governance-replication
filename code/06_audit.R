# Reproducibility audit: load the generated outputs and assert the manuscript's
# headline numbers. Writes a PASS/FAIL ledger and stops if any check fails.

library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(arrow)
library(here)
library(cli)

here::i_am("replication/code/06_audit.R")
out <- \(file) here("replication", "output", file)

ledger <- list()
check <- function(label, ok, detail = "") {
  ledger[[length(ledger) + 1L]] <<- tibble(check = label,
                                            status = if (isTRUE(ok)) "PASS" else "FAIL",
                                            detail = detail)
}
near <- \(x, target, tol = 5e-3) all(abs(x - target) < tol)

read_out <- \(f) read_csv(out(f), show_col_types = FALSE)

# Steps 03 and 05 re-derive product baskets and mirror statistics from raw BACI
# and Comtrade, so they cannot run from the shipped analysis panel alone. Audit
# whatever is present and record the rest, rather than erroring on a panel-only
# replication run.
have_out <- \(f) file.exists(out(f))
skipped_checks <- character()
note_skip <- function(what) skipped_checks <<- c(skipped_checks, what)

triple <- "treat:post_china_ban:weak_control_baseline"
by_measure <- \(d, col) d |> arrange(corruption_measure) |> pull({{ col }})
# corruption_measure sorts to: cpi_inverse, vdem_v2x_corr, wgi_cc_inverse


# Panel and product classes ----------------------------------------------------

panel <- read_parquet(out("plastic_corruption_panel.parquet"))
codes <- read_out("product_class_codes.csv")
check("Product classes 4/113/30",
      all(c(sum(codes$class == "plastic_waste"), sum(codes$class == "plastic_regular"),
            sum(codes$class == "general_waste")) == c(4, 113, 30)),
      "treated / regular-plastic / general-waste HS6 counts")
# 163 countries have complete governance coverage; the trade panel retains 162
# importers (Taiwan has governance data but is aggregated out of BACI imports).
check("Panel importers = 162", n_distinct(panel$imp) == 162)


# Main estimates (China-excluded and full sample) ------------------------------

main_xc <- read_out("main_ppml_coefficients_excluding_china.csv") |> filter(term == triple)
main_fs <- read_out("main_ppml_coefficients.csv") |> filter(term == triple)
check("China-excluded triple 1.263/0.663/0.800",
      near(by_measure(main_xc, estimate), c(0.800, 1.263, 0.663)),
      "cpi / vdem / wgi")
check("Full-sample triple 1.547/0.993/1.178",
      near(by_measure(main_fs, estimate), c(1.178, 1.547, 0.993)))
check("Relative ratios 3.54/1.94/2.23",
      near(exp(by_measure(main_xc, estimate)), c(2.23, 3.54, 1.94), tol = 5e-3))
check("China-excluded observations = 257,497", all(main_xc$observations == 257497))
check("Full-sample observations = 261,099", all(main_fs$observations == 261099))


# Timing and concentration -----------------------------------------------------

pre_xc <- read_out("pretrend_tests_excluding_china.csv")
check("Pretrend p-values 0.780/0.553/0.048",
      near(by_measure(pre_xc, p_value), c(0.048, 0.780, 0.553)))

if (have_out("additional_specifications.csv")) {
  conc <- read_out("additional_specifications.csv") |>
    filter(specification == "excl_turkey", term == triple)
  check("Turkey-excluded V-Dem triple = 0.758",
        near(conc |> filter(corruption_measure == "vdem_v2x_corr") |> pull(estimate), 0.758))
} else note_skip("Turkey-excluded V-Dem triple")

if (have_out("destination_decomposition.csv")) {
  decomp <- read_out("destination_decomposition.csv")
  check("Turkey V-Dem share = 66.1%",
        near(decomp |> filter(corruption_measure == "vdem_v2x_corr", imp == "TUR") |> pull(share_of_total_delta),
             0.661, tol = 1e-3))
} else note_skip("Turkey V-Dem share")

if (have_out("exporter_origin_decomposition.csv")) {
  eu_origin <- read_out("exporter_origin_decomposition.csv")
  check("EU15 share of Turkey increase = 78.6%",
        near(eu_origin |> filter(importer == "TUR", eu_origin == "EU15") |> pull(eu_share_of_delta),
             0.786, tol = 1e-3))
} else note_skip("EU15 share of Turkey increase")


# Product baskets and sensitivity ----------------------------------------------

if (have_out("product_control_baskets.csv")) {
  controls <- read_out("product_control_baskets.csv")
  check("All 12 control-basket estimates positive",
        all(controls$estimate > 0), "pooled / regular / general / policy-screened x 3")
} else note_skip("Control-basket estimates")

if (have_out("product_placebo.csv")) {
  placebo <- read_out("product_placebo.csv")
  check("Product placebo negative for all measures", all(placebo$estimate < 0))
} else note_skip("Product placebo")

if (!have_out("product_class_summary.csv")) note_skip("Product-summary medians")
if (!have_out("product_randomization_pvalues.csv")) note_skip("Randomization inference")

if (file.exists(out("product_class_summary.csv"))) {
  summ <- read_out("product_class_summary.csv")
  check("Product-summary medians (5.7/25.7/18.0 unit value)",
        near(summ |> arrange(match(class, c("plastic_waste","plastic_regular","general_waste"))) |> pull(median_unit_value_2014),
             c(5.7, 25.7, 18.0), tol = 0.1))
}
if (file.exists(out("product_randomization_pvalues.csv"))) {
  # The randomization is a seeded Monte Carlo; its exact p-values depend on the
  # draw stream, so the audit checks the qualitative result the paper reports:
  # V-Dem sits far in the placebo tail while CPI is only marginal.
  ri <- read_out("product_randomization_pvalues.csv")
  check("Randomization: V-Dem in tail, CPI marginal",
        ri |> filter(corruption_measure == "vdem_v2x_corr") |> pull(p_two_sided) < 0.05 &&
        ri |> filter(corruption_measure == "cpi_inverse") |> pull(p_two_sided) > 0.05)
}


# Write ledger and stop on failure ---------------------------------------------

audit <- list_rbind(ledger)
write_csv(audit, out("model_audit_checks.csv"))

for (i in seq_len(nrow(audit))) {
  cli_inform(rlang::set_names(audit$check[i], if (audit$status[i] == "PASS") "v" else "x"))
}
n_fail <- sum(audit$status == "FAIL")
if (n_fail > 0) {
  cli_abort("Audit failed: {n_fail} check(s) did not pass.")
}
cli_inform(c("v" = "Audit passed: {nrow(audit)} checks"))
if (length(skipped_checks) > 0) {
  cli_inform(c(
    "!" = "{length(skipped_checks)} check{?s} not run, because {?its/their} input{?s} {?was/were} not produced:",
    " " = "{skipped_checks}",
    "i" = "These come from steps 03/05, which rebuild product baskets and mirror
           statistics from raw BACI and Comtrade. Populate {.path replication/data/}
           and rerun to audit them."
  ))
}

# Record the estimation stack alongside the ledger so the deposit carries the
# exact versions the released numbers were produced with. Versions are looked
# up directly because each pipeline step runs in its own session.
pipeline_stack <- c(
  "dplyr", "tidyr", "readr", "stringr", "purrr", "tibble", "arrow", "haven",
  "here", "cli", "glue", "fixest", "ggplot2", "HonestDiD"
)
writeLines(
  c(
    R.version.string,
    paste0(
      pipeline_stack, " ",
      vapply(pipeline_stack, \(p) as.character(packageVersion(p)), character(1))
    )
  ),
  out("session_info.txt")
)
cli_inform(c("v" = "Session info written to {.file session_info.txt}"))
