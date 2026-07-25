# Main PPML triple-difference estimates, event study, pre-trend tests, and the
# robustness set (corruption thresholds, value outcome, control choices,
# timing). Saves fitted models for the downstream sensitivity scripts.

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)
library(arrow)
library(here)
library(cli)

here::i_am("replication/code/02_estimate_main.R")
source(here("replication", "code", "R", "functions.R"))

out <- \(file) here("replication", "output", file)


# Load panel -------------------------------------------------------------------

panel_full <- read_parquet(out("plastic_corruption_panel.parquet"))
measures <- sort(unique(panel_full$corruption_measure))

# Value sample is carved out before the quantity filter so positive-value cells
# with missing quantity stay in the value robustness.
panel_value <- panel_full |> filter(!is.na(value))
panel <- panel_full |> filter(!is.na(quantity))

# Event-study lead-lag dummies (explicit basis; the lower-order interactions are
# absorbed by the fixed effects). tpe_ppe is the systematic policy-event control.
panel <- reduce(
  2014:2020,
  \(df, y) mutate(df, "trip_{y}" := treat * as.integer(year == y) * weak_control_baseline),
  .init = panel
) |>
  mutate(tpe_ppe = treat_policy_event * post_policy_event)

policy_rhs <- "treat_policy_event * post_policy_event"
main_rhs   <- str_glue("treat * post_china_ban * weak_control_baseline + {policy_rhs}")
event_rhs_2016 <- str_c(c(str_c("trip_", setdiff(2014:2020, 2016)), "tpe_ppe"), collapse = " + ")
event_rhs_2014 <- str_c(c(str_c("trip_", setdiff(2014:2020, 2014)), "tpe_ppe"), collapse = " + ")


# Fit the core models per measure (full sample and China-excluded) -------------

model_set <- map(set_names(measures), \(m) {
  d  <- filter(panel, corruption_measure == m)
  dx <- filter(d, imp != "CHN")
  list(
    main_ppml                       = fit_ppml(d,  main_rhs),
    main_ppml_excluding_china       = fit_ppml(dx, main_rhs),
    event_ppml                      = fit_ppml(d,  event_rhs_2016),
    event_ppml_2014                 = fit_ppml(d,  event_rhs_2014),
    event_ppml_excluding_china      = fit_ppml(dx, event_rhs_2016),
    event_ppml_2014_excluding_china = fit_ppml(dx, event_rhs_2014)
  )
})

pick <- \(slot) map(model_set, slot)


# Main and event coefficient tables --------------------------------------------

main_output <- imap(pick("main_ppml"),
                    \(fit, m) tidy_ppml(fit) |> mutate(corruption_measure = m)) |>
  list_rbind()

robustness_excluding_china <- imap(
  pick("main_ppml_excluding_china"),
  \(fit, m) tidy_ppml(fit) |>
    mutate(corruption_measure = m, specification = "exclude_china_importer")
) |>
  list_rbind()

event_output <- imap(
  pick("event_ppml"),
  \(fit, m) tidy_ppml(fit) |> mutate(corruption_measure = m) |> add_conf_bands()
) |>
  list_rbind()

event_output_excluding_china <- imap(
  pick("event_ppml_excluding_china"),
  \(fit, m) tidy_ppml(fit) |>
    mutate(corruption_measure = m, sample = "excluding_china") |>
    add_conf_bands()
) |>
  list_rbind()


# Event-study plot data and pre-trend tests ------------------------------------

plot_data <- imap(pick("event_ppml"), event_plot_data) |>
  list_rbind() |>
  add_conf_bands() |>
  arrange(corruption_measure, series, year)

plot_data_excluding_china <- imap(pick("event_ppml_excluding_china"), event_plot_data) |>
  list_rbind() |>
  mutate(sample = "excluding_china") |>
  add_conf_bands() |>
  arrange(corruption_measure, series, year)

pretrend_tests <- imap(pick("event_ppml_2014"), pretrend_wald) |>
  list_rbind()

pretrend_tests_excluding_china <- imap(pick("event_ppml_2014_excluding_china"), pretrend_wald) |>
  list_rbind() |>
  mutate(sample = "excluding_china", .after = corruption_measure)


# Robustness: continuous score and pre-period baseline -------------------------

robustness_corruption <- bind_rows(
  estimate_across_measures(
    filter(panel, !is.na(corruption_z_baseline)),
    str_glue("treat * post_china_ban * corruption_z_baseline + {policy_rhs}"),
    measures, cols = list(robustness = "continuous_2016")
  ),
  estimate_across_measures(
    filter(panel, !is.na(weak_control_baseline_pre)),
    str_glue("treat * post_china_ban * weak_control_baseline_pre + {policy_rhs}"),
    measures, cols = list(robustness = "tercile_2010_2013")
  )
)


# Robustness: corruption thresholds and tercile bins ---------------------------

threshold_specs <- tribble(
  ~specification,  ~variable_name,
  "median_split",  "weak_control_median",
  "top_tercile",   "weak_control_baseline",
  "top_quartile",  "weak_control_top_quartile",
  "top_quintile",  "weak_control_top_quintile"
)

robustness_threshold <- bind_rows(
  pmap(threshold_specs, \(specification, variable_name) {
    estimate_across_measures(
      panel,
      str_glue("treat * post_china_ban * {variable_name} + {policy_rhs}"),
      measures,
      keep = str_glue("treat:post_china_ban:{variable_name}"),
      cols = list(specification = specification, threshold_variable = variable_name)
    )
  }) |> list_rbind(),
  estimate_across_measures(
    panel,
    str_glue(
      "treat * post_china_ban + treat:post_china_ban:middle_control_baseline + ",
      "treat:post_china_ban:high_control_baseline + {policy_rhs}"
    ),
    measures,
    keep = c("treat:post_china_ban:middle_control_baseline",
             "treat:post_china_ban:high_control_baseline")
  ) |>
    mutate(
      specification = if_else(
        term == "treat:post_china_ban:middle_control_baseline",
        "middle_tercile_vs_low", "high_tercile_vs_low"
      ),
      threshold_variable = str_remove(term, "treat:post_china_ban:")
    )
)


# Robustness: value outcome, control choices, and timing -----------------------

robustness_value <- estimate_across_measures(
  panel_value,
  str_glue("treat * post_china_ban * weak_control_baseline + {policy_rhs}"),
  measures, lhs = "value", cols = list(outcome = "value")
)

robustness_no_ban <- estimate_across_measures(
  panel, "treat * post_china_ban * weak_control_baseline",
  measures, cols = list(specification = "no_recipient_ban_controls")
)

legacy_policy_rhs <- str_c(
  "treat * post_china_ban * weak_control_baseline",
  "treat_vnm * post_vnm", "treat_idn_pl * post_idn_pl", "treat_idn_pa * post_idn_pa",
  "treat_mys * post_mys", "treat_tha * post_tha",
  "treat_ind_pl * post_ind_pl", "treat_ind_pa * post_ind_pa",
  sep = " + "
)
robustness_legacy_policy <- estimate_across_measures(
  panel, legacy_policy_rhs, measures,
  cols = list(specification = "legacy_destination_policy_controls")
)

robustness_alt_control <- estimate_across_measures(
  filter(panel, class %in% c("plastic_waste", "general_waste")),
  str_glue("treat * post_china_ban * weak_control_baseline + {policy_rhs}"),
  measures, cols = list(control_group = "general_waste")
)

timing_rhs <- str_glue("treat * post_china_ban_2018 * weak_control_baseline + {policy_rhs}")
timing_term <- "treat:post_china_ban_2018:weak_control_baseline"
robustness_timing <- bind_rows(
  estimate_across_measures(panel, timing_rhs, measures, keep = timing_term,
                           cols = list(specification = "post_2018_all_years")),
  estimate_across_measures(filter(panel, year != 2017), timing_rhs, measures,
                           keep = timing_term,
                           cols = list(specification = "drop_2017_post_2018"))
)


# Presentation table (multipliers) ---------------------------------------------

main_presentation <- main_output |>
  filter(term %in% c("treat:post_china_ban:weak_control_baseline",
                     "treat_policy_event:post_policy_event")) |>
  mutate(
    term_label = case_when(
      term == "treat:post_china_ban:weak_control_baseline" ~
        "Plastic waste x post-2017 x weak-control importer",
      term == "treat_policy_event:post_policy_event" ~
        "Plastic waste importer policy event",
      .default = term
    ),
    multiplier = exp(estimate),
    percent_change = 100 * (exp(estimate) - 1)
  )


# Save -------------------------------------------------------------------------

write_csv(main_output,                     out("main_ppml_coefficients.csv"))
write_csv(robustness_excluding_china,      out("main_ppml_coefficients_excluding_china.csv"))
write_csv(main_presentation,               out("main_ppml_coefficients_presentation.csv"))
write_csv(event_output,                    out("event_study_coefficients.csv"))
write_csv(event_output_excluding_china,    out("event_study_coefficients_excluding_china.csv"))
write_csv(plot_data,                       out("event_study_plot_data.csv"))
write_csv(plot_data_excluding_china,       out("event_study_plot_data_excluding_china.csv"))
write_csv(pretrend_tests,                  out("pretrend_tests.csv"))
write_csv(pretrend_tests_excluding_china,  out("pretrend_tests_excluding_china.csv"))
write_csv(robustness_corruption,           out("robustness_corruption.csv"))
write_csv(robustness_threshold,            out("robustness_corruption_thresholds.csv"))
write_csv(robustness_value,                out("robustness_value.csv"))
write_csv(robustness_no_ban,               out("robustness_no_recipient_ban_controls.csv"))
write_csv(robustness_legacy_policy,        out("robustness_legacy_destination_policy_controls.csv"))
write_csv(robustness_alt_control,          out("robustness_alt_control.csv"))
write_csv(robustness_excluding_china,      out("robustness_excluding_china.csv"))
write_csv(robustness_timing,               out("robustness_timing.csv"))
saveRDS(model_set, out("model_objects.rds"))

cli_inform(c("v" = "Estimation complete: {length(measures)} measures, models saved"))
