# China-excluded Rambachan-Roth HonestDiD relative-magnitude bounds, computed
# from the China-excluded event-study models fitted in 02. Sourced by 03.

if (!requireNamespace("HonestDiD", quietly = TRUE)) {
  cli::cli_abort("Missing package {.pkg HonestDiD}. Install it, then rerun.")
}

model_set <- readRDS(out("model_objects.rds"))
event_years <- c(2014L, 2015L, 2017L, 2018L, 2019L, 2020L)

honestdid_bounds_excluding_china <- imap(model_set, \(ms, m) {
  ev <- ms$event_ppml_excluding_china
  terms <- str_c("trip_", event_years)
  keep <- terms %in% names(coef(ev))
  terms <- terms[keep]
  kept_years <- event_years[keep]
  beta_hat <- unname(coef(ev)[terms])
  sigma_hat <- vcov(ev)[terms, terms, drop = FALSE]
  num_pre <- sum(kept_years < 2016L)
  post_years <- kept_years[kept_years > 2016L]

  map(seq_along(post_years), \(post_index) {
    HonestDiD::createSensitivityResults_relativeMagnitudes(
      betahat = beta_hat, sigma = sigma_hat,
      numPrePeriods = num_pre, numPostPeriods = length(post_years),
      Mbarvec = c(0, 0.5, 1, 1.5, 2),
      l_vec = as.numeric(seq_along(post_years) == post_index)
    ) |>
      as_tibble() |>
      mutate(corruption_measure = m, post_year = post_years[post_index])
  }) |>
    list_rbind()
}) |>
  list_rbind() |>
  rename(any_of(c(Mbar = "M"))) |>
  relocate(corruption_measure, post_year, Mbar)

write_csv(honestdid_bounds_excluding_china, out("honestdid_bounds_excluding_china.csv"))
