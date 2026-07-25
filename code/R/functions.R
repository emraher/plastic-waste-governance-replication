# Shared helpers for the plastic-waste / corruption replication pipeline.

library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(stringr)
library(purrr)
library(arrow)
library(here)
library(fixest)

# Read the chapter-filtered BACI HS12 bilateral trade with ISO3 codes and the
# standard value/quantity missingness rule. Shared by the panel build and the
# product-relabeling robustness (superset, policy-screened baskets).
load_baci_trade <- function(chapters = c("39", "47", "26", "40", "72", "74", "76")) {
  baci_dir <- here("replication", "data", "baci")
  country_codes <- read_csv(
    file.path(baci_dir, "country_codes_V202401.csv"),
    col_select = c(country_id = country_code, iso3 = country_iso3),
    show_col_types = FALSE
  )
  list.files(baci_dir, pattern = "^BACI_HS12_Y20(1[4-9]|20).*\\.csv$", full.names = TRUE) |>
    map(\(f) read_csv(
      f,
      col_select = c(year = t, exp_code = i, imp_code = j, hs6 = k, value = v, quantity = q),
      col_types = cols(t = col_integer(), i = col_integer(), j = col_integer(),
                       k = col_character(), v = col_double(), q = col_double())
    ) |>
      mutate(hs6 = str_pad(as.integer(hs6), 6, "left", "0")) |>
      filter(str_sub(hs6, 1, 2) %in% chapters)) |>
    list_rbind() |>
    left_join(country_codes, by = join_by(exp_code == country_id)) |>
    rename(exp = iso3) |>
    left_join(country_codes, by = join_by(imp_code == country_id)) |>
    rename(imp = iso3) |>
    filter(!is.na(exp), !is.na(imp), exp != imp) |>
    mutate(
      quantity = if_else(value > 0 & (is.na(quantity) | quantity == 0), NA_real_, quantity),
      value    = if_else(quantity > 0 & (is.na(value) | value == 0), NA_real_, value)
    )
}

# Triple-difference fixed effects, written as fixest interactions so the
# estimation never depends on precomputed string-key columns.
ppml_fe <- "class^exp^imp + class^exp^year + imp^year"

# Fit a PPML (Poisson) model with the standard fixed effects and two-way
# (exporter, importer) clustered standard errors.
fit_ppml <- function(data, rhs, lhs = "quantity") {
  fml <- as.formula(str_glue("{lhs} ~ {rhs} | {ppml_fe}"))
  fepois(fml, vcov = ~ exp + imp, data = data)
}

# Tidy a fixest fit into a tibble with snake_case columns. Renames the
# coefficient-table columns by position so it works for Poisson (z) and OLS (t).
tidy_ppml <- function(fit) {
  ct <- as_tibble(summary(fit)$coeftable, rownames = "term")
  names(ct)[2:5] <- c("estimate", "std_error", "z_value", "p_value")
  mutate(ct, observations = stats::nobs(fit))
}

# Symmetric normal confidence bands at the 90% and 95% levels.
add_conf_bands <- function(data) {
  data |>
    mutate(
      conf_low_95  = estimate - 1.96  * std_error,
      conf_high_95 = estimate + 1.96  * std_error,
      conf_low_90  = estimate - 1.645 * std_error,
      conf_high_90 = estimate + 1.645 * std_error
    )
}

# Fit one spec across all corruption measures and stack the tidy results.
# `cols` adds constant label columns; `keep` restricts to named terms.
estimate_across_measures <- function(data, rhs, measures, lhs = "quantity",
                                     keep = NULL, cols = list()) {
  map(set_names(measures), \(m) {
    fit <- fit_ppml(filter(data, corruption_measure == m), rhs, lhs)
    out <- tidy_ppml(fit) |> mutate(corruption_measure = m, !!!cols)
    if (!is.null(keep)) out <- filter(out, term %in% keep)
    out
  }) |>
    list_rbind()
}

# Fit one spec per measure and keep every term matching a pattern (used for
# the rival-characteristic specifications, which report both the weak-control
# and the rival triple interactions).
rival_spec <- function(data, rhs, measures, spec_label, sample_label,
                       keep = "treat:post_china_ban") {
  map(set_names(measures), \(m) {
    d <- filter(data, corruption_measure == m)
    fit <- tryCatch(fit_ppml(d, rhs), error = \(e) NULL)
    if (is.null(fit)) return(NULL)
    tidy_ppml(fit) |>
      filter(str_detect(term, fixed(keep))) |>
      mutate(specification = spec_label, sample = sample_label,
             corruption_measure = m)
  }) |>
    list_rbind()
}

# Bottom / top tercile indicator (type-7 quantiles, matching the pipeline).
tercile_flag <- function(x, side = c("bottom", "top")) {
  side <- match.arg(side)
  cut <- if (side == "bottom") {
    quantile(x, 1 / 3, na.rm = TRUE, type = 7)
  } else {
    quantile(x, 2 / 3, na.rm = TRUE, type = 7)
  }
  if (side == "bottom") as.integer(x <= cut) else as.integer(x > cut)
}

# Event-study plot rows for the identified triple interaction, with the
# reference year (2016) pinned to zero.
event_plot_data <- function(model, measure) {
  cf <- coef(model)
  vc <- diag(vcov(model))
  terms <- str_c("trip_", c(2014L, 2015L, 2017L, 2018L, 2019L, 2020L))
  terms <- terms[terms %in% names(cf)]
  bind_rows(
    tibble(
      corruption_measure = measure,
      year = as.integer(str_remove(terms, "trip_")),
      estimate = unname(cf[terms]),
      std_error = sqrt(unname(vc[terms]))
    ),
    tibble(corruption_measure = measure, year = 2016L, estimate = 0, std_error = 0)
  ) |>
    mutate(series = "High-corruption differential")
}

# Joint Wald test on the 2015/2016 pre-period leads of a 2014-referenced model.
pretrend_wald <- function(model_2014, measure) {
  pattern <- "^trip_201[56]$"
  terms <- str_subset(names(coef(model_2014)), pattern)
  w <- fixest::wald(model_2014, keep = pattern, print = FALSE)
  tibble(
    corruption_measure = measure,
    n_terms = length(terms),
    f_stat = w$stat,
    p_value = w$p
  )
}
