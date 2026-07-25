# Product-randomization inference (Appendix D): 500 seeded draws that assign
# treated status to four random regular-plastic codes and re-estimate the
# triple against the general-waste control, building a null distribution for
# the HS 3915 headline. Sourced by 05. RI needs coefficients only, so the
# models are fit without the clustered covariance.

n_draws <- as.integer(Sys.getenv("RI_DRAWS", "500"))
ri_seed <- 20260705L

panel_flags <- read_parquet(
  out("plastic_corruption_panel.parquet"),
  col_select = c("imp", "exp", "corruption_measure", "weak_control_baseline")
)
panel_exporters <- sort(unique(panel_flags$exp))
balanced_importers <- sort(unique(panel_flags$imp))
weak_flags <- distinct(panel_flags, imp, corruption_measure, weak_control_baseline)
measures <- sort(unique(weak_flags$corruption_measure))

# Regular-plastic codes in screen (first-appearance) order; the seeded draws
# depend on this order, so it must not be sorted.
unit_values <- baci |>
  filter(year == 2014) |>
  summarise(unit_value = mean(value / quantity, na.rm = TRUE), .by = hs6)
threshold <- unit_values |> filter(hs6 %in% plastic_codes) |> pull(unit_value) |> max(na.rm = TRUE)
regular_codes <- unit_values |>
  filter(!hs6 %in% plastic_codes, !hs6 %in% general_waste_codes,
         str_sub(hs6, 1, 2) == "39", unit_value > threshold) |>
  pull(hs6)
stopifnot(setequal(regular_codes, regular_plastic_codes))

hs <- baci |> filter(hs6 %in% c(plastic_codes, regular_codes, general_waste_codes))
cells <- expand_grid(year = 2014:2020, exp = panel_exporters, imp = balanced_importers) |>
  filter(exp != imp)

aggregate_codes <- function(codes, class_label, treat_value) {
  agg <- hs |>
    filter(hs6 %in% codes) |>
    summarise(quantity = if (all(is.na(quantity))) NA_real_ else sum(quantity, na.rm = TRUE),
              observed = 1L, .by = c(exp, imp, year))
  cells |>
    left_join(agg, by = join_by(exp, imp, year)) |>
    mutate(quantity = if_else(is.na(observed), 0, quantity),
           class = class_label, treat = treat_value) |>
    select(-observed)
}

gw_cells <- aggregate_codes(general_waste_codes, "general_waste", 0L)

estimate_draw <- function(treated_codes) {
  d0 <- bind_rows(aggregate_codes(treated_codes, "treated", 1L), gw_cells) |>
    mutate(post_china_ban = as.integer(year >= 2017L)) |>
    filter(imp != "CHN")
  map_dbl(set_names(measures), \(m) {
    dm <- inner_join(d0, filter(weak_flags, corruption_measure == m) |> select(imp, weak_control_baseline),
                     by = "imp")
    fit <- tryCatch(
      fepois(quantity ~ treat * post_china_ban * weak_control_baseline |
               class^exp^imp + class^exp^year + imp^year, data = dm),
      error = \(e) NULL
    )
    if (is.null(fit)) NA_real_ else
      tryCatch(coef(fit)[["treat:post_china_ban:weak_control_baseline"]], error = \(e) NA_real_)
  })
}

set.seed(ri_seed)
draw_sets <- map(seq_len(n_draws), \(i) sample(regular_codes, 4L))
actual <- estimate_draw(plastic_codes)

ri <- bind_rows(
  tibble(draw = 0L, corruption_measure = measures, estimate = unname(actual[measures])),
  imap(draw_sets, \(codes, dr) {
    est <- estimate_draw(codes)
    if (dr %% 50L == 0L) cli::cli_inform("RI draw {dr}/{n_draws}")
    tibble(draw = dr, corruption_measure = measures, estimate = unname(est[measures]))
  }) |>
    list_rbind()
) |>
  arrange(draw, corruption_measure)
write_csv(ri, out("product_randomization_draws.csv"))

ri_summary <- map(set_names(measures), \(m) {
  draws_m <- ri |> filter(draw > 0, corruption_measure == m, !is.na(estimate))
  tibble(
    corruption_measure = m,
    n_draws = nrow(draws_m),
    p_two_sided = (1 + sum(abs(draws_m$estimate) >= abs(actual[m]))) / (1 + nrow(draws_m))
  )
}) |>
  list_rbind()
write_csv(ri_summary, out("product_randomization_pvalues.csv"))
cli::cli_inform(c("v" = "Randomization inference complete ({n_draws} draws)"))
