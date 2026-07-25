# Product-relabeling and control-basket checks (Appendices C, D, J). Rebuilds
# the superset-treated panel and the policy-screened control panel from BACI,
# and estimates the headline triple against four product comparisons with a
# PPML support audit. Sourced by 03.

waste_categories <- read_csv(
  here("replication", "data", "reference", "waste_categories.csv"),
  col_types = cols(code = col_character(), .default = col_guess())
)
plastic_crosswalk <- read_csv(
  here("replication", "data", "reference", "plastic_hs6_crosswalk.csv"),
  col_types = cols(hs6 = col_character(), .default = col_guess())
)
plastic_codes <- unique(plastic_crosswalk$hs6)
general_waste_codes <- waste_categories |>
  filter(class %in% c("paper", "metal", "rubber_leather")) |>
  pull(code) |>
  unique()

product_class_codes <- read_csv(out("product_class_codes.csv"),
                                col_types = cols(hs6 = col_character(), .default = col_guess()))
regular_plastic_codes <- product_class_codes |> filter(class == "plastic_regular") |> pull(hs6)

panel_exporters <- sort(unique(panel_full$exp))
balanced_importers <- sort(unique(panel_full$imp))
weak_flags <- panel_full |> distinct(imp, corruption_measure, weak_control_baseline)

baci <- load_baci_trade()


# Superset-treated relabeling test (Appendix D) --------------------------------

superset_trade <- baci |>
  mutate(class = case_when(
    str_sub(hs6, 1, 2) == "39" & !hs6 %in% regular_plastic_codes ~ "plastic_superset",
    hs6 %in% general_waste_codes ~ "general_waste",
    .default = NA_character_
  )) |>
  filter(!is.na(class)) |>
  mutate(treat = as.integer(class == "plastic_superset")) |>
  summarise(
    quantity = if (all(is.na(quantity))) NA_real_ else sum(quantity, na.rm = TRUE),
    .by = c(year, exp, imp, class, treat)
  ) |>
  mutate(observed = 1L) |>
  filter(imp %in% balanced_importers, exp %in% panel_exporters)

superset_grid <- expand_grid(
  year = 2014:2020, exp = panel_exporters, imp = balanced_importers,
  class = c("plastic_superset", "general_waste")
) |>
  filter(exp != imp) |>
  mutate(treat = as.integer(class == "plastic_superset"))

superset_panel <- superset_grid |>
  left_join(superset_trade, by = join_by(year, exp, imp, class, treat)) |>
  mutate(quantity = if_else(is.na(observed), 0, quantity),
         post_china_ban = as.integer(year >= 2017L))

superset_relabeling <- map(set_names(measures), \(m) {
  mp <- superset_panel |>
    inner_join(filter(weak_flags, corruption_measure == m) |> select(imp, weak_control_baseline),
               by = "imp") |>
    filter(!is.na(quantity))
  map(c("full_sample", "excluding_china"), \(samp) {
    d <- if (samp == "excluding_china") filter(mp, imp != "CHN") else mp
    fit_ppml(d, "treat * post_china_ban * weak_control_baseline") |>
      tidy_ppml() |>
      filter(term == triple_term) |>
      mutate(corruption_measure = m, specification = "superset_treated_vs_general_waste", sample = samp)
  }) |>
    list_rbind()
}) |>
  list_rbind()
write_csv(superset_relabeling, out("superset_relabeling.csv"))


# Product code audit and policy-screened basket (Appendix C, J) ----------------

notification_hs6 <- c("261900", "262099", "470790")
product_code_audit <- product_class_codes |>
  filter(class == "general_waste") |>
  distinct(hs6, description, source) |>
  mutate(
    china_2017_notification_category = hs6 %in% notification_hs6,
    policy_clean_included = !china_2017_notification_category,
    notification_basis = if_else(
      china_2017_notification_category,
      "Matches an HS6 aggregation of a tariff line named in WTO G/TBT/N/CHN/1211",
      "Not named in WTO G/TBT/N/CHN/1211"
    )
  ) |>
  arrange(desc(china_2017_notification_category), hs6)
write_csv(product_code_audit, out("product_code_policy_audit.csv"))

policy_clean_general_codes <- product_code_audit |> filter(policy_clean_included) |> pull(hs6)

policy_clean_trade <- baci |>
  filter(hs6 %in% c(plastic_codes, policy_clean_general_codes),
         exp %in% panel_exporters, imp %in% balanced_importers) |>
  mutate(class = if_else(hs6 %in% plastic_codes, "plastic_waste", "general_waste_policy_clean"),
         treat = as.integer(class == "plastic_waste")) |>
  summarise(
    quantity = if (all(is.na(quantity))) NA_real_ else sum(quantity, na.rm = TRUE),
    .by = c(year, exp, imp, class, treat)
  ) |>
  mutate(observed = 1L)

policy_clean_panel <- expand_grid(
  year = 2014:2020, exp = panel_exporters, imp = balanced_importers,
  class = c("plastic_waste", "general_waste_policy_clean")
) |>
  filter(exp != imp) |>
  mutate(treat = as.integer(class == "plastic_waste")) |>
  left_join(policy_clean_trade, by = join_by(year, exp, imp, class, treat)) |>
  mutate(
    quantity = if_else(is.na(observed), 0, quantity),
    post_china_ban = as.integer(year >= 2017L),
    treat_policy_event = as.integer(class == "plastic_waste" & imp == "MYS"),
    post_policy_event = as.integer(year >= 2018L),
    exp_imp = str_c(exp, imp, sep = "_")
  )


# Four product comparisons with PPML support audit -----------------------------

comparisons <- tribble(
  ~comparison,                  ~classes,                                          ~note,
  "pooled_original",            list(c("plastic_waste","plastic_regular","general_waste")), "Original pooled regular-plastic and general-waste controls",
  "regular_plastic_only",       list(c("plastic_waste","plastic_regular")),        "Original 2014 unit-value-screened chapter 39 controls",
  "general_waste_original",     list(c("plastic_waste","general_waste")),          "Original paper, metal, and rubber-leather waste controls",
  "general_waste_policy_clean", list(c("plastic_waste","general_waste_policy_clean")), "Original general-waste controls excluding HS6 261900, 262099, and 470790; exploratory WTO-notification screen"
)

one_comparison <- function(comparison, classes, note, measure) {
  classes <- classes[[1]]
  measure_panel <- if (comparison == "general_waste_policy_clean") {
    policy_clean_panel |>
      filter(imp != "CHN") |>
      left_join(filter(weak_flags, corruption_measure == measure) |> select(imp, weak_control_baseline),
                by = "imp") |>
      mutate(corruption_measure = measure)
  } else {
    panel_full |>
      filter(corruption_measure == measure, imp != "CHN", class %in% classes)
  }

  constructed <- measure_panel |>
    summarise(
      constructed_rows = n(), usable_quantity_rows = sum(!is.na(quantity)),
      positive_quantity_rows = sum(quantity > 0, na.rm = TRUE),
      constructed_exporters = n_distinct(exp), constructed_importers = n_distinct(imp),
      constructed_pairs = n_distinct(exp_imp),
      .by = c(corruption_measure, weak_control_baseline, class)
    )

  est <- measure_panel |> filter(!is.na(quantity))
  fit <- fit_ppml(est, main_rhs)
  retained <- est |>
    slice(fixest::obs(fit)) |>
    summarise(
      retained_rows = n(), retained_positive_rows = sum(quantity > 0),
      retained_exporters = n_distinct(exp), retained_importers = n_distinct(imp),
      retained_pairs = n_distinct(exp_imp),
      .by = c(corruption_measure, weak_control_baseline, class)
    )

  coef_row <- tidy_ppml(fit) |>
    filter(term == triple_term) |>
    mutate(corruption_measure = measure, comparison = comparison,
           control_definition = note, sample = "excluding_china")

  support <- constructed |>
    left_join(retained, by = join_by(corruption_measure, weak_control_baseline, class)) |>
    mutate(across(starts_with("retained_"), \(x) coalesce(x, 0)),
           comparison = comparison, control_definition = note, sample = "excluding_china")

  list(coef = coef_row, support = support)
}

results <- comparisons |>
  cross_join(tibble(measure = measures)) |>
  pmap(\(comparison, classes, note, measure) one_comparison(comparison, classes, note, measure))

product_control_baskets <- map(results, "coef") |>
  list_rbind() |>
  select(corruption_measure, comparison, control_definition, sample, term,
         estimate, std_error, z_value, p_value, observations) |>
  arrange(comparison, corruption_measure)
write_csv(product_control_baskets, out("product_control_baskets.csv"))

ppml_support <- map(results, "support") |>
  list_rbind() |>
  arrange(comparison, corruption_measure, weak_control_baseline, class)
write_csv(ppml_support, out("ppml_support.csv"))
