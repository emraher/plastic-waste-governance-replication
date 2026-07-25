# Build the bilateral plastic-waste / corruption panel.
#
# Reads BACI HS12 trade, product crosswalks, country-year controls, and three
# corruption measures; screens the regular-plastic comparison basket on 2014
# unit values; completes the exporter-importer-class-year panel with structural
# zeros; and attaches pre-shock corruption terciles fixed at 2016.
#
# Output: analysis panel (parquet) + product-class code list and panel summary.

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)
library(arrow)
library(haven)
library(here)
library(cli)

here::i_am("replication/code/01_build_panel.R")


# Paths ------------------------------------------------------------------------

dp <- \(...) here("replication", "data", ...)

paths <- list(
  baci_dir       = dp("baci"),
  country_codes  = dp("baci", "country_codes_V202401.csv"),
  waste_cats     = dp("reference", "waste_categories.csv"),
  gdppc          = dp("controls", "gdp_per_capita.csv"),
  epi            = dp("controls", "epi.csv"),
  ban            = dp("membership", "panel_ban.csv"),
  eu             = dp("membership", "panel_eu.csv"),
  oecd           = dp("membership", "panel_oecd.csv"),
  crosswalk      = dp("reference", "plastic_hs6_crosswalk.csv"),
  cpi            = dp("corruption", "cpi.csv"),
  wgi            = dp("corruption", "wgidataset.dta"),
  vdem           = dp("corruption", "vdem.parquet"),
  policy_events  = dp("policy", "plastic_policy_events.csv")
)

out_dir <- here("replication", "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out <- \(file) file.path(out_dir, file)

keep_chapters <- c("39", "47", "26", "40", "72", "74", "76")


# Reference tables -------------------------------------------------------------

country_codes <- read_csv(
  paths$country_codes,
  col_select = c(country_id = country_code, iso3 = country_iso3),
  show_col_types = FALSE
)

waste_categories <- read_csv(
  paths$waste_cats, col_types = cols(code = col_character(), .default = col_guess())
)
plastic_crosswalk <- read_csv(
  paths$crosswalk, col_types = cols(hs6 = col_character(), .default = col_guess())
)

plastic_codes <- unique(plastic_crosswalk$hs6)
general_waste_codes <- waste_categories |>
  filter(class %in% c("paper", "metal", "rubber_leather")) |>
  pull(code) |>
  unique()


# BACI trade -------------------------------------------------------------------

baci_files <- list.files(
  paths$baci_dir,
  pattern = "^BACI_HS12_Y20(14|15|16|17|18|19|20).*\\.csv$",
  full.names = TRUE
)
if (length(baci_files) == 0) {
  cli_abort("No BACI files for 2014-2020 in {.path {paths$baci_dir}}.")
}

read_baci <- function(file) {
  read_csv(
    file,
    col_select = c(year = t, exp_code = i, imp_code = j, hs6 = k,
                   value = v, quantity = q),
    col_types = cols(
      t = col_integer(), i = col_integer(), j = col_integer(),
      k = col_character(), v = col_double(), q = col_double()
    )
  ) |>
    mutate(hs6 = str_pad(as.integer(hs6), width = 6, side = "left", pad = "0")) |>
    filter(str_sub(hs6, 1, 2) %in% keep_chapters)
}

trade <- baci_files |>
  map(read_baci) |>
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


# Product-class screen (2014 unit values) --------------------------------------

unit_values <- trade |>
  filter(year == 2014) |>
  summarise(unit_value = mean(value / quantity, na.rm = TRUE), .by = hs6)

treated_threshold <- unit_values |>
  filter(hs6 %in% plastic_codes) |>
  pull(unit_value) |>
  max(na.rm = TRUE)

plastic_regular_codes <- unit_values |>
  filter(
    !hs6 %in% plastic_codes,
    !hs6 %in% general_waste_codes,
    str_sub(hs6, 1, 2) == "39",
    unit_value > treated_threshold
  ) |>
  pull(hs6)

trade <- trade |>
  mutate(class = case_when(
    hs6 %in% plastic_codes         ~ "plastic_waste",
    hs6 %in% plastic_regular_codes ~ "plastic_regular",
    hs6 %in% general_waste_codes   ~ "general_waste",
    .default = NA_character_
  )) |>
  filter(!is.na(class)) |>
  mutate(treat = as.integer(class == "plastic_waste"))


# Canonical product-class code list --------------------------------------------
# Single source of truth for the estimation baskets. The chapter filter means
# only general-waste codes in the kept chapters enter the realized basket.

general_waste_codes_kept <- general_waste_codes[
  str_sub(general_waste_codes, 1, 2) %in% keep_chapters
]

product_class_codes <- bind_rows(
  plastic_crosswalk |>
    transmute(hs6, class = "plastic_waste", material = NA_character_, description, source),
  tibble(
    hs6 = sort(plastic_regular_codes),
    class = "plastic_regular",
    material = NA_character_,
    description = "Non-waste chapter 39 code with 2014 mean bilateral unit value above the treated maximum",
    source = "Screened in replication/code/01_build_panel.R from BACI 2014 unit values"
  ),
  waste_categories |>
    filter(code %in% general_waste_codes_kept) |>
    transmute(
      hs6 = code, class = "general_waste", material = class, description,
      source = "Higashida-Managi waste_categories.csv restricted to chapters 26, 40, 47, 72, 74, 76"
    )
)

write_csv(product_class_codes, out("product_class_codes.csv"))
cli_inform(c("i" = paste(
  "Product classes: {sum(product_class_codes$class == 'plastic_waste')} treated,",
  "{sum(product_class_codes$class == 'plastic_regular')} regular-plastic,",
  "{sum(product_class_codes$class == 'general_waste')} general-waste HS6 codes"
)))


# Aggregate to exporter-importer-class-year and complete with zeros ------------

trade <- trade |>
  summarise(
    value    = if (all(is.na(value))) NA_real_ else sum(value, na.rm = TRUE),
    quantity = if (all(is.na(quantity))) NA_real_ else sum(quantity, na.rm = TRUE),
    .by = c(year, exp, imp, class, treat)
  ) |>
  mutate(observed_trade_cell = 1L)

panel_index <- expand_grid(
  year  = sort(unique(trade$year)),
  exp   = sort(unique(trade$exp)),
  imp   = sort(unique(trade$imp)),
  class = c("plastic_waste", "plastic_regular", "general_waste")
) |>
  filter(exp != imp) |>
  mutate(treat = as.integer(class == "plastic_waste"))

trade <- panel_index |>
  left_join(trade, by = join_by(year, exp, imp, class, treat)) |>
  # Fill structural zeros only for newly completed (unobserved) cells. Observed
  # cells with positive value but missing quantity keep quantity = NA.
  mutate(
    unobserved          = is.na(observed_trade_cell),
    value               = if_else(unobserved, 0, value),
    quantity            = if_else(unobserved, 0, quantity),
    observed_trade_cell = coalesce(observed_trade_cell, 0L)
  ) |>
  select(-unobserved)


# Country-year controls --------------------------------------------------------

gdppc <- read_csv(paths$gdppc, name_repair = "minimal", show_col_types = FALSE) |>
  rename(country_name = `Country Name`, iso3 = `Country Code`) |>
  select(country_name, iso3, `2014`:`2020`) |>
  pivot_longer(`2014`:`2020`, names_to = "year", values_to = "gdp_per_capita") |>
  mutate(year = as.integer(year), gdp_per_capita = as.numeric(gdp_per_capita)) |>
  filter(!is.na(iso3))

trade <- trade |>
  left_join(
    gdppc |> select(exp = iso3, year, exp_gdp_per_capita = gdp_per_capita),
    by = join_by(exp, year)
  ) |>
  left_join(
    gdppc |> select(imp = iso3, year, imp_gdp_per_capita = gdp_per_capita),
    by = join_by(imp, year)
  )

epi <- read_csv(paths$epi, show_col_types = FALSE)
trade <- trade |>
  left_join(epi |> select(exp = iso3, year, exp_epi = EPI), by = join_by(exp, year)) |>
  left_join(epi |> select(imp = iso3, year, imp_epi = EPI), by = join_by(imp, year))


# Basel-ban pair structure -----------------------------------------------------

read_status <- function(path, value_col) {
  read_csv(path, show_col_types = FALSE) |>
    transmute(iso3 = Country, year = Year, status = .data[[value_col]])
}

ban  <- read_status(paths$ban,  "bana")
eu   <- read_status(paths$eu,   "eu")
oecd <- read_status(paths$oecd, "oecd")

trade <- trade |>
  left_join(ban  |> select(exp = iso3, year, exp_ban  = status), by = join_by(exp, year)) |>
  left_join(ban  |> select(imp = iso3, year, imp_ban  = status), by = join_by(imp, year)) |>
  left_join(eu   |> select(exp = iso3, year, exp_eu   = status), by = join_by(exp, year)) |>
  left_join(eu   |> select(imp = iso3, year, imp_eu   = status), by = join_by(imp, year)) |>
  left_join(oecd |> select(exp = iso3, year, exp_oecd = status), by = join_by(exp, year)) |>
  left_join(oecd |> select(imp = iso3, year, imp_oecd = status), by = join_by(imp, year)) |>
  mutate(across(c(exp_ban, imp_ban, exp_eu, imp_eu, exp_oecd, imp_oecd),
                \(x) coalesce(x, 0L))) |>
  mutate(ban_pair = as.integer(
    (exp_ban == 1 & (exp_eu == 1 | exp_oecd == 1) & (imp_eu == 0 & imp_oecd == 0)) |
    (imp_ban == 1 & (imp_eu == 0 & imp_oecd == 0) & (exp_eu == 1 | exp_oecd == 1))
  )) |>
  select(-exp_eu, -imp_eu, -exp_oecd, -imp_oecd)


# Treatment timing and destination-specific policy controls --------------------

trade <- trade |>
  mutate(
    post_china_ban      = as.integer(year >= 2017),
    post_china_ban_2018 = as.integer(year >= 2018),
    treat_vnm    = as.integer(imp == "VNM" & class == "plastic_waste"),
    post_vnm     = as.integer(year >= 2018),
    treat_idn_pl = as.integer(imp == "IDN" & class == "plastic_waste"),
    post_idn_pl  = as.integer(year >= 2018),
    treat_idn_pa = as.integer(imp == "IDN" & class == "plastic_regular"),
    post_idn_pa  = as.integer(year >= 2019),
    treat_mys    = as.integer(imp == "MYS" & class == "plastic_waste"),
    post_mys     = as.integer(year >= 2018),
    treat_tha    = as.integer(imp == "THA" & class == "plastic_waste"),
    post_tha     = as.integer(year >= 2018),
    treat_ind_pl = as.integer(imp == "IND" & class == "plastic_waste"),
    post_ind_pl  = as.integer(year >= 2019),
    treat_ind_pa = as.integer(imp == "IND" & class == "plastic_regular"),
    post_ind_pa  = as.integer(year >= 2020),
    treat_policy_event = 0L,
    post_policy_event  = 0L
  )

policy_events_used <- tibble(
  iso3 = character(), material = character(),
  policy_name = character(), start_year = integer()
)

if (file.exists(paths$policy_events)) {
  policy_events <- read_csv(paths$policy_events, show_col_types = FALSE) |>
    mutate(across(c(iso3, material, policy_name), as.character),
           start_year = as.integer(start_year)) |>
    filter(material %in% c("plastic", "plastic_waste"), !is.na(iso3), !is.na(start_year)) |>
    distinct()

  if (nrow(policy_events) > 0) {
    policy_events_used <- policy_events |>
      summarise(
        material    = str_c(sort(unique(material)), collapse = " | "),
        policy_name = str_c(sort(unique(policy_name)), collapse = " | "),
        start_year  = min(start_year, na.rm = TRUE),
        .by = iso3
      )

    trade <- trade |>
      left_join(
        policy_events_used |> select(imp = iso3, policy_start_year = start_year),
        by = join_by(imp)
      ) |>
      mutate(
        treat_policy_event = as.integer(imp %in% policy_events_used$iso3 &
                                          class == "plastic_waste"),
        post_policy_event  = as.integer(!is.na(policy_start_year) &
                                          year >= policy_start_year)
      ) |>
      select(-policy_start_year)
  }
}

write_csv(policy_events_used, out("policy_event_controls_used.csv"))


# Corruption measures ----------------------------------------------------------

cpi_full  <- read_csv(paths$cpi, show_col_types = FALSE)
wgi_full  <- read_dta(paths$wgi)
vdem_full <- read_parquet(paths$vdem)

corruption <- bind_rows(
  cpi_full |>
    filter(year >= 2014, year <= 2020) |>
    transmute(imp = iso, year = as.integer(year),
              corruption_measure = "cpi_inverse",
              corruption_score = as.numeric(100 - cpi_score)),
  wgi_full |>
    filter(indicator == "cc", year >= 2014, year <= 2020) |>
    transmute(imp = code, year = as.integer(year),
              corruption_measure = "wgi_cc_inverse",
              corruption_score = as.numeric(-estimate)),
  vdem_full |>
    filter(year >= 2014, year <= 2020) |>
    transmute(imp = country_text_id, year = as.integer(year),
              corruption_measure = "vdem_v2x_corr",
              corruption_score = as.numeric(v2x_corr))
) |>
  filter(!is.na(imp), !is.na(year), !is.na(corruption_score)) |>
  distinct() |>
  mutate(corruption_z = as.numeric(scale(corruption_score)), .by = corruption_measure)

# Importers with complete 2014-2020 coverage on all three measures.
balanced_importers <- corruption |>
  summarise(years_observed = n_distinct(year), .by = c(imp, corruption_measure)) |>
  pivot_wider(names_from = corruption_measure, values_from = years_observed,
              values_fill = 0L) |>
  filter(cpi_inverse == 7, wgi_cc_inverse == 7, vdem_v2x_corr == 7) |>
  pull(imp)

if (length(balanced_importers) == 0) {
  cli_abort("No importers have balanced 2014-2020 coverage across all three measures.")
}
cli_inform(c("i" = "Balanced importer count: {length(balanced_importers)}"))

corruption <- corruption |> filter(imp %in% balanced_importers)

# Pre-shock (2016) terciles and alternative cutoffs.
baseline_corruption <- corruption |>
  filter(year == 2016) |>
  distinct(imp, corruption_measure, corruption_score) |>
  mutate(
    lower_cutoff        = quantile(corruption_score, 1 / 3, na.rm = TRUE, type = 7),
    upper_cutoff        = quantile(corruption_score, 2 / 3, na.rm = TRUE, type = 7),
    median_cutoff       = quantile(corruption_score, 1 / 2, na.rm = TRUE, type = 7),
    top_quartile_cutoff = quantile(corruption_score, 3 / 4, na.rm = TRUE, type = 7),
    top_quintile_cutoff = quantile(corruption_score, 4 / 5, na.rm = TRUE, type = 7),
    .by = corruption_measure
  ) |>
  mutate(
    corruption_tercile = case_when(
      corruption_score <= lower_cutoff ~ "low",
      corruption_score <= upper_cutoff ~ "middle",
      .default = "high"
    ),
    weak_control_baseline      = as.integer(corruption_tercile == "high"),
    middle_control_baseline    = as.integer(corruption_tercile == "middle"),
    high_control_baseline      = as.integer(corruption_tercile == "high"),
    weak_control_median        = as.integer(corruption_score > median_cutoff),
    weak_control_top_quartile  = as.integer(corruption_score > top_quartile_cutoff),
    weak_control_top_quintile  = as.integer(corruption_score > top_quintile_cutoff)
  ) |>
  select(imp, corruption_measure, corruption_tercile,
         weak_control_baseline, middle_control_baseline, high_control_baseline,
         weak_control_median, weak_control_top_quartile, weak_control_top_quintile)

# Pre-period (2010-2013) mean classification.
pre_period_score <- function(data, imp_col, score_expr, measure) {
  data |>
    filter(year >= 2010, year <= 2013) |>
    summarise(score_pre = mean({{ score_expr }}, na.rm = TRUE),
              .by = {{ imp_col }}) |>
    rename(imp = {{ imp_col }}) |>
    mutate(corruption_measure = measure)
}

baseline_pre <- bind_rows(
  pre_period_score(cpi_full,  iso,             as.numeric(100 - cpi_score), "cpi_inverse"),
  pre_period_score(filter(wgi_full, indicator == "cc"), code, as.numeric(-estimate), "wgi_cc_inverse"),
  pre_period_score(vdem_full, country_text_id, as.numeric(v2x_corr),        "vdem_v2x_corr")
) |>
  filter(imp %in% balanced_importers) |>
  mutate(
    weak_control_baseline_pre = as.integer(
      score_pre > quantile(score_pre, 2 / 3, na.rm = TRUE, type = 7)
    ),
    .by = corruption_measure
  ) |>
  select(imp, corruption_measure, weak_control_baseline_pre)

baseline_z <- corruption |>
  filter(year == 2016) |>
  select(imp, corruption_measure, corruption_z_baseline = corruption_z)


# Assemble and save ------------------------------------------------------------

panel <- trade |>
  filter(imp %in% balanced_importers) |>
  inner_join(corruption, by = join_by(imp, year), relationship = "many-to-many") |>
  left_join(baseline_corruption, by = join_by(imp, corruption_measure)) |>
  mutate(weak_control_baseline = coalesce(weak_control_baseline, 0L)) |>
  left_join(baseline_pre, by = join_by(imp, corruption_measure)) |>
  mutate(weak_control_baseline_pre = coalesce(weak_control_baseline_pre, 0L)) |>
  left_join(baseline_z, by = join_by(imp, corruption_measure)) |>
  mutate(
    exp_imp       = str_c(exp, imp, sep = "_"),
    class_exp_imp = str_c(class, exp, imp, sep = "_"),
    class_exp_year = str_c(class, exp, year, sep = "_"),
    class_imp_year = str_c(class, imp, year, sep = "_"),
    imp_year       = str_c(imp, year, sep = "_")
  )

write_parquet(panel, out("plastic_corruption_panel.parquet"))

panel_summary <- panel |>
  summarise(
    bilateral_pairs      = n_distinct(exp_imp),
    importer_count       = n_distinct(imp),
    total_quantity       = sum(quantity, na.rm = TRUE),
    total_value          = sum(value, na.rm = TRUE),
    positive_trade_pairs = sum(quantity > 0, na.rm = TRUE),
    .by = c(corruption_measure, year, class)
  ) |>
  arrange(corruption_measure, year, class)

write_csv(panel_summary, out("panel_summary_by_year.csv"))

cli_inform(c("v" = "Panel written: {nrow(panel)} rows, {ncol(panel)} columns"))
