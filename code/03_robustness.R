# Robustness and appendix estimates: rival destination characteristics,
# destination concentration, leave-one-out, trade margins, product-relabeling
# and control-basket checks, mirror statistics, and China-excluded HonestDiD
# bounds. Reads the panel and the fitted models from 02.

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)
library(arrow)
library(haven)
library(here)
library(cli)
library(fixest)

here::i_am("replication/code/03_robustness.R")
source(here("replication", "code", "R", "functions.R"))
out <- \(file) here("replication", "output", file)

panel_full <- read_parquet(out("plastic_corruption_panel.parquet"))
panel <- panel_full |> filter(!is.na(quantity))
measures <- sort(unique(panel$corruption_measure))
policy_rhs <- "treat_policy_event * post_policy_event"
main_rhs <- str_glue("treat * post_china_ban * weak_control_baseline + {policy_rhs}")
triple_term <- "treat:post_china_ban:weak_control_baseline"


# Importer baseline attributes -------------------------------------------------

dist_cepii <- read_dta(here("replication", "data", "gravity", "dist_cepii.dta"))
eu15 <- c("AUT", "BEL", "DEU", "DNK", "ESP", "FIN", "FRA", "GBR",
          "GRC", "IRL", "ITA", "LUX", "NLD", "PRT", "SWE")
wgi_raw <- read_dta(here("replication", "data", "corruption", "wgidataset.dta"))

imp_gdp <- panel |>
  filter(corruption_measure == "cpi_inverse", year %in% 2014:2016, !is.na(imp_gdp_per_capita)) |>
  distinct(imp, year, imp_gdp_per_capita) |>
  summarise(gdppc_base = mean(imp_gdp_per_capita), .by = imp)

imp_epi <- panel |>
  filter(corruption_measure == "cpi_inverse", year == 2016, !is.na(imp_epi)) |>
  distinct(imp, epi_base = imp_epi)

dist_china <- dist_cepii |>
  filter(iso_o == "CHN") |>
  transmute(imp = iso_d, dist_china = as.numeric(dist))

dist_eu15 <- dist_cepii |>
  filter(iso_o %in% eu15) |>
  rename(imp = iso_d) |>
  summarise(dist_eu15 = min(dist, na.rm = TRUE), .by = imp)

wgi_2016 <- wgi_raw |>
  filter(year == 2016, indicator %in% c("ge", "rl")) |>
  transmute(imp = code, indicator, estimate) |>
  pivot_wider(names_from = indicator, values_from = estimate)

waste_intensity <- panel |>
  filter(corruption_measure == "cpi_inverse", treat == 1L, year %in% 2014:2016) |>
  summarise(pre_waste_imports = sum(quantity, na.rm = TRUE) / 3, .by = imp) |>
  mutate(high_waste_intensity = tercile_flag(pre_waste_imports, "top"))

importers <- tibble(imp = sort(unique(panel$imp))) |>
  left_join(imp_gdp, by = "imp") |>
  left_join(imp_epi, by = "imp") |>
  left_join(dist_china, by = "imp") |>
  left_join(dist_eu15, by = "imp") |>
  left_join(wgi_2016, by = "imp") |>
  left_join(waste_intensity |> select(imp, high_waste_intensity), by = "imp") |>
  mutate(
    low_income   = tercile_flag(gdppc_base, "bottom"),
    low_epi      = tercile_flag(epi_base, "bottom"),
    near_china   = as.integer(dist_china <= quantile(dist_china, 0.5, na.rm = TRUE, type = 7)),
    near_eu15    = as.integer(dist_eu15  <= quantile(dist_eu15,  0.5, na.rm = TRUE, type = 7)),
    low_ge       = tercile_flag(ge, "bottom"),
    low_rl       = tercile_flag(rl, "bottom"),
    high_waste_intensity = coalesce(high_waste_intensity, 0L)
  )

# Income-residualized corruption tercile (per measure).
resid_weak <- map(set_names(measures), \(m) {
  scores <- panel |>
    filter(corruption_measure == m, !is.na(corruption_z_baseline), !is.na(imp_gdp_per_capita)) |>
    distinct(imp, corruption_z_baseline) |>
    left_join(imp_gdp, by = "imp") |>
    filter(!is.na(gdppc_base))
  scores |>
    mutate(
      z_resid = resid(lm(corruption_z_baseline ~ log(gdppc_base), data = scores)),
      weak_resid = as.integer(z_resid > quantile(z_resid, 2 / 3, na.rm = TRUE, type = 7)),
      corruption_measure = m
    ) |>
    select(imp, corruption_measure, weak_resid)
}) |>
  list_rbind()

panel <- panel |>
  left_join(select(importers, imp, low_income, low_epi, near_china, near_eu15,
                   low_ge, low_rl, high_waste_intensity), by = "imp") |>
  left_join(resid_weak, by = join_by(imp, corruption_measure)) |>
  mutate(tur_policy = as.integer(imp == "TUR" & treat == 1L & year >= 2019L))

panel_x <- panel |> filter(imp != "CHN")


# Rival destination characteristics (Appendix H) -------------------------------

rival <- \(v) str_glue("treat * post_china_ban * weak_control_baseline + treat * post_china_ban * {v} + {policy_rhs}")

rival_characteristics <- bind_rows(
  rival_spec(filter(panel,   !is.na(low_income)), rival("low_income"),  measures, "joint_weak_plus_income", "full_sample"),
  rival_spec(filter(panel_x, !is.na(low_income)), rival("low_income"),  measures, "joint_weak_plus_income", "excluding_china"),
  rival_spec(filter(panel_x, !is.na(low_epi)),    rival("low_epi"),     measures, "joint_weak_plus_epi", "excluding_china"),
  rival_spec(filter(panel_x, !is.na(near_china)), rival("near_china"),  measures, "joint_weak_plus_nearchina", "excluding_china"),
  rival_spec(filter(panel_x, !is.na(low_income) & !is.na(near_china)),
             str_glue("treat * post_china_ban * weak_control_baseline + treat * post_china_ban * low_income + treat * post_china_ban * near_china + {policy_rhs}"),
             measures, "joint_weak_income_nearchina", "excluding_china"),
  rival_spec(filter(panel,   !is.na(weak_resid)), str_glue("treat * post_china_ban * weak_resid + {policy_rhs}"), measures, "income_residualized_tercile", "full_sample"),
  rival_spec(filter(panel_x, !is.na(weak_resid)), str_glue("treat * post_china_ban * weak_resid + {policy_rhs}"), measures, "income_residualized_tercile", "excluding_china"),
  rival_spec(filter(panel,   !imp %in% c("MYS","THA","VNM","IDN","IND")), main_rhs, measures, "drop_policy_responders", "full_sample"),
  rival_spec(filter(panel_x, !imp %in% c("MYS","THA","VNM","IDN","IND")), main_rhs, measures, "drop_policy_responders", "excluding_china")
)
# Rival-only specs (trade data common across measures; estimate on the cpi panel).
cpi_x <- filter(panel_x, corruption_measure == "cpi_inverse")
rival_only <- bind_rows(
  rival_spec(filter(panel, corruption_measure == "cpi_inverse", !is.na(low_income)),
             str_glue("treat * post_china_ban * low_income + {policy_rhs}"), "cpi_inverse", "income_only", "full_sample"),
  rival_spec(filter(cpi_x, !is.na(low_income)), str_glue("treat * post_china_ban * low_income + {policy_rhs}"), "cpi_inverse", "income_only", "excluding_china"),
  rival_spec(filter(cpi_x, !is.na(low_epi)),    str_glue("treat * post_china_ban * low_epi + {policy_rhs}"),    "cpi_inverse", "epi_only", "excluding_china"),
  rival_spec(filter(cpi_x, !is.na(near_china)), str_glue("treat * post_china_ban * near_china + {policy_rhs}"), "cpi_inverse", "nearchina_only", "excluding_china")
) |>
  mutate(corruption_measure = "trade_data_common")
write_csv(bind_rows(rival_characteristics, rival_only),
          out("rival_characteristics.csv"))

# State-capacity, EU15 geography, waste-intensity, and destination checks.
weak_wide <- panel |>
  distinct(imp, corruption_measure, weak_control_baseline) |>
  pivot_wider(names_from = corruption_measure, values_from = weak_control_baseline) |>
  mutate(consensus_weak = as.integer(cpi_inverse == 1 & wgi_cc_inverse == 1 & vdem_v2x_corr == 1))
panel <- panel |> left_join(select(weak_wide, imp, consensus_weak), by = "imp")
panel_x <- panel |> filter(imp != "CHN")
cpi_x <- filter(panel_x, corruption_measure == "cpi_inverse")

top5 <- list(
  cpi_inverse = c("UKR","MMR","UZB","LAO","MEX"),
  wgi_cc_inverse = c("UKR","PAK","UZB","LAO","MEX"),
  vdem_v2x_corr = c("TUR","UKR","PAK","UZB","PHL")
)

additional_specifications <- bind_rows(
  rival_spec(filter(panel_x, !is.na(near_eu15)), rival("near_eu15"), measures, "joint_weak_plus_near_eu15", "excluding_china"),
  rival_spec(filter(panel_x, !is.na(low_ge)),    rival("low_ge"),    measures, "joint_weak_plus_low_ge", "excluding_china"),
  rival_spec(filter(panel_x, !is.na(low_rl)),    rival("low_rl"),    measures, "joint_weak_plus_low_rl", "excluding_china"),
  rival_spec(filter(panel_x, imp != "TUR"),      main_rhs,           measures, "excl_turkey", "excluding_china"),
  rival_spec(panel,   str_glue("{main_rhs} + tur_policy"), measures, "turkey_policy_control", "full_sample"),
  rival_spec(panel_x, str_glue("{main_rhs} + tur_policy"), measures, "turkey_policy_control", "excluding_china"),
  rival_spec(filter(panel,   year <= 2019L), main_rhs, measures, "drop_2020", "full_sample"),
  rival_spec(filter(panel_x, year <= 2019L), main_rhs, measures, "drop_2020", "excluding_china"),
  rival_spec(filter(panel_x, class != "general_waste"), main_rhs, measures, "reg_plastic_only_control", "excluding_china"),
  rival_spec(filter(cpi_x, !is.na(near_eu15)), str_glue("treat * post_china_ban * near_eu15 + {policy_rhs}"), "cpi_inverse", "near_eu15_only", "excluding_china", keep = "near_eu15") |> mutate(corruption_measure = "trade_data_common"),
  rival_spec(filter(cpi_x, !is.na(low_ge)),    str_glue("treat * post_china_ban * low_ge + {policy_rhs}"),    "cpi_inverse", "low_ge_only", "excluding_china", keep = "low_ge") |> mutate(corruption_measure = "trade_data_common"),
  rival_spec(filter(cpi_x, !is.na(low_rl)),    str_glue("treat * post_china_ban * low_rl + {policy_rhs}"),    "cpi_inverse", "low_rl_only", "excluding_china", keep = "low_rl") |> mutate(corruption_measure = "trade_data_common"),
  rival_spec(filter(panel, corruption_measure == "cpi_inverse"), str_glue("treat * post_china_ban * consensus_weak + {policy_rhs}"), "cpi_inverse", "consensus_weak", "full_sample", keep = "consensus_weak") |> mutate(corruption_measure = "trade_data_common"),
  rival_spec(cpi_x, str_glue("treat * post_china_ban * consensus_weak + {policy_rhs}"), "cpi_inverse", "consensus_weak", "excluding_china", keep = "consensus_weak") |> mutate(corruption_measure = "trade_data_common"),
  imap(top5, \(drops, m) rival_spec(filter(panel_x, corruption_measure == m, !imp %in% drops), main_rhs, m, "drop_top5_contributors", "excluding_china")) |> list_rbind()
)
write_csv(additional_specifications, out("additional_specifications.csv"))


# Destination concentration and exporter origin (Appendix F) -------------------

destination_decomposition <- panel |>
  filter(treat == 1L, weak_control_baseline == 1L) |>
  mutate(period = if_else(year >= 2017L, "post", "pre")) |>
  summarise(q = sum(quantity), .by = c(corruption_measure, imp, period, year)) |>
  summarise(mean_annual_quantity = sum(q) / n_distinct(year), .by = c(corruption_measure, imp, period)) |>
  pivot_wider(names_from = period, values_from = mean_annual_quantity, values_fill = 0) |>
  mutate(delta = post - pre, total_delta = sum(delta), share_of_total_delta = delta / total_delta,
         .by = corruption_measure) |>
  arrange(corruption_measure, desc(delta)) |>
  mutate(rank = row_number(), .by = corruption_measure)
write_csv(destination_decomposition, out("destination_decomposition.csv"))

exporter_origin_decomposition <- map(c("TUR", "UKR"), \(country) {
  panel |>
    filter(corruption_measure == "cpi_inverse", imp == country, treat == 1L) |>
    mutate(eu_origin = if_else(exp %in% eu15, "EU15", "other"),
           period = if_else(year >= 2017L, "post", "pre")) |>
    summarise(q = sum(quantity, na.rm = TRUE), .by = c(eu_origin, period, year)) |>
    summarise(mean_annual = sum(q) / n_distinct(year), .by = c(eu_origin, period)) |>
    pivot_wider(names_from = period, values_from = mean_annual, values_fill = 0) |>
    mutate(delta = post - pre, importer = country, eu_share_of_delta = delta / sum(delta))
}) |>
  list_rbind()
write_csv(exporter_origin_decomposition, out("exporter_origin_decomposition.csv"))


# Leave-one-out contributor checks (Appendix F) --------------------------------

leave_one_out_contributors <- imap(top5, \(drops, m) {
  mpx <- filter(panel_x, corruption_measure == m)
  map(drops, \(ctry) {
    fit <- fit_ppml(filter(mpx, imp != ctry), main_rhs)
    tidy_ppml(fit) |>
      filter(term == triple_term) |>
      mutate(specification = "leave_one_out", sample = "excluding_china",
             corruption_measure = m, excluded_importer = ctry)
  }) |> list_rbind()
}) |>
  list_rbind()
write_csv(leave_one_out_contributors, out("leave_one_out_contributors.csv"))


# Trade margins and waste-intensity rival (Appendix G, H) ----------------------

pre_positive_pairs <- panel |>
  filter(corruption_measure == "cpi_inverse", treat == 1L, year %in% 2014:2016, quantity > 0) |>
  pull(exp_imp) |>
  unique()

trade_margins <- map(set_names(measures), \(m) {
  mpx <- filter(panel_x, corruption_measure == m) |> mutate(positive_trade = as.integer(quantity > 0))
  ext <- feols(as.formula(str_glue("positive_trade ~ {main_rhs} | {ppml_fe}")), vcov = ~ exp + imp, data = mpx)
  int <- fit_ppml(filter(mpx, exp_imp %in% pre_positive_pairs), main_rhs)
  bind_rows(
    tidy_ppml(ext) |> filter(term == triple_term) |> mutate(specification = "extensive_margin_lpm"),
    tidy_ppml(int) |> filter(term == triple_term) |> mutate(specification = "intensive_margin_pre_positive_pairs")
  ) |>
    mutate(sample = "excluding_china", corruption_measure = m)
}) |>
  list_rbind()
write_csv(trade_margins, out("trade_margins.csv"))

waste_intensity_rivals <- bind_rows(
  rival_spec(filter(cpi_x), str_glue("treat * post_china_ban * high_waste_intensity + {policy_rhs}"),
             "cpi_inverse", "waste_intensity_only", "excluding_china", keep = "high_waste_intensity") |>
    mutate(corruption_measure = "trade_data_common"),
  rival_spec(panel_x, rival("high_waste_intensity"), measures, "joint_weak_plus_waste_intensity", "excluding_china")
)
write_csv(waste_intensity_rivals, out("waste_intensity_rivals.csv"))


# Extensive-margin base rate and pre-period positive-trade rate ----------------

extensive_margin_base_rate <- panel |>
  filter(corruption_measure == "cpi_inverse", imp != "CHN", year <= 2016L) |>
  summarise(mean_positive = mean(quantity > 0), .by = class)
write_csv(extensive_margin_base_rate, out("extensive_margin_base_rate.csv"))


# Product placebo: regular plastic treated vs general waste (Appendix D) --------

pseudo_control_product_placebo <- map(set_names(measures), \(m) {
  d <- panel_x |>
    filter(corruption_measure == m, class %in% c("plastic_regular", "general_waste")) |>
    mutate(treat = as.integer(class == "plastic_regular"))
  fit_ppml(d, main_rhs) |>
    tidy_ppml() |>
    filter(term == triple_term) |>
    mutate(specification = "regular_plastic_pseudo_treated", sample = "excluding_china",
           corruption_measure = m)
}) |>
  list_rbind()
write_csv(pseudo_control_product_placebo, out("product_placebo.csv"))


# Superset-treated relabeling test (Appendix D) --------------------------------

source(here("replication", "code", "R", "robustness_relabeling.R"))


# Mirror statistics (Appendix I) -----------------------------------------------

source(here("replication", "code", "R", "robustness_mirror.R"))


# China-excluded HonestDiD bounds (Appendix E) ---------------------------------

source(here("replication", "code", "R", "robustness_honestdid.R"))

cli_inform(c("v" = "Robustness complete"))
