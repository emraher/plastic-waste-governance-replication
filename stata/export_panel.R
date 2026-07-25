# Export the estimation panel to a Stata dataset for the cross-checks in
# estimation.do and robustness.do. Adds the panel's threshold/continuous
# classifications and the importer attributes that 03_robustness.R builds
# (income, EPI, proximity, EU15, state capacity, waste intensity), so the
# robustness specifications can be reproduced in Stata from one .dta.

library(dplyr)
library(tidyr)
library(purrr)
library(arrow)
library(haven)
library(here)

here::i_am("replication/stata/export_panel.R")
source(here("replication", "code", "R", "functions.R"))  # tercile_flag

panel <- read_parquet(here("replication", "output", "plastic_corruption_panel.parquet"))
measures <- sort(unique(panel$corruption_measure))


# Importer attributes (mirrors 03_robustness.R) --------------------------------

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
dist_china <- dist_cepii |> filter(iso_o == "CHN") |> transmute(imp = iso_d, dist_china = as.numeric(dist))
dist_eu15 <- dist_cepii |> filter(iso_o %in% eu15) |> rename(imp = iso_d) |>
  summarise(dist_eu15 = min(dist, na.rm = TRUE), .by = imp)
wgi_2016 <- wgi_raw |> filter(year == 2016, indicator %in% c("ge", "rl")) |>
  transmute(imp = code, indicator, estimate) |> pivot_wider(names_from = indicator, values_from = estimate)
waste_intensity <- panel |>
  filter(corruption_measure == "cpi_inverse", treat == 1L, year %in% 2014:2016) |>
  summarise(pre_waste_imports = sum(quantity, na.rm = TRUE) / 3, .by = imp) |>
  mutate(high_waste_intensity = tercile_flag(pre_waste_imports, "top"))

importers <- tibble(imp = sort(unique(panel$imp))) |>
  left_join(imp_gdp, by = "imp") |> left_join(imp_epi, by = "imp") |>
  left_join(dist_china, by = "imp") |> left_join(dist_eu15, by = "imp") |>
  left_join(wgi_2016, by = "imp") |>
  left_join(waste_intensity |> select(imp, high_waste_intensity), by = "imp") |>
  transmute(
    imp,
    low_income = tercile_flag(gdppc_base, "bottom"),
    low_epi    = tercile_flag(epi_base, "bottom"),
    near_china = as.integer(dist_china <= quantile(dist_china, 0.5, na.rm = TRUE, type = 7)),
    near_eu15  = as.integer(dist_eu15  <= quantile(dist_eu15,  0.5, na.rm = TRUE, type = 7)),
    low_ge     = tercile_flag(ge, "bottom"),
    low_rl     = tercile_flag(rl, "bottom"),
    high_waste_intensity = coalesce(high_waste_intensity, 0L)
  )

resid_weak <- map(set_names(measures), \(m) {
  scores <- panel |>
    filter(corruption_measure == m, !is.na(corruption_z_baseline), !is.na(imp_gdp_per_capita)) |>
    distinct(imp, corruption_z_baseline) |>
    left_join(imp_gdp, by = "imp") |> filter(!is.na(gdppc_base))
  scores |>
    mutate(weak_resid = as.integer(
      resid(lm(corruption_z_baseline ~ log(gdppc_base), data = scores)) >
        quantile(resid(lm(corruption_z_baseline ~ log(gdppc_base), data = scores)), 2 / 3, na.rm = TRUE, type = 7)
    ), corruption_measure = m) |>
    select(imp, corruption_measure, weak_resid)
}) |>
  list_rbind()

consensus <- panel |>
  distinct(imp, corruption_measure, weak_control_baseline) |>
  pivot_wider(names_from = corruption_measure, values_from = weak_control_baseline) |>
  transmute(imp, consensus_weak = as.integer(cpi_inverse == 1 & wgi_cc_inverse == 1 & vdem_v2x_corr == 1))


# Assemble and export ----------------------------------------------------------

panel |>
  left_join(importers, by = "imp") |>
  left_join(resid_weak, by = join_by(imp, corruption_measure)) |>
  left_join(consensus, by = "imp") |>
  transmute(
    corruption_measure, year, exp, imp, pclass = class, exp_imp,
    quantity, value,
    treat, post_china_ban, post_china_ban_2018,
    weak_control_baseline, weak_control_baseline_pre,
    weak_control_median, weak_control_top_quartile, weak_control_top_quintile,
    middle_control_baseline, high_control_baseline, corruption_z_baseline,
    treat_policy_event, post_policy_event,
    low_income, low_epi, near_china, near_eu15, low_ge, low_rl,
    high_waste_intensity, weak_resid, consensus_weak
  ) |>
  write_dta(here("replication", "stata", "estimation_panel.dta"))

cli::cli_inform(c("v" = "Wrote replication/stata/estimation_panel.dta"))
