# Appendix descriptive tables: the HS6 code list (Appendix A), the product-class
# summary medians (Appendix A), adjacent chapter-39 descriptives (Appendix D
# text), and the 500-draw product-randomization inference (Appendix D).

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)
library(arrow)
library(here)
library(cli)
library(fixest)

here::i_am("replication/code/05_tables.R")
source(here("replication", "code", "R", "functions.R"))
out <- \(file) here("replication", "output", file)

product_class_codes <- read_csv(out("product_class_codes.csv"),
                                col_types = cols(hs6 = col_character(), .default = col_guess()))
plastic_codes <- product_class_codes |> filter(class == "plastic_waste") |> pull(hs6)
regular_plastic_codes <- product_class_codes |> filter(class == "plastic_regular") |> pull(hs6)
general_waste_codes <- product_class_codes |> filter(class == "general_waste") |> pull(hs6)


# Appendix A: HS6 code list ----------------------------------------------------

class_order <- c("plastic_waste", "plastic_regular", "general_waste")
appendix_hs6_codes <- product_class_codes |>
  mutate(class = factor(class, levels = class_order)) |>
  arrange(class, hs6) |>
  mutate(class = as.character(class))
write_csv(appendix_hs6_codes, out("appendix_hs6_codes.csv"))


# Per-code product diagnostics -------------------------------------------------

baci <- load_baci_trade()

unit_values_2014 <- baci |>
  filter(year == 2014, quantity > 0, value > 0) |>
  summarise(unit_value_2014 = mean(value / quantity, na.rm = TRUE), .by = hs6)

product_year <- baci |>
  summarise(
    quantity = if (all(is.na(quantity))) NA_real_ else sum(quantity, na.rm = TRUE),
    value    = if (all(is.na(value))) NA_real_ else sum(value, na.rm = TRUE),
    .by = c(hs6, year)
  ) |>
  mutate(class = case_when(
    hs6 %in% plastic_codes         ~ "plastic_waste",
    hs6 %in% regular_plastic_codes ~ "plastic_regular",
    hs6 %in% general_waste_codes   ~ "general_waste",
    str_sub(hs6, 1, 2) == "39"     ~ "adjacent_ch39_other",
    .default = "other_kept_code"
  )) |>
  left_join(unit_values_2014, by = "hs6")

# Appendix A: product-class summary medians.
control_group_diagnostics <- product_year |>
  filter(class %in% class_order) |>
  select(hs6, class, unit_value_2014, year, quantity) |>
  pivot_wider(names_from = year, values_from = quantity, names_prefix = "quantity_", values_fill = 0) |>
  mutate(
    avg_quantity_2014_2016 = (quantity_2014 + quantity_2015 + quantity_2016) / 3,
    quantity_pct_change_2014_2016 = if_else(
      quantity_2014 > 0, 100 * (quantity_2016 - quantity_2014) / quantity_2014, NA_real_
    )
  ) |>
  arrange(class, hs6)
write_csv(control_group_diagnostics, out("control_group_diagnostics.csv"))

product_control_summary <- control_group_diagnostics |>
  summarise(
    hs6_codes = n(),
    median_unit_value_2014 = median(unit_value_2014, na.rm = TRUE),
    median_avg_quantity_2014_2016 = median(avg_quantity_2014_2016, na.rm = TRUE),
    median_quantity_growth_2014_2016 = median(quantity_pct_change_2014_2016, na.rm = TRUE),
    .by = class
  ) |>
  arrange(class)
write_csv(product_control_summary, out("product_class_summary.csv"))

# Appendix D text: adjacent chapter-39 descriptives.
adjacent_diagnostics <- product_year |>
  filter(str_sub(hs6, 1, 2) == "39") |>
  summarise(
    pre_quantity  = mean(quantity[year %in% 2014:2016], na.rm = TRUE),
    post_quantity = mean(quantity[year %in% 2018:2020], na.rm = TRUE),
    pre_value     = mean(value[year %in% 2014:2016], na.rm = TRUE),
    post_value    = mean(value[year %in% 2018:2020], na.rm = TRUE),
    .by = c(hs6, class)
  ) |>
  mutate(
    quantity_pct_change = if_else(pre_quantity > 0, 100 * (post_quantity - pre_quantity) / pre_quantity, NA_real_),
    pre_value_per_ton  = if_else(pre_quantity > 0, pre_value / pre_quantity, NA_real_),
    post_value_per_ton = if_else(post_quantity > 0, post_value / post_quantity, NA_real_),
    value_per_ton_change = post_value_per_ton - pre_value_per_ton
  )
write_csv(adjacent_diagnostics, out("adjacent_hs6_diagnostics.csv"))

adjacent_summary <- adjacent_diagnostics |>
  summarise(median_quantity_pct_change = median(quantity_pct_change, na.rm = TRUE),
            median_value_per_ton_change = median(value_per_ton_change, na.rm = TRUE),
            .by = class)
write_csv(adjacent_summary, out("adjacent_hs6_summary.csv"))

cli_inform(c("v" = "Appendix descriptive tables written"))


# Appendix D: product-randomization inference (500 draws, seeded) --------------

source(here("replication", "code", "R", "randomization_inference.R"))
