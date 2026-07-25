# Figures: raw HS 3915 group trends (Figure 1) and the China-excluded
# event study (Figure 2), in AER/JPE style. Reads the panel and the
# event-study plot data written by 02.

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(arrow)
library(here)
library(cli)
library(ggplot2)

here::i_am("replication/code/04_figures.R")
out <- \(file) here("replication", "output", file)

measure_levels <- c("V-Dem corruption index",
                    "WGI control of corruption inverse",
                    "CPI inverse")
label_measure <- \(x) factor(case_when(
  x == "cpi_inverse"    ~ "CPI inverse",
  x == "wgi_cc_inverse" ~ "WGI control of corruption inverse",
  x == "vdem_v2x_corr"  ~ "V-Dem corruption index"
), levels = measure_levels)

journal_theme <- theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray92", linewidth = 0.3),
    panel.border = element_rect(color = "gray75", linewidth = 0.4),
    axis.ticks = element_line(color = "gray55", linewidth = 0.3),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.width = grid::unit(1.5, "cm")
  )


# Figure 1: raw HS 3915 group trends, excluding China -------------------------

panel <- read_parquet(out("plastic_corruption_panel.parquet"))
trend_base <- panel |> filter(class == "plastic_waste", imp != "CHN")

group_trends <- trend_base |>
  summarise(total_quantity = sum(quantity, na.rm = TRUE),
            .by = c(corruption_measure, weak_control_baseline, year)) |>
  mutate(series = if_else(weak_control_baseline == 1L, "Weak control", "Other importers"))

# Turkey is weak-control in 2016 only under V-Dem, so its exclusion is
# informative for that measure alone.
no_turkey_trends <- trend_base |>
  filter(corruption_measure == "vdem_v2x_corr", weak_control_baseline == 1L, imp != "TUR") |>
  summarise(total_quantity = sum(quantity, na.rm = TRUE), .by = c(corruption_measure, year)) |>
  mutate(series = "Weak control excluding Turkey")

raw_trends <- bind_rows(group_trends, no_turkey_trends) |>
  mutate(index_2016 = total_quantity / total_quantity[year == 2016],
         .by = c(corruption_measure, series)) |>
  mutate(
    corruption_measure_label = label_measure(corruption_measure),
    series = factor(series, levels = c("Weak control", "Weak control excluding Turkey",
                                       "Other importers"))
  )
write_csv(raw_trends, out("raw_trend_plot_data_excluding_china.csv"))

raw_trend_plot <- ggplot(raw_trends,
    aes(year, index_2016, color = series, linetype = series, shape = series, group = series)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray60", linewidth = 0.3) +
  geom_vline(xintercept = 2016.5, linetype = "dashed", color = "gray60", linewidth = 0.3) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.9) +
  facet_wrap(~corruption_measure_label) +
  scale_x_continuous(breaks = sort(unique(raw_trends$year))) +
  scale_color_manual(values = c("Weak control" = "#C0392B",
                                "Weak control excluding Turkey" = "#E67E22",
                                "Other importers" = "#2C3E50")) +
  scale_linetype_manual(values = c("Weak control" = "solid",
                                   "Weak control excluding Turkey" = "dashed",
                                   "Other importers" = "solid")) +
  scale_shape_manual(values = c("Weak control" = 16,
                                "Weak control excluding Turkey" = 17,
                                "Other importers" = 15)) +
  labs(x = "Year", y = "Recorded HS 3915 import quantity (index, 2016 = 1)",
       color = NULL, linetype = NULL, shape = NULL) +
  journal_theme

ggsave(out("raw_trends_hs3915_excluding_china.pdf"), raw_trend_plot, width = 10, height = 5)
ggsave(out("raw_trends_hs3915_excluding_china.png"), raw_trend_plot, width = 10, height = 5, dpi = 300)


# Figure 2: China-excluded event study ----------------------------------------

event_data <- read_csv(out("event_study_plot_data_excluding_china.csv"), show_col_types = FALSE) |>
  mutate(corruption_measure_label = label_measure(corruption_measure))

event_plot <- ggplot(event_data, aes(year, estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.3) +
  geom_vline(xintercept = 2016, linetype = "dashed", color = "gray50", linewidth = 0.3) +
  geom_errorbar(aes(ymin = conf_low_95, ymax = conf_high_95), width = 0.05, alpha = 0.6, color = "#C0392B") +
  geom_errorbar(aes(ymin = conf_low_90, ymax = conf_high_90), width = 0.1, linewidth = 0.7, color = "#C0392B") +
  geom_line(aes(group = 1), linewidth = 0.4, color = "#C0392B") +
  geom_point(size = 1.5, color = "#C0392B") +
  facet_wrap(~corruption_measure_label) +
  scale_x_continuous(breaks = sort(unique(event_data$year))) +
  labs(x = "Year", y = "Triple-interaction coefficient") +
  journal_theme

ggsave(out("event_study_corruption_heterogeneity_excluding_china.pdf"), event_plot, width = 10, height = 5)
ggsave(out("event_study_corruption_heterogeneity_excluding_china.png"), event_plot, width = 10, height = 5, dpi = 300)

cli_inform(c("v" = "Figures written"))
