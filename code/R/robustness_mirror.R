# Mirror statistics (Appendix I): exporter-reported vs importer-reported HS 3915
# flows from raw UN Comtrade (pre-BACI reconciliation). Tests whether the
# importer-level reporting gap moves differentially for weak-control importers
# after 2017. Sourced by 03.

comtrade <- map(2014:2020, \(y) {
  path <- here("replication", "data", "comtrade", str_glue("comtrade_{y}.rds"))
  if (!file.exists(path)) cli::cli_abort("Missing file {.path {path}}.")
  readRDS(path) |>
    as_tibble() |>
    filter(cmdCode == "3915") |>
    transmute(
      year = as.integer(refYear), reporter = reporterISO, partner = partnerISO,
      flow = flowCode, value = as.numeric(primaryValue), net_weight = as.numeric(netWgt)
    )
}) |>
  list_rbind() |>
  filter(
    !partner %in% c("W00", "WLD", "_X", "X1", "X2"),
    str_length(partner) == 3, str_length(reporter) == 3,
    reporter != partner, flow %in% c("M", "X")
  )

importer_reported <- comtrade |>
  filter(flow == "M") |>
  rename(imp = reporter) |>
  summarise(m_value = sum(value, na.rm = TRUE), m_weight = sum(net_weight, na.rm = TRUE),
            .by = c(imp, year))
exporter_reported <- comtrade |>
  filter(flow == "X") |>
  rename(imp = partner) |>
  summarise(x_value = sum(value, na.rm = TRUE), x_weight = sum(net_weight, na.rm = TRUE),
            .by = c(imp, year))

weak_flags <- panel_full |> distinct(imp, corruption_measure, weak_control_baseline)
balanced_importers <- sort(unique(weak_flags$imp))

mirror <- full_join(importer_reported, exporter_reported, by = join_by(imp, year)) |>
  filter(imp %in% balanced_importers) |>
  mutate(post_china_ban = as.integer(year >= 2017L))

counts <- tibble(
  statistic = c("importer_years_total", "importer_years_both_positive_value",
                "importer_years_importer_only", "importer_years_exporter_only"),
  value = c(
    nrow(mirror),
    sum(mirror$m_value > 0 & mirror$x_value > 0, na.rm = TRUE),
    sum(mirror$m_value > 0 & (is.na(mirror$x_value) | mirror$x_value == 0)),
    sum(mirror$x_value > 0 & (is.na(mirror$m_value) | mirror$m_value == 0))
  )
)
write_csv(counts, out("mirror_gap_counts.csv"))

gap_panel <- mirror |>
  filter(m_value > 0, x_value > 0) |>
  mutate(
    gap_value = log(m_value / x_value),
    gap_weight = if_else(m_weight > 0 & x_weight > 0, log(m_weight / x_weight), NA_real_)
  )

mirror_gap <- map(set_names(sort(unique(weak_flags$corruption_measure))), \(m) {
  gp <- gap_panel |>
    inner_join(filter(weak_flags, corruption_measure == m) |> select(imp, weak_control_baseline),
               by = "imp")
  map(c("gap_value", "gap_weight"), \(outcome) {
    d <- gp |> filter(!is.na(.data[[outcome]]))
    fit <- feols(as.formula(str_glue("{outcome} ~ post_china_ban * weak_control_baseline | imp + year")),
                 vcov = ~ imp, data = d)
    summary(fit)$coeftable |>
      as_tibble(rownames = "term") |>
      rename(estimate = "Estimate", std_error = "Std. Error",
             t_value = "t value", p_value = "Pr(>|t|)") |>
      filter(term == "post_china_ban:weak_control_baseline") |>
      mutate(corruption_measure = m, outcome = outcome,
             observations = stats::nobs(fit), importers = n_distinct(d$imp))
  }) |>
    list_rbind()
}) |>
  list_rbind()

write_csv(mirror_gap, out("mirror_gap.csv"))
