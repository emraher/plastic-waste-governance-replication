# Assemble the Transparency International Corruption Perceptions Index (CPI).
#
# The current-edition CPI (2012+) is fetched from the TI API and saved as
# CPI_2012-2025.rds; that step is run manually (see the commented block below)
# because the API occasionally rate-limits. Historical editions (1995-2011) are
# manually downloaded CSVs in replication/data/corruption/. This script harmonizes both onto a
# common 0-100 scale and writes cpi.csv (read by the panel build).

library(dplyr)
library(purrr)
library(readr)
library(here)

here::i_am("replication/code/download/download_cpi.R")
cpi_dir <- here("replication", "data", "corruption")

# Manual API fetch (run once, then reused from the saved RDS):
#   library(httr)
#   response <- GET("https://www.transparency.org/en/api/latest/cpi")
#   stopifnot(status_code(response) == 200)
#   content(response, as = "parsed", type = "application/json") |>
#     data.table::rbindlist() |>
#     saveRDS(file.path(cpi_dir, "CPI_2012-2025.rds"))

latest_cpi <- read_rds(file.path(cpi_dir, "CPI_2012-2025.rds")) |>
  select(year, country, iso = iso3, score, rank)

historical_cpi <- list.files(cpi_dir, pattern = "CPI_.*csv", full.names = TRUE) |>
  map(read_csv) |>
  map2(1995:2011, \(df, y) mutate(df, year = y)) |>
  map(\(df) select(df, -any_of("interval"))) |>
  reduce(full_join) |>
  select(year, country, iso, score, rank)

combined_cpi <- full_join(latest_cpi, historical_cpi) |>
  arrange(year, country, iso) |>
  rename(cpi_score = score, cpi_rank = rank) |>
  mutate(
    # A few pre-2012 rows are entered as 37/38 instead of 3.7/3.8 on the 0-10 scale.
    cpi_score = if_else(year <= 2011 & cpi_score > 10, cpi_score / 10, cpi_score),
    # TI rescaled the CPI from 0-10 to 0-100 in 2012; lift the early years.
    cpi_score = if_else(year <= 2011, cpi_score * 10, cpi_score),
    # Repair a handful of missing/incorrect ISO3 codes.
    iso = case_when(
      country == "Belize" ~ "BLZ", country == "Benin" ~ "BEN",
      country == "Cote d'Ivoire" ~ "CIV", country == "Croatia" ~ "HRV",
      country == "Cuba" ~ "CUB", country == "Cyprus" ~ "CYP",
      country == "Czech Republic" ~ "CZE", country == "Denmark" ~ "DNK",
      country == "Dominican Republic" ~ "DOM", country == "Ecuador" ~ "ECU",
      country == "Ireland" ~ "IRL", country == "Nepal" ~ "NPL",
      country == "Nigeria" ~ "NGA", country == "Saint Vincent and the Grenadines" ~ "VCT",
      country == "Slovenia" ~ "SVN", country == "USA" ~ "USA",
      country == "Uruguay" ~ "URY", .default = iso
    )
  )

if (any(combined_cpi$cpi_score < 0 | combined_cpi$cpi_score > 100, na.rm = TRUE)) {
  stop("CPI score outside the expected 0-100 range after harmonization.")
}

write_rds(combined_cpi, file.path(cpi_dir, "cpi.rds"))
write_csv(combined_cpi, file.path(cpi_dir, "cpi.csv"))
