# Download UN Comtrade HS 4707 (recovered paper) flows for the placebo trade
# series. Requires a Comtrade API token (see download_comtrade.R).
#
# Writes one RDS per year to replication/data/comtrade/placebo_4707/ and a combined file.

library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(comtradr)
library(here)

here::i_am("replication/code/download/download_comtrade_placebo.R")

key <- Sys.getenv("COMTRADE_PRIMARY", unset = "")
if (!identical(key, "")) comtradr::set_primary_comtrade_key(key)
if (identical(comtradr::get_primary_comtrade_key(), "")) {
  stop("Missing Comtrade API key. Run comtradr::ct_register_token() or set COMTRADE_PRIMARY.")
}

placebo_codes <- c("4707", "470710", "470720", "470730", "470790")

output_dir <- here("replication", "data", "comtrade", "placebo_4707")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

get_year <- function(year_value) {
  ct_get_data(
    type = "goods", frequency = "A", commodity_classification = "HS",
    commodity_code = placebo_codes,
    flow_direction = c("import", "export", "re-export", "re-import"),
    reporter = "all_countries", partner = "all_countries",
    start_date = year_value, end_date = year_value,
    tidy_cols = TRUE, cache = TRUE
  )
}

walk(2002:2020, \(year_value) {
  message("Downloading Comtrade HS 4707 placebo for ", year_value)
  write_rds(get_year(year_value), file.path(output_dir, str_glue("comtrade_4707_{year_value}.rds")))
})

list.files(output_dir, pattern = "^comtrade_4707_\\d+\\.rds$", full.names = TRUE) |>
  map(read_rds) |>
  bind_rows() |>
  write_rds(here("replication", "data", "comtrade", "comtrade_4707.rds"))
