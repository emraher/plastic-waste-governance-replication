# Download UN Comtrade HS 3915 plastic-waste flows (used by the mirror-statistics
# test). Requires a free Comtrade API token: register once with
# comtradr::ct_register_token(), or set the COMTRADE_PRIMARY environment variable.
#
# Writes one RDS per year to replication/data/comtrade/. This refreshes from the live
# API; the paper uses the frozen vintage documented in replication/data/README.md.

library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(comtradr)
library(here)

here::i_am("replication/code/download/download_comtrade.R")

key <- Sys.getenv("COMTRADE_PRIMARY", unset = "")
if (!identical(key, "")) comtradr::set_primary_comtrade_key(key)
if (identical(comtradr::get_primary_comtrade_key(), "")) {
  stop("Missing Comtrade API key. Run comtradr::ct_register_token() or set COMTRADE_PRIMARY.")
}

# HS 3915 and its four six-digit children.
plastic_codes <- ct_get_ref_table("HS") |>
  filter(id %in% ct_commodity_lookup("waste", return_code = TRUE, return_char = TRUE),
         parent == "3915" | id == "3915") |>
  pull(id)

output_dir <- here("replication", "data", "comtrade")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

get_year <- function(year_value) {
  ct_get_data(
    type = "goods", frequency = "A", commodity_classification = "HS",
    commodity_code = plastic_codes,
    flow_direction = c("import", "export", "re-export", "re-import"),
    reporter = "all_countries", partner = "all_countries",
    start_date = year_value, end_date = year_value,
    tidy_cols = TRUE, cache = TRUE
  )
}

walk(1996:2024, \(year_value) {
  message("Downloading Comtrade HS 3915 for ", year_value)
  write_rds(get_year(year_value), file.path(output_dir, str_glue("comtrade_{year_value}.rds")))
})

# Combined convenience file.
list.files(output_dir, pattern = "^comtrade_\\d+\\.rds$", full.names = TRUE) |>
  map(read_rds) |>
  bind_rows() |>
  write_rds(file.path(output_dir, "comtrade.rds"))
