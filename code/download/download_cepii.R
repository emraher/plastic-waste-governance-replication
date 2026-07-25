# Download CEPII geographic and gravity data: bilateral distances (GeoDist),
# the Gravity database, and the language file. Writes to replication/data/gravity/.
#
# The analysis uses the bilateral `dist` field from dist_cepii.dta.

library(dplyr)
library(readr)
library(haven)
library(here)

here::i_am("replication/code/download/download_cepii.R")

cepii_dir <- here("replication", "data", "gravity")
dir.create(cepii_dir, recursive = TRUE, showWarnings = FALSE)

# GeoDist (https://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=6)
write_dta(read_dta("https://www.cepii.fr/distance/geo_cepii.dta"),
          file.path(cepii_dir, "geo_cepii.dta"))
write_dta(read_dta("https://www.cepii.fr/distance/dist_cepii.dta"),
          file.path(cepii_dir, "dist_cepii.dta"))

# Gravity (https://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=8)
gravity_url <- "https://www.cepii.fr/DATA_DOWNLOAD/gravity/data/Gravity_rds_V202211.zip"
gravity_zip <- file.path(cepii_dir, "Gravity_rds_V202211.zip")
gravity_out <- file.path(cepii_dir, "Gravity_rds_V202211")
download.file(gravity_url, destfile = gravity_zip, mode = "wb")
dir.create(gravity_out, showWarnings = FALSE, recursive = TRUE)
unzip(gravity_zip, exdir = gravity_out)
read_rds(file.path(gravity_out, "Gravity_V202211.rds")) |>
  write_rds(file.path(cepii_dir, "gravity.rds"))

# Language (https://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=19)
write_dta(read_dta("https://www.cepii.fr/DATA_DOWNLOAD/language/ling_web.dta"),
          file.path(cepii_dir, "ling_web.dta"))
