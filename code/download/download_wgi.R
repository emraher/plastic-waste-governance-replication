# Download the World Bank Worldwide Governance Indicators (WGI) dataset and
# unzip it to replication/data/corruption/ (the analysis reads wgidataset.dta).
#
# This pulls the current WGI release, which may revise historical values; the
# paper uses the frozen 2024 update documented in replication/data/README.md.

library(purrr)
library(here)

here::i_am("replication/code/download/download_wgi.R")

wgi_urls <- c(
  "https://www.worldbank.org/content/dam/sites/govindicators/doc/wgidataset_excel.zip",
  "https://www.worldbank.org/content/dam/sites/govindicators/doc/wgidataset_stata.zip",
  "https://www.worldbank.org/content/dam/sites/govindicators/doc/wgidataset_with_sourcedata_excel.zip",
  "https://www.worldbank.org/content/dam/sites/govindicators/doc/wgidataset_with_sourcedata_stata.zip"
)

download_dir <- here("replication", "data", "corruption")
unzipped_dir <- download_dir
dir.create(unzipped_dir, recursive = TRUE, showWarnings = FALSE)

walk(wgi_urls, \(url) {
  destination <- file.path(download_dir, basename(url))
  download.file(url, destfile = destination, mode = "wb")
  unzip(destination, exdir = unzipped_dir)
})
