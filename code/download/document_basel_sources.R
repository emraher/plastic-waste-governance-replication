# Basel Convention plastic-waste policy sources. The Basel amendment status,
# reporting dashboard, and import/export restriction pages have no stable
# machine-download endpoint, so this script writes a source manifest and an
# empty policy-event template. The frozen plastic_policy_events.csv used by the
# analysis is curated by hand from these sources.

library(tibble)
library(readr)
library(here)

here::i_am("replication/code/download/document_basel_sources.R")

policy_dir <- here("replication", "data", "policy")
reference_dir <- file.path(policy_dir, "basel_reference")
dir.create(reference_dir, recursive = TRUE, showWarnings = FALSE)

sources <- tribble(
  ~dataset,                              ~source_url,                                                                    ~dataset_type,
  "basel_plastic_amendments_status",     "https://www.basel.int/Countries/StatusofRatifications/PlasticWasteamendments/tabid/8377/Default.aspx", "policy",
  "basel_reporting_dashboard",           "https://ers.basel.int/eRSodataReports2/ReportBC_DashBoard.html",                "reporting",
  "basel_import_export_restrictions",    "https://www.basel.int/Countries/ImportExportRestrictions/tabid/4835/Default.aspx", "policy",
  "basel_country_profiles",              "https://www.basel.int/Countries/CountryProfiles/tabid/4498/Default.aspx",       "policy_reference",
  "oecd_transboundary_hazardous_waste",  "https://stats.oecd.org/",                                                      "trade_reference",
  "eurostat_waste_shipments",            "https://ec.europa.eu/eurostat/web/waste/data/database",                        "trade_reference",
  "github_basel_scraper_reference",      "https://github.com/jstet/Basel_Convention_Scraper",                            "reference"
)

expected_downloads <- tribble(
  ~filename_hint,                 ~source_dataset,
  "basel_dashboard_export.xlsx",  "basel_reporting_dashboard",
  "oecd_transboundary_waste.csv", "oecd_transboundary_hazardous_waste",
  "eurostat_env_wasship.csv",     "eurostat_waste_shipments"
)

write_csv(sources, file.path(reference_dir, "sources.csv"))
write_csv(expected_downloads, file.path(reference_dir, "expected_downloads.csv"))

template_path <- file.path(policy_dir, "plastic_policy_events.csv")
if (!file.exists(template_path)) {
  write_csv(
    tibble(iso3 = character(), material = character(),
           policy_name = character(), start_year = integer()),
    template_path
  )
}

message("Basel source manifest and policy-event template written to ", policy_dir)
