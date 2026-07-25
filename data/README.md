# Data directory

Every analysis and download script reads from this tree via
`here("replication", "data", ...)`.

The inputs fall into two groups. The tables the authors assembled are included
in the replication package, because they are not available anywhere else. The
third-party sources are not redistributed and must be obtained from their
providers, which the second table below explains how to do.

## Included in the replication package

These are the authors' own compilations. They ship with the code, so nothing
needs to be downloaded to obtain them.

| Path | Contents |
|------|----------|
| `membership/panel_ban.csv` | Basel-ban membership by country-year |
| `membership/panel_eu.csv` | EU membership by country-year |
| `membership/panel_oecd.csv` | OECD membership by country-year |
| `reference/plastic_hs6_crosswalk.csv` | HS 3915 treated-code crosswalk, with the classification source for each code |
| `reference/waste_categories.csv` | Waste-category HS6 codes, following the Higashida–Managi crosswalk |
| `policy/plastic_policy_events.csv` | Destination policy-event table (Malaysia and others); `code/download/document_basel_sources.R` writes its provenance manifest |

## Obtain these from the provider

None of these are redistributed here. All are publicly available, though BACI,
Comtrade, and V-Dem require a free registration. `00_run_all.R` checks for each
one before it runs and stops with a list of what is missing.

| Path | Contents | How to get it |
|------|----------|---------------|
| `baci/` | BACI HS12 V202401 bilateral trade (annual CSVs) + `country_codes_V202401.csv` | CEPII BACI; free registration, manual bulk download |
| `comtrade/comtrade_<year>.rds` | UN Comtrade HS 3915 flows (mirror test) | `code/download/download_comtrade.R` |
| `comtrade/comtrade_4707.rds` | UN Comtrade HS 4707 flows, pooled | `code/download/download_comtrade_placebo.R` |
| `comtrade/placebo_4707/` | UN Comtrade HS 4707 placebo flows, by year | `code/download/download_comtrade_placebo.R` |
| `corruption/cpi.csv` | Transparency International CPI, harmonized 1995–2025 | `code/download/download_cpi.R` |
| `corruption/wgidataset.dta` | World Bank Worldwide Governance Indicators | `code/download/download_wgi.R` |
| `corruption/vdem.parquet` | V-Dem political corruption index (`v2x_corr`) | V-Dem release; free registration, manual download |
| `gravity/dist_cepii.dta` | CEPII bilateral distances | `code/download/download_cepii.R` |
| `controls/gdp_per_capita.csv` | World Bank real GDP per capita (`NY.GDP.PCAP.KD`) | World Bank bulk CSV; manual |
| `controls/epi.csv` | Environmental Performance Index | Yale EPI; manual |

Inputs marked *manual* have no stable machine-download endpoint — bulk trade
archives or registration-gated releases — and are frozen at the vintages the
published results use.

## Refresh helpers in `code/download/`

These fetch the scriptable sources above. They are **optional**, not part of
`00_run_all.R`: the paper is built from frozen vintages, and re-running them can
pull newer releases. Do not let them overwrite the frozen inputs.

| Script | Source | Pipeline input it writes |
|--------|--------|--------------------------|
| `download_comtrade.R` | UN Comtrade (HS 3915) | `comtrade/comtrade_<year>.rds` |
| `download_comtrade_placebo.R` | UN Comtrade (HS 4707) | `comtrade/placebo_4707/`, `comtrade/comtrade_4707.rds` |
| `download_wgi.R` | World Bank WGI | `corruption/wgidataset.dta` |
| `download_cepii.R` | CEPII GeoDist / Gravity / Language | `gravity/dist_cepii.dta` |
| `download_cpi.R` | Transparency International CPI | `corruption/cpi.csv` |

**Comtrade API token:** register a free token once with
`comtradr::ct_register_token()`, or set `COMTRADE_PRIMARY` in the environment.

Some helpers write more than the pipeline reads: `download_cpi.R` also saves an
RDS copy of `cpi.csv`, `download_comtrade.R` also saves a pooled copy of the
per-year files, `download_cepii.R` fetches several CEPII releases alongside
`dist_cepii.dta`, and `document_basel_sources.R` writes a provenance manifest for
the policy-event table. Nothing downstream reads these, so their presence or
absence is not a dependency problem.
