# Stata cross-implementation

An independent Stata replication of the paper's PPML **estimation** results
(no output tables or figures — those are produced by the R pipeline). It
re-derives the headline and robustness coefficients with `ppmlhdfe`/`reghdfe`
and confirms they match the R (`fixest`) estimates to machine precision.

## Files

| File | Purpose |
|------|---------|
| `export_panel.R` | Reads `replication/output/plastic_corruption_panel.parquet` and writes `estimation_panel.dta` (estimation columns plus the importer attributes the robustness specs need). |
| `estimation.do` / `estimation.log` | Headline estimates and their batch-mode log. |
| `robustness.do` / `robustness.log` | Appendix robustness estimates and their batch-mode log. |

## Run

```sh
Rscript replication/stata/export_panel.R          # build estimation_panel.dta
cd replication/stata
stata-mp -b do estimation.do
stata-mp -b do robustness.do
```

`estimation_panel.dta` is a regenerated artifact; `export_panel.R` writes it and
it is not shipped with the code.

## What `estimation.do` estimates

Per corruption measure (CPI, V-Dem, WGI), on the R pipeline's exact samples:

1. **Main triple difference** — full sample (`1.547 / 0.993 / 1.178`) and
   China-excluded (`1.263 / 0.663 / 0.800`).
2. **Event study** on the clean lead-lag dummy basis (2016 reference).
3. **Joint pre-trend Wald test** on the 2015/2016 leads. Stata reports the
   chi-square convention, so its p-value differs from the R F-based p-value
   (0.780 / 0.553 / 0.048) while testing the same restriction.
4. **Timing check** — the post-2018 triple.
5. **Value-outcome triple** on the value-complete sample.

## What `robustness.do` estimates

Per corruption measure, all matching the R `fixest` triple-interaction
coefficients exactly:

- **Continuous corruption score** (treat × post × z-score).
- **Alternative weak-control cutoffs** — pre-period (2010–2013) tercile,
  median split, top quartile, top quintile, and middle/high-vs-low tercile bins.
- **No receiving-country policy controls.**
- **Timing** — post-2018 (dropping 2017) and dropping 2020.
- **Alternative control baskets** (excl. China) — regular-plastic-only and
  general-waste-only.
- **Product placebo** — regular plastic pseudo-treated vs general waste.
- **Trade margins** — extensive-margin LPM (`reghdfe`) and intensive margin on
  pre-positive pairs.
- **Leave-one-out** — the top-5 contributing importers per measure.
- **Rival destination characteristics** — joint weak-control + rival triples for
  low income, low EPI, near China, near EU15, low government effectiveness, low
  rule of law, and high waste intensity.

Specifications that require BACI/Comtrade rebuilds or extra packages stay
R-only: the policy-screened and superset baskets, the mirror statistics, and the
HonestDiD bounds.

## Requirements

Stata 17 (or newer) with `ppmlhdfe`, `reghdfe`, `ftools`, and `require`. Both
do-files install any missing packages from SSC on first run.
