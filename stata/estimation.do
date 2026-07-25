* ============================================================================
* Stata cross-implementation of the PPML estimation results.
*
* Reproduces, with ppmlhdfe, the headline estimates from the R pipeline:
*   1. Main triple difference (full sample and China-excluded)
*        expected: 1.547 / 0.993 / 1.178 (full)  and  1.263 / 0.663 / 0.800 (xc)
*   2. Event study on the clean lead-lag dummy basis (2016 reference)
*   3. Joint pre-trend Wald test on the 2015/2016 leads (chi2 convention)
*   4. Timing check: post-2018 triple
*   5. Value-outcome triple on the value-complete sample
* Order of corruption_measure below prints cpi / vdem / wgi.
*
* Input : estimation_panel.dta  (run export_panel.R first)
* Log   : estimation.log
* Requires: ppmlhdfe (ssc install ppmlhdfe), reghdfe, ftools.
* ============================================================================

version 17
clear all
set more off
cap log close
log using "estimation.log", replace text

* ppmlhdfe from SSC if not already installed.
cap which ppmlhdfe
if _rc ssc install ppmlhdfe, replace

local measures "cpi_inverse vdem_v2x_corr wgi_cc_inverse"
local panel "estimation_panel.dta"

program define make_fe
    egen long g_ci  = group(pclass exp imp)
    egen long g_cy  = group(pclass exp year)
    egen long g_iy  = group(imp year)
    egen long g_exp = group(exp)
    egen long g_imp = group(imp)
    gen double triple   = treat * post_china_ban * weak_control_baseline
    gen double tp       = treat * post_china_ban
    gen double tw       = treat * weak_control_baseline
    gen double pw       = post_china_ban * weak_control_baseline
    gen double tpe_ppe  = treat_policy_event * post_policy_event
end

foreach m of local measures {
    di _n(2) "{hline 78}"
    di as result "MEASURE: `m'"
    di "{hline 78}"

    * ---------- Quantity sample ----------
    use "`panel'", clear
    keep if corruption_measure == "`m'"
    keep if !missing(quantity)
    make_fe
    forvalues y = 2014/2020 {
        gen double trip_`y' = treat * (year == `y') * weak_control_baseline
    }
    gen double triple18 = treat * post_china_ban_2018 * weak_control_baseline
    gen double tp18      = treat * post_china_ban_2018
    gen double p18w      = post_china_ban_2018 * weak_control_baseline

    di _n as txt ">>> 1a. Main triple, FULL sample"
    ppmlhdfe quantity triple tp tw pw treat post_china_ban weak_control_baseline ///
        tpe_ppe treat_policy_event post_policy_event, ///
        absorb(g_ci g_cy g_iy) vce(cluster g_exp g_imp)

    di _n as txt ">>> 2a. Event study (2016 reference), FULL sample"
    ppmlhdfe quantity trip_2014 trip_2015 trip_2017 trip_2018 trip_2019 trip_2020 ///
        tpe_ppe, absorb(g_ci g_cy g_iy) vce(cluster g_exp g_imp)

    di _n as txt ">>> 3a. Joint pre-trend Wald (2014-referenced model), FULL sample"
    ppmlhdfe quantity trip_2015 trip_2016 trip_2017 trip_2018 trip_2019 trip_2020 ///
        tpe_ppe, absorb(g_ci g_cy g_iy) vce(cluster g_exp g_imp)
    test trip_2015 trip_2016

    di _n as txt ">>> 4a. Timing: post-2018 triple, FULL sample"
    ppmlhdfe quantity triple18 tp18 tw p18w treat post_china_ban_2018 weak_control_baseline ///
        tpe_ppe treat_policy_event post_policy_event, ///
        absorb(g_ci g_cy g_iy) vce(cluster g_exp g_imp)

    * ---------- China-excluded ----------
    drop if imp == "CHN"

    di _n as txt ">>> 1b. Main triple, EXCLUDING China"
    ppmlhdfe quantity triple tp tw pw treat post_china_ban weak_control_baseline ///
        tpe_ppe treat_policy_event post_policy_event, ///
        absorb(g_ci g_cy g_iy) vce(cluster g_exp g_imp)

    di _n as txt ">>> 2b. Event study (2016 reference), EXCLUDING China"
    ppmlhdfe quantity trip_2014 trip_2015 trip_2017 trip_2018 trip_2019 trip_2020 ///
        tpe_ppe, absorb(g_ci g_cy g_iy) vce(cluster g_exp g_imp)

    di _n as txt ">>> 3b. Joint pre-trend Wald (2014-referenced model), EXCLUDING China"
    ppmlhdfe quantity trip_2015 trip_2016 trip_2017 trip_2018 trip_2019 trip_2020 ///
        tpe_ppe, absorb(g_ci g_cy g_iy) vce(cluster g_exp g_imp)
    test trip_2015 trip_2016

    * ---------- Value sample ----------
    use "`panel'", clear
    keep if corruption_measure == "`m'"
    keep if !missing(value)
    make_fe

    di _n as txt ">>> 5. Value-outcome triple, value-complete sample"
    ppmlhdfe value triple tp tw pw treat post_china_ban weak_control_baseline ///
        tpe_ppe treat_policy_event post_policy_event, ///
        absorb(g_ci g_cy g_iy) vce(cluster g_exp g_imp)
}

di _n(2) as result "Done. See estimation.log."
log close
