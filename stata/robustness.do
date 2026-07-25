* ============================================================================
* Stata cross-implementation of the robustness estimates.
*
* Reproduces, with ppmlhdfe/reghdfe, the appendix robustness specifications
* that run from the panel: corruption thresholds and continuous score,
* alternative control baskets, product placebo, leave-one-out, trade margins,
* and rival destination characteristics. Each reports the identified triple
* interaction(s), matching the R (fixest) estimates.
*
* Not covered here (require BACI/Comtrade rebuilds or extra packages, so they
* stay R-only): the policy-screened and superset baskets, mirror statistics,
* and the HonestDiD bounds.
*
* Input : estimation_panel.dta   (run export_panel.R first)
* Log   : robustness.log
* ============================================================================

version 17
clear all
set more off
cap log close
log using "robustness.log", replace text

cap which ppmlhdfe
if _rc ssc install ppmlhdfe, replace
* reghdfe (extensive-margin LPM) needs ftools and require.
cap ssc install ftools, replace
cap ssc install require, replace
cap which reghdfe
if _rc ssc install reghdfe, replace

local measures "cpi_inverse vdem_v2x_corr wgi_cc_inverse"
local panel "estimation_panel.dta"

* Build fixed-effect ids and the base interaction terms on the current sample.
program define make_fe
    cap drop g_ci g_cy g_iy g_exp g_imp triple tp tw pw tpe_ppe
    egen long g_ci  = group(pclass exp imp)
    egen long g_cy  = group(pclass exp year)
    egen long g_iy  = group(imp year)
    egen long g_exp = group(exp)
    egen long g_imp = group(imp)
    gen double triple  = treat * post_china_ban * weak_control_baseline
    gen double tp      = treat * post_china_ban
    gen double tw      = treat * weak_control_baseline
    gen double pw      = post_china_ban * weak_control_baseline
    gen double tpe_ppe = treat_policy_event * post_policy_event
end

* Triple difference with an arbitrary binary weak-control variable.
program define ptriple
    args wv
    cap drop _t3 _t2 _w2
    gen double _t3 = treat * post_china_ban * `wv'
    gen double _t2 = treat * `wv'
    gen double _w2 = post_china_ban * `wv'
    ppmlhdfe quantity _t3 tp _t2 _w2 treat post_china_ban `wv' ///
        tpe_ppe treat_policy_event post_policy_event, ///
        absorb(g_ci g_cy g_iy) vce(cluster g_exp g_imp)
    drop _t3 _t2 _w2
end

* Joint weak-control + rival-characteristic triple (reports both 3-way terms).
program define pjoint
    args rv
    cap drop _r3 _r2 _rw
    gen double _r3 = treat * post_china_ban * `rv'
    gen double _r2 = treat * `rv'
    gen double _rw = post_china_ban * `rv'
    ppmlhdfe quantity triple _r3 tp tw pw _r2 _rw ///
        treat post_china_ban weak_control_baseline `rv' ///
        tpe_ppe treat_policy_event post_policy_event, ///
        absorb(g_ci g_cy g_iy) vce(cluster g_exp g_imp)
    drop _r3 _r2 _rw
end

foreach m of local measures {
    di _n(2) "{hline 78}"
    di as result "MEASURE: `m'"
    di "{hline 78}"

    use "`panel'", clear
    keep if corruption_measure == "`m'"
    keep if !missing(quantity)
    make_fe

    di _n as txt ">>> Continuous corruption score (treat x post x z-score)"
    cap drop _t3 _t2 _w2
    gen double _t3 = treat * post_china_ban * corruption_z_baseline
    gen double _t2 = treat * corruption_z_baseline
    gen double _w2 = post_china_ban * corruption_z_baseline
    ppmlhdfe quantity _t3 tp _t2 _w2 treat post_china_ban corruption_z_baseline ///
        tpe_ppe treat_policy_event post_policy_event if !missing(corruption_z_baseline), ///
        absorb(g_ci g_cy g_iy) vce(cluster g_exp g_imp)
    drop _t3 _t2 _w2

    di _n as txt ">>> Pre-period (2010-2013) tercile"
    ptriple weak_control_baseline_pre
    di _n as txt ">>> Threshold: median split"
    ptriple weak_control_median
    di _n as txt ">>> Threshold: top quartile"
    ptriple weak_control_top_quartile
    di _n as txt ">>> Threshold: top quintile"
    ptriple weak_control_top_quintile

    di _n as txt ">>> Tercile bins: middle and high vs low"
    cap drop mt ht tmt tht wmt wht
    gen double tmt = treat * post_china_ban * middle_control_baseline
    gen double tht = treat * post_china_ban * high_control_baseline
    ppmlhdfe quantity tmt tht tp treat post_china_ban middle_control_baseline high_control_baseline ///
        tpe_ppe treat_policy_event post_policy_event, ///
        absorb(g_ci g_cy g_iy) vce(cluster g_exp g_imp)
    drop tmt tht

    di _n as txt ">>> No receiving-country policy controls"
    ppmlhdfe quantity triple tp tw pw treat post_china_ban weak_control_baseline, ///
        absorb(g_ci g_cy g_iy) vce(cluster g_exp g_imp)

    di _n as txt ">>> Timing: post-2018, drop 2017"
    preserve
        keep if year != 2017
        make_fe
        cap drop t3_18
        gen double t3_18 = treat * post_china_ban_2018 * weak_control_baseline
        gen double tp18   = treat * post_china_ban_2018
        gen double pw18   = post_china_ban_2018 * weak_control_baseline
        ppmlhdfe quantity t3_18 tp18 tw pw18 treat post_china_ban_2018 weak_control_baseline ///
            tpe_ppe treat_policy_event post_policy_event, ///
            absorb(g_ci g_cy g_iy) vce(cluster g_exp g_imp)
    restore

    di _n as txt ">>> Drop 2020"
    preserve
        keep if year <= 2019
        make_fe
        ppmlhdfe quantity triple tp tw pw treat post_china_ban weak_control_baseline ///
            tpe_ppe treat_policy_event post_policy_event, ///
            absorb(g_ci g_cy g_iy) vce(cluster g_exp g_imp)
    restore

    * ---- China-excluded specifications ----
    drop if imp == "CHN"
    make_fe

    di _n as txt ">>> Control basket: regular plastic only (excl. China)"
    preserve
        keep if inlist(pclass, "plastic_waste", "plastic_regular")
        make_fe
        ppmlhdfe quantity triple tp tw pw treat post_china_ban weak_control_baseline ///
            tpe_ppe treat_policy_event post_policy_event, ///
            absorb(g_ci g_cy g_iy) vce(cluster g_exp g_imp)
    restore

    di _n as txt ">>> Control basket: general waste only (excl. China)"
    preserve
        keep if inlist(pclass, "plastic_waste", "general_waste")
        make_fe
        ppmlhdfe quantity triple tp tw pw treat post_china_ban weak_control_baseline ///
            tpe_ppe treat_policy_event post_policy_event, ///
            absorb(g_ci g_cy g_iy) vce(cluster g_exp g_imp)
    restore

    di _n as txt ">>> Product placebo: regular plastic treated vs general waste (excl. China)"
    preserve
        keep if inlist(pclass, "plastic_regular", "general_waste")
        gen byte treat_pl = pclass == "plastic_regular"
        cap drop g_ci g_cy g_iy g_exp g_imp
        egen long g_ci  = group(pclass exp imp)
        egen long g_cy  = group(pclass exp year)
        egen long g_iy  = group(imp year)
        egen long g_exp = group(exp)
        egen long g_imp = group(imp)
        gen double p3 = treat_pl * post_china_ban * weak_control_baseline
        gen double p_tp = treat_pl * post_china_ban
        gen double p_tw = treat_pl * weak_control_baseline
        ppmlhdfe quantity p3 p_tp p_tw pw treat_pl post_china_ban weak_control_baseline ///
            tpe_ppe treat_policy_event post_policy_event, ///
            absorb(g_ci g_cy g_iy) vce(cluster g_exp g_imp)
    restore

    di _n as txt ">>> Trade margins: extensive (LPM) and intensive (excl. China)"
    preserve
        gen byte positive_trade = quantity > 0
        reghdfe positive_trade triple tp tw pw treat post_china_ban weak_control_baseline ///
            tpe_ppe treat_policy_event post_policy_event, ///
            absorb(g_ci g_cy g_iy) vce(cluster g_exp g_imp)
    restore
    preserve
        gen byte _prepos = quantity > 0 & treat == 1 & inrange(year, 2014, 2016)
        bysort exp_imp: egen byte pre_positive = max(_prepos)
        keep if pre_positive == 1
        make_fe
        ppmlhdfe quantity triple tp tw pw treat post_china_ban weak_control_baseline ///
            tpe_ppe treat_policy_event post_policy_event, ///
            absorb(g_ci g_cy g_iy) vce(cluster g_exp g_imp)
    restore

    di _n as txt ">>> Leave-one-out: top contributors (excl. China)"
    local drops_cpi_inverse    "UKR MMR UZB LAO MEX"
    local drops_vdem_v2x_corr  "TUR UKR PAK UZB PHL"
    local drops_wgi_cc_inverse "UKR PAK UZB LAO MEX"
    foreach c of local drops_`m' {
        preserve
            drop if imp == "`c'"
            make_fe
            di as txt "  -- excluding `c'"
            ppmlhdfe quantity triple tp tw pw treat post_china_ban weak_control_baseline ///
                tpe_ppe treat_policy_event post_policy_event, ///
                absorb(g_ci g_cy g_iy) vce(cluster g_exp g_imp)
        restore
    }

    di _n as txt ">>> Rival destination characteristics (joint, excl. China)"
    foreach rv in low_income low_epi near_china near_eu15 low_ge low_rl high_waste_intensity {
        di as txt "  -- weak-control + `rv'"
        pjoint `rv'
    }
}

di _n(2) as result "Done. See robustness.log."
log close
