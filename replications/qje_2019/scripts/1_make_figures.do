************
* SCRIPT:   1_make_figures.do
* PURPOSE:  Creates Figures 2, 3, 5a, 5b, and 5c
************

*****************************
*** Set Color/Font Scheme ***
*****************************

local dkorange = `" "232 74 39" "'
local ltorange = `" "244 165 147" "'
local dkblue = `" "19 41 75" "'
local ltblue = `" "137 148 165" "'
local dkgray = `" "gs12" "'
local ltgray = `" "gs12*.5" "'
local vltgray = `" "gs15" "'

graph set window fontface "Times New Roman"

*****************************
*** Figure 2 (participation)
*****************************

* Load participation data
use "$WellnessPublic/data/stata/participation.dta", clear

gen participation = .
gen group = .
gen x_par = .
gen par_lab = ""

local j = 0

foreach v in screening2016_c hra_c_yr1 activity_f_c_yr1 activity_s_c_yr1 screening2017_c hra_c_yr2 activity_f_c_yr2 activity_s_c_yr2 {
	
	summ `v' if treat==1
	local ++j
	replace participation = r(mean) in `j'
}

replace x_par = 2 + (_n - 1)*3 if _n <= 8

replace par_lab = string(100*participation,"%2.1f") + "%" if _n <= 8

replace group = 1 if inlist(_n,1,2,5,6) /*screening*/
replace group = 2 if inlist(_n,3,7) /*fall*/
replace group = 3 if inlist(_n,4,8) /*spring*/

twoway (bar participation x_par if group == 1, color(`dkorange') fintensity(inten100) base(0)) ///
	(bar participation x_par if group == 2, color(`dkblue') fintensity(inten100) base(0)) ///
	(bar participation x_par if group == 3, color(`dkgray') fintensity(inten100) base(0)) ///
	(scatter participation x_par, msymbol(i) mlabcolor(black) mlabel(par_lab) mlabposition(12)), ///
	ylabel(0 "0%" .2 "20%" .4 "40%" .6 "60%", gmin gmax glcolor(`vltgray') angle(0) labsize(small) noticks) ///
	xlabel(2 `" " " "Screening" "' 5 `" " " "HRA" "' ///
			6.5 `" " " " " " " "{bf:Year 1}" "' ///
			8 `" "Fall" "Activity" "' 11 `" "Spring" "Activity" "' ///
			14 `" " " "Screening" "' 17 `" " " "HRA" "' ///
			18.5 `" " " " " " " "{bf:Year 2}" "' ///
			20 `" "Fall" "Activity" "' 23 `" "Spring" "Activity" "', labsize(small) noticks) /// 
	yscale(range(0 .65) noline)  xscale(range(1 23) noline) ysize(4.25) xsize(5.5) ///
	graphregion(color(white) margin(r+5)) legend(off) xtitle("") ytitle("") ///
	name(par, replace)

gr_edit .AddLine added_lines editor 8 4.8 62 4.8
gr_edit .AddLine added_lines editor 8 4.8 8 5.5
gr_edit .AddLine added_lines editor 62 4.8 62 5.5
gr_edit .AddLine added_lines editor 63.8 4.8 117.8 4.8
gr_edit .AddLine added_lines editor 63.8 4.8 63.8 5.5
gr_edit .AddLine added_lines editor 117.8 4.8 117.8 5.5
gr_edit .added_lines[1].style.editstyle linestyle(width(medthin) color(black)) editcopy
gr_edit .added_lines[2].style.editstyle linestyle(width(medthin) color(black)) editcopy
gr_edit .added_lines[3].style.editstyle linestyle(width(medthin) color(black)) editcopy
gr_edit .added_lines[4].style.editstyle linestyle(width(medthin) color(black)) editcopy
gr_edit .added_lines[5].style.editstyle linestyle(width(medthin) color(black)) editcopy
gr_edit .added_lines[6].style.editstyle linestyle(width(medthin) color(black)) editcopy
 
graph export "$WellnessPublic_QJE_2019/results/figures/figure_2.pdf", replace


*****************************
*** Figures 5a and 5b (difference in mean spending and nonzero spending)
*****************************

use "$WellnessPublic/data/stata/claims.dta", clear

gen y = .
gen x = 2 + (_n-1)*2 if _n <= 2
gen y_lab = ""
gen ci_hi = .
gen ci_lo = .

foreach y in spend_0816_0717 nonzero_spend_0816_0717 {

	if "`y'" == "spend_0816_0717" { 
		local weights "[aw=covg_0816_0717]"
		local y_lab `" "$" + string(y,"%9.0fc") "'
		local y_ax `" 0 "$0" 250 "$250" 500 "$500" 750 "$750" "'
		local fn "figure_5a"
	}
	else if "`y'" == "nonzero_spend_0816_0717" { 
		local weights ""
		local y_lab `" string(100*y,"%2.1f") + "%" "'
		local y_ax `" 0 "0%" .25 "25%" .5 "50%" .75 "75%" 1 "100%" "'
		local fn "figure_5b"
	}
	
	reg `y' i.treat `weights', robust
	
	replace y = _b[_cons] in 1
	replace y = _b[_cons] + _b[1.treat] in 2
	replace ci_hi = y + invttail(e(df_r),.025)*_se[1.treat] in 2
	replace ci_lo = y - invttail(e(df_r),.025)*_se[1.treat] in 2
	
	count if !treat & e(sample)
	local x_ax = `"2 "Control (N=`=string(r(N),"%9.0gc")')" "'
	count if treat & e(sample)
	local x_ax = `" `x_ax' 4 "Treatment (N=`=string(r(N),"%9.0gc")')" "'
	
	replace y_lab = `y_lab'
	
	twoway (bar y x if _n == 1, color(`dkblue') fintensity(inten100) base(0)) ///
		(bar y x if _n == 2, color(`dkorange') fintensity(inten100) base(0)) ///
		(rcap ci_hi ci_lo x if _n == 2, lwidth(vthin) color(black)) ///
		(scatter y x if _n == 1, msymbol(i) mlabcolor(black) mlabel(y_lab) mlabposition(12) mlabsize(medlarge)) ///
		(scatter ci_hi x if _n == 2, msymbol(i) mlabcolor(black) mlabel(y_lab) mlabposition(12) mlabsize(medlarge)), ///
		ylabel(`y_ax', angle(0) grid gmin gmax glcolor(`vltgray') noticks labsize(medlarge)) ///
		xlabel(`x_ax', noticks labsize(medlarge)) ///
		xtitle("") ytitle("") legend(off) graphregion(color(white)) ///
		xscale(noline range(1 5)) yscale(noline) ///
		name(`y'_dif, replace)
	
	graph export "$WellnessPublic_QJE_2019/results/figures/`fn'.pdf", replace
}


*****************************
*** Figures 3 and 5c (baseline spending and post spending)
*****************************

* Salary is omitted because this variable is censored
foreach v in spend_0715_0716 spend_0816_0717 {
	
	if "`v'"== "spend_0715_0716" {
		use "$WellnessPublic/data/stata/claims.dta", clear
		local fn "figure_3"
	}     
	if "`v'"== "spend_0816_0717" {
		use "$WellnessPublic/data/stata/claims.dta", clear
		local fn "figure_5c"
	}

	gen y = .
	gen ci_hi = .
	gen ci_lo = .

	* Baseline vars: compare screened to unscreened, so limit sample to people in the treatment group
	if "`v'"=="spend_0715_0716" {		
		keep if treat==1
		local x_title = "Monthly Spending in Pre-Period (Dollars)"
		local group "hra_c_yr1"
		local scale ""
		local y_lab `" 0 "0%" .05 "5%" .1 "10%" .15 "15%" .2 "20%" .25 "25%" .3 "30%" .35 "35%" "'
		local control "Not Screened"
		local treat "Screened"
		local text1 ".28 18"
		local text2 ".27 18"
		local fname "spend_pre"
	}

	* Post vars: compare treatment to control, so limit sample to people in the study
	else if "`v'"=="spend_0816_0717" {
		local x_title = "Monthly Spending in Post-Period (Dollars)"		
		local group "treat"
		local scale ""
		local y_lab `" 0 "0%" .05 "5%" .1 "10%" .15 "15%" .2 "20%" .25 "25%" .3 "30%" "'
		local control "Control         "
		local treat "Treated   "
		local text1 ".24 18"
		local text2 ".232 18"
		local fname "spend_post"
	}
	else error 1
	
	drop if mi(`v')
	gen group = `group'==1

	* Kolmogorov-Smirnov test
	ksmirnov `v', by(group)
	local KS_p = r(p)
	local KS_p = "= " + string(r(p),"%9.3f")
	if r(p) < 0.001 local KS_p = "< 0.001"
	
	* Create bins for the histogram, according to quantiles of !group (i.e., quantiles of either non-participants or control group)
	qui summ `v' if !group, d
	local p25 = round(`r(p25)', .01)
	local p50 = round(`r(p50)', .01)
	local p75 = round(`r(p75)', .01)
	local p90 = round(`r(p90)', .01)
	local p95 = round(`r(p95)', .01)
	gen `v'_cut = 1 + (`v'>0) + (`v'>`r(p25)') + (`v'>`r(p50)') + (`v'>`r(p75)') + (`v'>`r(p90)') + (`v'>`r(p95)')
	
	local x_ax `" 1.5 "$0" 6.5 `"(0–`=round(`p25'`scale')']"' 11.5 "(`=round(`p25'`scale')'–`=round(`p50'`scale')']" 16.5 "(`=round(`p50'`scale')'–`=round(`p75'`scale')']" 21.5 "(`=round(`p75'`scale')'–`=round(`p90'`scale')']" 26.5 "(`=round(`p90'`scale')'–`=round(`p95'`scale')']" 31.5 "`=round(`p95'`scale')'+" "'

	* Calculate chi-squared test
	tab `v'_cut group, col nofreq chi2
	local cs_p = "= " + string(r(p),"%9.3f")
	if r(p) < 0.001 local cs_p = "< 0.001"
	
	* Bins and confidence intervals
	qui forvalues i = 1/7 {
	
		qui gen `v'_cut_`i' = `v'_cut==`i'

		qui _regress `v'_cut_`i' group, robust
		replace y = _b[_cons] if _n == 2*`i' - 1
		replace y = _b[_cons] + _b[group] if _n == 2*`i'
		replace ci_hi = y + invttail(e(df_r),.025)*_se[group] if _n == 2*`i'
		replace ci_lo = y - invttail(e(df_r),.025)*_se[group] if _n == 2*`i'
		
		drop `v'_cut_`i'
		
	}
	
	gen x = .5*(5*_n - 6 + mod(_n,2)*3) if _n <= 14
	
	twoway (bar y x if mod(_n,2), color(`dkblue') fintensity(inten100) base(0)) ///
		(bar y x if !mod(_n,2), color(`dkorange') fintensity(inten100) base(0)) ///
		(rcap ci_hi ci_lo x if !mod(_n,2), lwidth(vthin) color(black)), ///
		ylabel(`y_lab', angle(0) grid gmin gmax glcolor(`vltgray') noticks labsize(medsmall)) ///
		xlabel(`x_ax', noticks labsize(small)) ysize(4.25) xsize(5.5) ///
		xtitle("`x_title'", size(medium)) ytitle("") graphregion(color(white)) xscale(noline) yscale(noline) ///
		legend(order(1 "`control'" 2 "`treat'") symxsize(*.2) bmargin(medium) ring(0) bplacement(ne) size(small) region(color(white)) position(2)) ///
		text(`text1' "Pearson's chi-squared test for equality: p-value `cs_p'", place(e) size(vsmall)) ///
		text(`text2' "Kolmogorov-Smirnov test for equality: p-value `KS_p'", place(e) size(vsmall)) ///
		name(`v'_distribution, replace)
		
	graph export "$WellnessPublic_QJE_2019/results/figures/`fn'.pdf", replace
}

** EOF
