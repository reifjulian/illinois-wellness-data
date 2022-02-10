************
* SCRIPT: make_tables.do
* PURPOSE: generates the tables for the main text and for Supplement 3
************

************
* Code begins
************


***
* Programs
***

program group_cnt_pct, nclass
  syntax varlist, treated(varname numeric) [format(string)]

	* Default format for numerical output
	if missing("`format'") local format %02.1f

	* Assert outcomes are binary
	foreach var of local varlist {
		assert inlist(`var', 0, 1, 100) if !mi(`var')
	}

	* Assert treatment is binary
	assert inlist(`treated', 0, 1)
	local label_0 "Control group"
	local label_1 "Treatment group"

	* Create matrices for treatment and control group
	matrix input group1_point = ()
	matrix input group1_pct = ()
	matrix input group0_point = ()
	matrix input group0_pct = ()
	
	* Store mean and percentage in treatment/control groups
	foreach treatment_status of numlist 1 0{
		foreach var of local varlist{
			* Numerator
			qui count if (`treated' == `treatment_status') & !mi(`var') & (inlist(`var',1,100))
			local num = r(N)
			matrix group`treatment_status'_point = group`treatment_status'_point \ `num'
			
			* Denominator
			qui count if (`treated' == `treatment_status') & !mi(`var')
			local denom = r(N)
			local pct: di `format' 100*`num'/`denom'
			matrix group`treatment_status'_pct = group`treatment_status'_pct \ `pct'
		}
	}
	
	* Write the result in the result dataframe
	frame change result
	local dim = rowsof(group1_point)
	
	foreach j of numlist 1/`dim'{
		local N = _N+1
		set obs `N'
		foreach v in  group1_pct  group0_pct{
			local num = string(`v'[`j',1], "`format'")
			replace `v' = "`num'" in `N'
		}
		foreach v in group1_point  group0_point {
			local num = string(`v'[`j',1], "%2.0f")
			replace `v' = "`num'" in `N'
		}
		
		frame change default
		local i: word `j' of `varlist'
		local lab: variable label `i'
		frame change result
		replace name = "`lab'" in `N'
	}
	frame change default
	
end

program group_mean_sd, nclass
  syntax varlist [aweight], treated(varname numeric) [format(string)]

	* Default format for numerical output
	if missing("`format'") local format %02.1f

	* Assert treatment is binary
	assert inlist(`treated', 0, 1)
	local label_0 "Control group"
	local label_1 "Treatment group"

	* Create matrices for treatment and control group
	matrix input group1_point = ()
	matrix input group1_pct = ()
	matrix input group0_point = ()
	matrix input group0_pct = ()
	
	
	
	* Store mean and standard deviation in treatment/control groups
	foreach treatment_status of numlist 1 0{
		foreach var of local varlist{
			qui  sum `var' if `treated' == `treatment_status' [`weight' `exp']
			local mean: di `format' `r(mean)'
			local sd: di `format' `r(sd)'
			matrix group`treatment_status'_point = group`treatment_status'_point \ `mean'
			matrix group`treatment_status'_pct = group`treatment_status'_pct \ `sd'
		}
	}
	
	* Write the result in the result dataframe
	frame change result
	local dim = rowsof(group1_point)
	
	foreach j of numlist 1/`dim'{
		local N = _N+1
		set obs `N'
		foreach v of varlist group1_point group1_pct group0_point group0_pct{
			local num = string(`v'[`j',1], "`format'")
			replace `v' = "`num'" in `N'
		}
		frame change default
		local i: word `j' of `varlist'
		local lab: variable label `i'
		frame change result
		replace name = "`lab'" in `N'
	}
	frame change default
end

program itt_effect, nclass
	syntax varlist [aweight], treated(varname numeric) [controls(string) format(string)]

	* Change to default frame
	frame change default
	
	* Default format for numerical output
	if missing("`format'") local format %3.2f

	* Assert treatment is binary
	assert inlist(`treated', 0, 1)
	
	* Estimate ITT
	quietly foreach var of local varlist{
		regress `var' `treated' `controls' [`weight' `exp'], robust
		local point = string(_b[`treated'], "`format'")
		local ci_lb = string(_b[`treated'] - invttail(e(df_r), 0.025)* _se[`treated'], "`format'")
		local ci_ub = string(_b[`treated'] + invttail(e(df_r), 0.025)* _se[`treated'], "`format'")
		local t_stat = _b[`treated'] / _se[`treated']
		local pval = string(2 * ttail(e(df_r), abs(`t_stat')), "`format'")
		local lab: variable label `var' 
		frame change result
		gen temp = _n if !missing(group0_pct) & missing(effect)
		qui sum temp
		local min = r(min)
		replace effect = "`point'" in `min'
		replace ci = "(" + "`ci_lb'" + " to " + "`ci_ub'" + ")" in `min'
		replace pval = "`pval'" in `min'
		drop temp
		frame change default
	}
end

program iv_effect, nclass
	syntax varlist [aweight], ENDOGenous(varname numeric) INSTrument(varname numeric) [controls(string) format(string)]

	* Change to default frame
	frame change default
	
	* Default format for numerical output
	if missing("`format'") local format %2.1f

	* Assert treatment is binary
	assert inlist(`instrument', 0, 1)
	
	* Estimate IV
	qui foreach y of local varlist {
		ivreg2 `y' (`endogenous' = i.`instrument') `controls' [`weight' `exp'], robust partial(`controls')
		local point = string(_b[`endogenous'], "`format'")
		local ci_lb = string(_b[`endogenous'] - invnormal(0.975)* _se[`endogenous'], "`format'")
		local ci_ub = string(_b[`endogenous'] + invnormal(0.975)* _se[`endogenous'], "`format'")
		local z_stat = _b[`endogenous'] / _se[`endogenous']
		local pval = string(2 * (1-normal(abs(`z_stat'))), "`format'")
		local lab: variable label `y' 
		frame change result
		gen temp = _n if !missing(pval) & missing(iv_effect)
		qui sum temp
		local min = r(min)
		replace iv_effect = "`point'" in `min'
		replace iv_ci = "(" + "`ci_lb'" + " to " + "`ci_ub'" + ")" in `min'
		replace iv_pval = "`pval'" in `min'
		drop temp
		frame change default
	}
end

program wy_testing_itt
	syntax anything(name = cmd), treated(varname numeric) rep(int) 
	
	* Run wyoung command
	frame change default
	quietly wyoung, cmd(`cmd') familyp(`treated') bootstraps($nboot) seed(11) detail
	matrix input A = ()
	matrix A = r(table)[1..., 4]
	
	* Input the adjusted p values in the result dataframe
	frame change result
	local dim = rowsof(A)
	foreach v of numlist 1/`dim'{
		gen temp = _n if !missing(pval) & missing(adj_pval)
		quietly sum temp
		local min = r(min)
		replace adj_pval = string(A[`v', 1],  "%3.2f") if temp==`min' & A[`v',1] >=0.01 & A[`v',1] <=0.99
		replace adj_pval = "$>$0.99" if temp==`min' & A[`v',1] > 0.99
		replace adj_pval = "$<$0.01" if temp==`min' & A[`v',1] < 0.01
		drop temp		
	}
	frame change default
end

program wy_testing_iv
	syntax anything(name = cmd), treated(varname numeric) rep(int) 
	
	* Run wyoung command
	frame change default
	quietly wyoung, cmd(`cmd') familyp(`treated') bootstraps($nboot) seed(11) detail
	matrix input A = ()
	matrix A = r(table)[1..., 4]
	
	* Input the adjusted p values in the result dataframe
	frame change result
	local dim = rowsof(A)
	foreach v of numlist 1/`dim'{
		gen temp = _n if !missing(pval) & missing(iv_adj_pval)
		quietly sum temp
		local min = r(min)
		replace iv_adj_pval = string(A[`v', 1],  "%3.2f") if temp==`min' & A[`v',1] >=0.01 & A[`v',1] <=0.99
		replace iv_adj_pval = "$>$0.99" if temp==`min' & A[`v',1] > 0.99
		replace iv_adj_pval = "$<$0.01" if temp==`min' & A[`v',1] < 0.01
		drop temp		
	}
	frame change default
end

program ste_itt, nclass
	syntax varlist, treated(varname numeric) [controls(string) format(string) weights]

	frame change default
	preserve

	* Default format for numerical output
	if missing("`format'") local format %2.1f

	* Assert treatment is binary
	assert inlist(`treated', 0, 1)

	* Number of outcome variables in domain
	local numvars = wordcount("`varlist'")

	* Control standard deviations
	forvalues i = 1/`numvars' {
		qui sum `=word("`varlist'", `i')' if `treated'==0
		local sd_varnum_`i' = `r(sd)'
	}

	* Expand dataset for stacked regression
	qui expand `numvars'
	bys AnalysisID: gen varnum = _n
	bys AnalysisID varnum: assert _N==1

	* Outcome for stacked regression
	qui gen outcome = .
	qui forvalues i = 1/`numvars' {
		by AnalysisID (varnum): replace outcome = `=word("`varlist'", `i')' if varnum == `i'
	}

	
	* Allow controls to vary by varnum. Optionally allow user to specify claims-based controls
	local CC_dx_2016 "diabetes_1015_0716 hypertension_1015_0716 hyperlipidemia_1015_0716"
	local pos_2016 "pos_office_outpatient_1015_0716 pos_hospital_1015_0716 pos_er_critical_1015_0716"
	local pcp_vars_2016 "pcp_total_visits_1015_0716 pcp_total_office_1015_0716 pcp_any_visits_1015_0716 pcp_any_office_1015_0716"
	
	assert "`controls'" == "i.Strata" | "`controls'" == "i.Strata `CC_dx_2016' `pos_2016'" | "`controls'" == "i.Strata `pcp_vars_2016'"
	egen controls_strata = group(Strata varnum)

	if strpos("`controls'","`CC_dx_2016'") {
		local controls_x_var
		foreach var of varlist  `CC_dx_2016' `pos_2016' {
			local controls_x_var `controls_x_var' c.`var'##i.varnum
		}
	}
	
	* Allow treatment to vary by varnum
	forvalues i = 1/`numvars' {
		gen treat_varnum_`i' = `treated'*(varnum==`i')
	}
	
	* If weights specified, choose the appropriate coverage period as the weight (determined by suffix of outcome variable)
	qui if !missing("`weights'") {
		tempname covg_weights tmp
		gen `tmp' = ""
		gen `covg_weights' = .
		forvalues i = 1/`numvars' {
			by AnalysisID (varnum): replace `tmp' = `"`=word("`varlist'", `i')'"' if varnum == `i'
		}
		replace `covg_weights' = covg_0816_0717 if strpos(`tmp',"0717")
		replace `covg_weights' = covg_0816_0718 if strpos(`tmp',"0718")
		assert !mi(`covg_weights')
		local wt "[aw=`covg_weights']"
	}

	* Stacked regression
	qui areg outcome treat_varnum_* `controls_x_var' `wt', a(controls_strata) cluster(AnalysisID)

	* Standardized treatment effect
	local ste 0
	forvalues i = 1/`numvars' {
		local ste `ste' + (_b[treat_varnum_`i']/`sd_varnum_`i'')
	}
	qui lincom (`ste')/`numvars'

	local effect = string(`r(estimate)', "`format'")
	local ci = "(" + string(`r(lb)', "`format'") + " to " + string(`r(ub)', "`format'") + ")"
	local pval = string(`r(p)', "`format'")
	
	frame change result
	local N = _N+1
	set obs `N'

	foreach var of varlist effect ci pval{
		replace `var' = "``var''" in `N'
	}

	frame change default
	restore
end

program ste_iv, nclass
	syntax varlist, ENDOGenous(varname numeric) INSTrument(varname numeric) row(int) [controls(string) format(string) weights]

	frame change default
	preserve

	* Default format for numerical output
	if missing("`format'") local format %2.1f

	* Assert instrument is binary
	assert inlist(`instrument', 0, 1)
	local treated `instrument'

	* Number of outcome variables in domain
	local numvars = wordcount("`varlist'")

	* Control standard deviations
	forvalues i = 1/`numvars' {
		qui sum `=word("`varlist'", `i')' if `treated'==0
		local sd_varnum_`i' = `r(sd)'
	}

	* Expand dataset for stacked regression
	qui expand `numvars'
	bys AnalysisID: gen varnum = _n
	bys AnalysisID varnum: assert _N==1

	* Outcome for stacked regression
	qui gen outcome = .
	qui forvalues i = 1/`numvars' {
		by AnalysisID (varnum): replace outcome = `=word("`varlist'", `i')' if varnum == `i'
	}

	* Allow controls to vary by varnum. Optionally allow user to specify claims-based controls
	local CC_dx_2016 "diabetes_1015_0716 hypertension_1015_0716 hyperlipidemia_1015_0716"
	local pos_2016 "pos_office_outpatient_1015_0716 pos_hospital_1015_0716 pos_er_critical_1015_0716"
	local pcp_vars_2016 "pcp_total_visits_1015_0716 pcp_total_office_1015_0716 pcp_any_visits_1015_0716 pcp_any_office_1015_0716"
	
	assert "`controls'" == "i.Strata" | "`controls'" == "i.Strata `CC_dx_2016' `pos_2016'" | "`controls'" == "i.Strata `pcp_vars_2016'"
	egen controls_strata = group(Strata varnum)

	if strpos("`controls'","`CC_dx_2016'") {
		local controls_x_var
		foreach var of varlist  `CC_dx_2016' `pos_2016' {
			local controls_x_var `controls_x_var' c.`var'##i.varnum
		}
	}

	* Allow both endogenous var and "treatment" (instrument) to vary by varnum
	forvalues i = 1/`numvars' {
		gen endog_varnum_`i' = `endogenous'*(varnum==`i')
		gen iv_varnum_`i'    = `instrument'*(varnum==`i')
	}
	
	* If weights specified, choose the appropriate coverage period as the weight (determined by suffix of outcome variable)
	qui if !missing("`weights'") {
		tempname covg_weights tmp
		gen `tmp' = ""
		gen `covg_weights' = .
		forvalues i = 1/`numvars' {
			by AnalysisID (varnum): replace `tmp' = `"`=word("`varlist'", `i')'"' if varnum == `i'
		}
		replace `covg_weights' = covg_0816_0717 if strpos(`tmp',"0717")
		replace `covg_weights' = covg_0816_0718 if strpos(`tmp',"0718")
		assert !mi(`covg_weights')
		local wt "[aw=`covg_weights']"
	}	

	* Stacked regression
	qui ivreg2 outcome (endog_varnum_* = iv_varnum_*) i.controls_strata `controls_x_var' `wt', cluster(AnalysisID) partial(i.controls_strata `controls_x_var')

	* Standardized treatment effect
	local ste 0
	forvalues i = 1/`numvars' {
		local ste `ste' + (_b[endog_varnum_`i']/`sd_varnum_`i'')
	}
	qui lincom (`ste')/`numvars'
	
	
	local iv_effect = string(`r(estimate)', "`format'")
	local iv_ci = "(" + string(`r(lb)', "`format'") + " to " + string(`r(ub)', "`format'") + ")"
	local iv_pval = string(`r(p)', "`format'")
	
	frame change result
	foreach var of varlist iv_effect iv_ci iv_pval{
		replace `var' = "``var''" in `row'
	}

	frame change default
	restore
end

program itt_mde, nclass
  syntax varlist [aweight], treated(varname numeric) [controls(string) format(string)]

	* Frame
	frame change default 
	
	* Default format for numerical output
	if missing("`format'") local format %3.2f

	* Assert treatment is binary
	assert inlist(`treated', 0, 1)

	* Matrix to store the result
	matrix input A = ()
	
	qui foreach y of local varlist{
		_regress `y' `treated' `controls' [`weight' `exp'], robust
		local mde = _se[treat]*2.8
		matrix A = A \ `mde'
	}
	
	* Input the result in the result dataframe
	frame change result
	local dim = rowsof(A)
	foreach v of numlist 1/`dim'{
		gen temp = _n if !missing(group0_pct) & missing(mde)
		quietly sum temp
		local min = r(min)
		replace mde = string(A[`v', 1],  "%3.2f") if temp==`min'
		drop temp
	}
	frame change default
end

program itt_subgroup, nclass
  syntax varlist [aweight], treated(varname numeric) subgroup(varname numeric) [controls(string) format(string)]

	* Change to default frame
	frame change default
  
	* Default format for numerical output
	if missing("`format'") local format %2.1f

	* Assert treatment is binary
	assert inlist(`treated', 0, 1)
	
	* Assert subgroup is binary
	assert inlist(`subgroup', 0, 1)
		
	* Estimate ITT
	qui foreach y of local varlist {
		_regress `y' i.`subgroup'##i.`treated' `controls' [`weight' `exp'], robust
		local point = string(_b[1.`subgroup'#1.`treated'], "`format'")
		local ci_lb = string(_b[1.`subgroup'#1.`treated'] - invttail(e(df_r), 0.025)* _se[1.`subgroup'#1.`treated'], "`format'")
		local ci_ub = string(_b[1.`subgroup'#1.`treated'] + invttail(e(df_r), 0.025)* _se[1.`subgroup'#1.`treated'], "`format'")
		local t_stat = _b[1.`subgroup'#1.`treated'] / _se[1.`subgroup'#1.`treated']
		local pval = string(2 * ttail(e(df_r), abs(`t_stat')), "`format'")
		local pval = string(2 * ttail(e(df_r), abs(`t_stat')), "`format'")
		frame change result
		gen temp = _n if !missing(pval) & missing(iv_effect)
		qui sum temp
		local min = r(min)
		replace iv_effect = "`point'" in `min'
		replace iv_ci = "(" + "`ci_lb'" + " to " + "`ci_ub'" + ")" in `min'
		replace iv_pval = "`pval'" in `min'
		drop temp
		frame change default
	}
end

program ste_itt_subgroup, nclass
  syntax varlist, treated(varname numeric) subgroup(varname numeric) row(int) [controls(string) format(string) weights]

	* Change to default frame
	frame change default
  
	preserve

	* Default format for numerical output
	if missing("`format'") local format %2.1f

	* Assert treatment is binary
	assert inlist(`treated', 0, 1)
	
	* Assert subgroup is binary
	assert inlist(`subgroup', 0, 1)	

	* Number of outcome variables in domain
	local numvars = wordcount("`varlist'")

	* Control standard deviations
	forvalues i = 1/`numvars' {
		qui sum `=word("`varlist'", `i')' if `treated'==0
		local sd_varnum_`i' = `r(sd)'
	}

	* Expand dataset for stacked regression
	qui expand `numvars'
	bys AnalysisID: gen varnum = _n
	bys AnalysisID varnum: assert _N==1

	* Outcome for stacked regression
	qui gen outcome = .
	qui forvalues i = 1/`numvars' {
		by AnalysisID (varnum): replace outcome = `=word("`varlist'", `i')' if varnum == `i'
	}

	* Allow controls to vary by varnum. Optionally allow user to specify claims-based controls
	local CC_dx_2016 "diabetes_1015_0716 hypertension_1015_0716 hyperlipidemia_1015_0716"
	local pos_2016 "pos_office_outpatient_1015_0716 pos_hospital_1015_0716 pos_er_critical_1015_0716"
	
	assert "`controls'" == "i.Strata" | "`controls'" == "i.Strata `CC_dx_2016' `pos_2016'"
	egen controls_strata = group(Strata varnum)

	if strpos("`controls'","`CC_dx_2016'") {
		local controls_x_var
		foreach var of varlist  `CC_dx_2016' `pos_2016' {
			local controls_x_var `controls_x_var' c.`var'##i.varnum
		}
	}
	
	* Allow treatment, subgroup effect, and interaction effect to vary by varnum
	forvalues i = 1/`numvars' {
		gen treat_varnum_`i'          = `treated'*(varnum==`i')
		gen subgroup_varnum_`i'        = `subgroup'*(varnum==`i')
		gen treat_subgroup_varnum_`i' = `treated'*`subgroup'*(varnum==`i')
	}
	
	* If weights specified, choose the appropriate coverage period as the weight (determined by suffix of outcome variable)
	qui if !missing("`weights'") {
		tempname covg_weights tmp
		gen `tmp' = ""
		gen `covg_weights' = .
		forvalues i = 1/`numvars' {
			by AnalysisID (varnum): replace `tmp' = `"`=word("`varlist'", `i')'"' if varnum == `i'
		}
		replace `covg_weights' = covg_0816_0717 if strpos(`tmp',"0717")
		replace `covg_weights' = covg_0816_0718 if strpos(`tmp',"0718")
		assert !mi(`covg_weights')
		local wt "[aw=`covg_weights']"
	}

	* Stacked regression
	qui areg outcome treat_varnum_* subgroup_varnum_* treat_subgroup_varnum_* `controls_x_var' `wt', a(controls_strata) cluster(AnalysisID)

	* Standardized treatment effect
	local ste 0
	forvalues i = 1/`numvars' {
		local ste `ste' + (_b[treat_subgroup_varnum_`i']/`sd_varnum_`i'')
	}
	qui lincom (`ste')/`numvars'

	local iv_effect = string(`r(estimate)', "`format'")
	local iv_ci = "(" + string(`r(lb)', "`format'") + " to " + string(`r(ub)', "`format'") + ")"
	local iv_pval = string(`r(p)', "`format'")
	
	frame change result
	foreach var of varlist iv_effect iv_ci iv_pval{
		replace `var' = "``var''" in `row'
	}

	frame change default
	restore
end
	
program character_table, nclass

	* Build the structure for baseline character table
	capture frame create result
	frame change result
	foreach v in name group1_point group1_pct group0_point group0_pct{
		gen str `v'= ""
	}
	set obs 2
	replace  name = "Variable" in 2
	replace  group1_point = "Treatment group" in 1
	replace  group0_point = "Control group" in 1
	frame change default
end

program regression_table, nclass
	
	* Build the structure for regression table
	capture frame create result
	frame change result
	foreach v in name group1_point group1_pct group0_point group0_pct effect ci pval adj_pval{
		gen str `v'= ""
	}
	set obs 1
	replace name = "Outcome"
	replace group1_point = "Treatment"
	replace group0_point = "Control"
	replace effect = "Effect"
	replace ci = "(95\% CI)"
	replace pval = "P Value"
	replace adj_pval = "Adjusted P Value"
	frame change default
end

program add_iv_columns, nclass
	
	* Generate variables to add iv estimation results
	frame change result
	foreach v in iv_effect iv_ci iv_pval iv_adj_pval{
		gen str `v' = ""
	}
	replace iv_effect = "Effect" in 1
	replace iv_ci = "(95\% CI)" in 1
	replace iv_pval = "P Value" in 1
	replace iv_adj_pval = "Adjusted P Value$^{b}$" in 1
	frame change default
end

program add_group_label, nclass
	syntax anything(name = label)
	
	frame change result
	local N = _N+1
	set obs `N'
	replace name = `label' in  `N'
	frame change default

end

program add_missing_label, nclass
	syntax anything(name = label)
	
	frame change result
	local N = _N
	replace name = `label' in `N'
	frame change default

end

program combine_columns, nclass
	
	gen `3' = `1' + "  (" + `2' + ")"
	order `3', after(`2')
	replace `3' = `1' if missing(`2')
	drop `1' `2'
end

program add_na, nclass

	* Change frame
	frame change result
	
	* Add NA
	local N = _N+1
	set obs `N'
	replace group1_point = "N/A" in `N'
	replace group0_point = "N/A" in `N'

	* Change frame
	frame change default
	
end



	**# Table 1

	* Open the main dataset
	use "$WellnessPublic/data/stata/biometrics.dta", clear

	* Generate variables
	gen age36 = (age37_49!= 1) & (age50!=1)
	
	* Variable labels 
	label variable age36 "$<$37 yr"
	label variable age37_49 "37-50 yr"
	label variable age50 "$\geq$ 50 yr"
	label variable male "Male"
	label variable white "White"
	capture label variable salary_40k "$<$40,000"
	capture label variable salary_50k "40,000 to $<$50,000"
	capture label variable salary_75k "50,000 to $<$75,000"
	capture label variable salary_over_75k "$\geq$ 75,000"


	* Create a table
	frame change default
	character_table
	frame change result
	replace group1_point = "(n=3000)" in 2
	replace group0_point = "(n=1534)" in 2
	
	* Age variables
	add_group_label "Age group, No.(%)"
	group_cnt_pct age36 age37_49 age50, treated(treat)
	capture group_mean_sd age, treated(treat) 
	add_na
	add_missing_label "Age, mean (SD)"
	
	* Sex
	add_group_label "Sex, No.(%)"
	gen female = 1-male
	label variable female "Female"
	group_cnt_pct male, treated(treat)
	group_cnt_pct female, treated(treat)

	* Race
	add_group_label "Race, No.(%)"
	gen nonwhite = 1-white
	label variable nonwhite "Non-white"
	group_cnt_pct white nonwhite, treated(treat)

	
	* Annual salary
	* Change the dataset
	use "$WellnessPublic/data/stata/participation.dta", clear
	add_group_label "Annual salary, \textdollar, No.(%)"
	capture group_cnt_pct salary_40k salary_50k salary_75k salary_over_75k, treated(treat)
	foreach v in "$<$40,000" "40,000 to $<$50,000" "50,000 to $<$75,000" "$\geq$ 75,000"{
		add_na
		add_missing_label "`v'"
	}
	
	
	* Employee class
	add_group_label "Employee class, No.(%)"
	capture label variable faculty "Faculty"
	capture label variable AP "Academic professional"
	capture label variable CS "Civil service"
	capture group_cnt_pct faculty AP CS, treated(treat)

	
	
	* Health insurance
	use "$WellnessPublic/data/stata/claims.dta", clear
	add_group_label "Health Alliance insurance, Oct 2015 to Jul 2016"
	local covg_2016 covg_1015_0716
	capture gen covg_months_2016 = `covg_2016'*10
	capture label variable covg_months_2016 "Months of coverage, mean (SD)"
	capture gen covg_any_2016 = (`covg_2016' > 0)
	capture label variable covg_any_2016 "Any coverage, No. (%)"
	capture group_cnt_pct covg_any_2016, treated(treat) 
	capture group_mean_sd covg_months_2016, treated(treat) 

	
	* Insurance claim title
	frame change result
	local N = _N+2
	set obs `N'
	foreach v in 1 0{
		replace group`v'_point = "Insurance Claims" in `N'
	}
	local N = _N+1
	set obs `N'
	replace group1_point = "Subsample (n=2184)" in `N'
	replace group0_point = "Subsample (n=1033)" in `N'
	
	* Medical diagnoses
	add_group_label "Medical diagnoses, No.(%)"
	capture label variable diabetes_1015_0716 "Diabetes"
	capture label variable hypertension_1015_0716 "Hypertension"
	capture label variable hyperlipidemia_1015_0716 "Hyperlipidemia"
	capture group_cnt_pct diabetes_1015_0716 hypertension_1015_0716 hyperlipidemia_1015_0716, treated(treat)

	
	
	* Medical utilization
	add_group_label "Medical utilization, mean (SD), days"
	capture label variable pos_office_outpatient_1015_0716 "Office/outpatient"
	capture label variable pos_hospital_1015_0716 "Inpatient"
	capture label variable pos_er_critical_1015_0716 "ER"
	capture group_mean_sd pos_office_outpatient_1015_0716 pos_hospital_1015_0716 pos_er_critical_1015_0716, treated(treat)

	
	* Combine columns
	frame change result
	combine_columns group1_point group1_pct group1
	combine_columns group0_point group0_pct group0

	* Revise for latex
	replace name = subinstr(name, "%", "\%", .)
	replace name = subinstr(name, "_", "\_", .)
	replace name = subinstr(name, "#", "\#", .)
	replace name = "\hspace{1em} " + name if !missing(group1)
	replace group1 = group1 + "$^{b}$" in 7
	
	
	* Title and footnote
	local title "Table 1. Baseline Characteristics of the Study Population$^{a}$" 
	local fn "$^{a}$ Age, salary, and employee class are defined as of June 2016, two months prior to the start of the intervention. Medical diagnoses and medical utilization are measured over the period October 2015 to July 2016 and are derived from the insurance claims subsample, which includes all study participants enrolled in the Health Alliance plan. \newline $^{b}$ N/A indicates a censored value."
	
	* Export into latex
	texsave * using "$Wellness_Public_JAMA/results/tables/table1.tex", hlines(2 -8) nofix nonames title("`title'") footnote("`fn'") preamble("\usepackage{caption}" "\captionsetup{labelformat=empty}") replace


	**# Table 2 & eTable 4 & eTable 7 & eTable 10 & eTable 13 & eTable 16 & eTable 19 & eTable 22
	clear
	frame change default
	regression_table
	
	* Open the main dataset
	use "$WellnessPublic/data/stata/biometrics.dta", clear
	
	* Change variable labels
	foreach year of numlist 2017 2018{
		label variable self_weight_`year'		 "Weight, kg"
		label variable self_height_`year'	 "Height, cm"
		label variable self_cholesterol_`year' "Chance of high cholesterol, %"
		label variable self_bp_`year'			 "Chance of high blood pressure, %"
		label variable self_glucose_`year'	 "Chance of impaired glucose, %"
		label variable self_bmi_`year'		 "Chance of BMI > 30, %"
		
		label variable has_pcp_`year'         "Has primary physician"
		label variable tobacco_none_`year'    "No tobacco use"
		label variable activity_any_`year'    "Exercise 1+ times/week"
		label variable activity_more_`year'   "Exercise 3+ times/week"
		label variable activity_20min_`year'  "Exercise for 20 minutes"
		label variable activity_40min_`year'  "Exercise for 40 minutes"
		label variable anxiety_some_`year'    "Never or sometimes anxious/depressed"
		label variable anxiety_never_`year'   "Never anxious/depressed"
	}
	
	* Generate AnalysisID variable
	gen AnalysisID = _n
	rename Strata_biometrics Strata
	
	
	* Add group mean and sd
	local expect_vars     "self_height self_weight self_bmi self_cholesterol self_bp self_glucose"
	local expect_vars_ste "                        self_bmi self_cholesterol self_bp self_glucose"
	local survey_vars     "has_pcp tobacco_none activity_any activity_more activity_20min activity_40min anxiety_never anxiety_some"
	local survey_vars_ste "has_pcp tobacco_none activity_any activity_more activity_20min activity_40min anxiety_never anxiety_some"
	
	forvalues yyyy = 2017/2018 {
		foreach var_group in expect_vars expect_vars_ste survey_vars survey_vars_ste {
			local `var_group'_`yyyy'
			foreach var of local `var_group' {
				local `var_group'_`yyyy' ``var_group'_`yyyy'' `var'_`yyyy'
			}
			di "`var_group'_`yyyy': ``var_group'_`yyyy''"
		}
	}
	
	add_group_label "Health beliefs, 2017"
	group_mean_sd `expect_vars_2017', treated(treat)
	add_group_label "Health beliefs, 2018"
	group_mean_sd `expect_vars_2018', treated(treat)
	add_group_label "Self‐reported health behaviors, 2017, %"
	group_mean_sd `survey_vars_2017', treated(treat)
	add_group_label "Self‐reported health behaviors, 2018, %"
	group_mean_sd `survey_vars_2018', treated(treat)
	
	* Add itt effect
	
	itt_effect `expect_vars_2017' `expect_vars_2018' `survey_vars_2017' `survey_vars_2018'  , treated(treat) controls(i.Strata)
	

	* Add wyoung adjusted pvalues

	
	local family_models
	foreach yr in 2017 2018 {
	foreach y in `expect_vars' {
		local model _regress `y'_`yr' treat, a(Strata) robust
		local family_models `"`family_models'  "`model'""'
	}
	}
	
	wy_testing_itt `family_models',  treated(treat) rep($nboot)
	


	local family_models
	foreach yr in 2017 2018 {
	foreach y in `survey_vars' {
		local model _regress `y'_`yr' treat, a(Strata) robust
		local family_models `"`family_models' "`model'""'
	}
	}
	wy_testing_itt  `family_models', treated(treat) rep($nboot)
	
	
	* Standardized treatment effect
	add_group_label "Standardized treatment effect"
	ste_itt `expect_vars_ste_2017' `expect_vars_ste_2018', treated(treat) controls(i.Strata) format(%3.2f) 
	add_missing_label "Health beliefs"
	ste_itt `survey_vars_ste_2017' `survey_vars_ste_2018', treated(treat) controls(i.Strata) format(%3.2f) 	
	add_missing_label "Self‐reported health behaviors"
	


	
	* Combine columns
	frame change result
	combine_columns group1_point group1_pct group1
	combine_columns group0_point group0_pct group0
	gen effect_ci = effect +" "+ ci, after(ci)
	drop effect ci
	
	* Revise for latex
	replace name = subinstr(name, "%", "\%", .)
	replace name = subinstr(name, "_", "\_", .)
	replace name = subinstr(name, "#", "\#", .)
	replace name = subinstr(name, ">", "$>$", .)
	replace name = "\hspace{1em} " + name if !missing(pval)
	replace name = "Outcome" in 1
	
	* Footnote in the table
	replace adj_pval = adj_pval + "$^{b}$" in 1
	replace name = name + "$^{c}$" in 34
	
	* Title and footnote
	local title "Table 2. Mean Values and Effect of Wellness Program on Health Beliefs and Self-Reported Health Behaviors$^{a}$" 
	local headerlines "& \multicolumn{2}{c}{Group mean (SD)} & \multicolumn{3}{c}{Effect of Wellness Program Eligibility}" "\cmidrule(lr){2-3} \cmidrule(lr){4-6}"

	local fn "$^{a}$ This table reports effects of the wellness program. All regressions included stratification variables as controls. All outcome variables were obtained during the on-site screening in either 2017 (12-month follow-up) or 2018 (24-month follow-up). The sample size of the regressions ranged from 1,739 to 1,999 because fewer subjects participated in the 2018 screening than in the 2017 screening and because some outcomes were occasionally missing or illegible. \newline $^{b}$ Adjusted P values account for the number of hypotheses tested in each domain. We tested 12 hypotheses in the health beliefs domain and 16 hypotheses in the self-reported health behaviors domain. \newline $^{c}$ The standardized treatment effect gives equal weight to each outcome within a domain and includes both the 2017 and 2018 outcomes. The standardized treatment effect for health beliefs excludes height and weight."
	
	* Export into latex
	texsave * using "$Wellness_Public_JAMA/results/tables/table2.tex", hlines(1) headerlines("`headerlines'") nofix nonames title("`title'") footnote("`fn'", size("tiny")) size("tiny") preamble("\usepackage{caption}" "\captionsetup{labelformat=empty}") align(lCClCC)  replace



	************************
	*** Add eTable 4 
	************************
	
	* Add columns
	add_iv_columns
	
	
	* iv effect
	iv_effect `expect_vars_2017' `expect_vars_2018' `survey_vars_2017' `survey_vars_2018', endog(completed_screening_nomiss_2016) instrument(treat) controls(i.Strata) format(%3.2f)

	* wy_testing_iv
	
	local family_models_iv
	foreach yr in 2017 2018 {
	foreach y in `expect_vars' {
		local model_iv ivreg2 `y'_`yr' (completed_screening_nomiss_2016 = treat) i.Strata, robust partial(i.Strata)
		local family_models_iv `"`family_models_iv' "`model_iv'""'
	}
	}
	
	wy_testing_iv `family_models_iv',  treated(completed_screening_nomiss_2016) rep($nboot)
	


	local family_models_iv
	foreach yr in 2017 2018 {
	foreach y in `survey_vars' {
		local model_iv ivreg2 `y'_`yr' (completed_screening_nomiss_2016 = treat) i.Strata, robust partial(i.Strata)
		local family_models_iv `"`family_models_iv' "`model_iv'""'
	}
	}
	wy_testing_iv  `family_models_iv', treated(completed_screening_nomiss_2016) rep($nboot)
	
	
	* ste_iv
	ste_iv `expect_vars_ste_2017' `expect_vars_ste_2018', endog(completed_screening_nomiss_2016) instrument(treat) controls(i.Strata) format(%3.2f) row(35)
	ste_iv `survey_vars_ste_2017' `survey_vars_ste_2018', endog(completed_screening_nomiss_2016) instrument(treat) controls(i.Strata) format(%3.2f) row(36)
	
	
	
	* Combine columns
	frame change result
	gen iv_effect_ci = iv_effect +" "+ iv_ci, after(iv_ci)
	drop iv_effect iv_ci
	
	* Title and footnote
	local title "eTable 4. Local Mean Treatment Effect of Wellness Program on Health Beliefs and Self-Reported Health Behaviors$^{a}$" 
	local headerlines "& \multicolumn{2}{c}{Group mean (SD)} & \multicolumn{3}{c}{Effect of Wellness Program Eligibility} & \multicolumn{3}{c}{Effect of Wellness Program Participation}" "\cmidrule(lr){2-3} \cmidrule(lr){4-6} \cmidrule(lr){7-9}"

	local fn "$^{a}$ This table reports effects of program eligibility and local mean treatment effects of program participation. All regressions included stratification variables as controls. All outcome variables were obtained during the on-site screening in either 2017 (12-month follow-up) or 2018 (24-month follow-up). The sample size of the regressions ranged from 1,739 to 1,999 because fewer subjects participated in the 2018 screening than in the 2017 screening and because some outcomes were occasionally missing or illegible. \newline $^{b}$ Adjusted P values account for the number of hypotheses tested in each domain. We tested 12 hypotheses in the health beliefs domain and 16 hypotheses in the self-reported health behaviors domain.\newline $^{c}$ The standardized treatment effect gives equal weight to each outcome within a domain and includes both the 2017 and 2018 outcomes. The standardized treatment effect for health beliefs excludes height and weight."
	
	* Export into latex
	texsave * using "$Wellness_Public_JAMA/results/tables/etable4.tex", hlines(1) headerlines("`headerlines'") nofix nonames title("`title'") footnote("`fn'", size("tiny")) size("tiny") preamble("\usepackage{caption}" "\captionsetup{labelformat=empty}") align(lCClCClCC) landscape replace
		


	************************
	*** Heterogeneity tables
	************************
		
	local het_group "male age50 white AP CS salaryM2"	
	local title1 "Male"
	local title2 "Age 50 and Over"
	local title3 "White"
	local title4 "Academic Professional Employees"
	local title5 "Civil Service Employees"
	local title6 "Above Median Salary"
	local table_numlist "7 10 13 16 19 22"
	
	foreach i of numlist 1/6{
		local het: word `i' of `het_group'
		local table_num: word `i' of `table_numlist'
		
		* Drop iv columns and create new iv columns
		drop iv_*
		add_iv_columns
		
		* Heterogeneity itt (`het')
		capture itt_subgroup  `expect_vars_2017' `expect_vars_2018' `survey_vars_2017' `survey_vars_2018', treated(treat) subgroup(`het') controls(i.Strata) format(%3.2f)
		frame change result
		foreach v of varlist iv_effect iv_pval iv_adj_pval{
			replace `v' = "N/A" if !missing(adj_pval) & (`table_num' > 15) in 2/36
		}
		
		* wyoung
		frame change default
		capture gen interaction = `het' * treat
		local family_models
		foreach yr in 2017 2018 {
		foreach y in `expect_vars' {
			local model _regress `y'_`yr' i.`het' i.treat interaction i.Strata,  robust
			local family_models `"`family_models'  "`model'""'
		}
		}
		
		capture wy_testing_iv `family_models',  treated(interaction) rep($nboot)
		


		local family_models
		foreach yr in 2017 2018 {
		foreach y in `survey_vars' {
			local model _regress `y'_`yr' i.`het' i.treat interaction i.Strata,  robust
			local family_models `"`family_models' "`model'""'
		}
		}
		capture wy_testing_iv  `family_models', treated(interaction) rep($nboot)
		
		capture drop interaction
		
		* Heterogeneity ste_itt (`het')
		capture ste_itt_subgroup `expect_vars_2017' `expect_vars_2018', treated(treat) subgroup(`het') row(35) controls(i.Strata) format(%3.2f)
		capture ste_itt_subgroup `survey_vars_2017' `survey_vars_2018', treated(treat) subgroup(`het') row(36) controls(i.Strata) format(%3.2f)
		frame change result
		replace iv_effect = "N/A" in 35 if `table_num' > 15
		replace iv_effect = "N/A" in 36 if `table_num' > 15
		replace iv_effect = iv_effect + "$^{d}$" in 3 if `table_num' > 15
		
		* Combine columns
		gen iv_effect_ci = iv_effect +" "+ iv_ci, after(iv_ci)
		drop iv_effect iv_ci
		
		* Title and footnote
		local title "eTable `table_num'. Heterogeneity: `title`i'': Interaction Effect of Wellness Program on Health Beliefs and Self-Reported Health Behaviors$^{a}$" 
		local headerlines "& \multicolumn{2}{c}{Group mean (SD)} & \multicolumn{3}{c}{Effect of Wellness Program Eligibility (Main Effect)} & \multicolumn{3}{c}{Effect of Wellness Program Eligibility (Interaction Effect)}" "\cmidrule(lr){2-3} \cmidrule(lr){4-6} \cmidrule(lr){7-9}"

		if `table_num'<=15 local fn "$^{a}$ This table reports interaction effects for the effect of the wellness program. For reference, the table also includes the group means and effects of program eligibility from Table 2 reported in the main text. All regressions included stratification variables as controls. All outcome variables were obtained during the on-site screening in either 2017 (12-month follow-up) or 2018 (24-month follow-up). \newline $^{b}$ Adjusted P values account for the number of hypotheses tested in each domain. We tested 12 hypotheses in the health beliefs domain and 16 hypotheses in the self-reported health behaviors domain.\newline $^{c}$ The standardized treatment effect gives equal weight to each outcome within a domain and includes both the 2017 and 2018 outcomes. The standardized treatment effect for health beliefs excludes height and weight."
		
		if `table_num' > 15 local fn "$^{a}$ This table reports interaction effects for the effect of the wellness program. For reference, the table also includes the group means and effects of program eligibility from Table 2 reported in the main text. All regressions included stratification variables as controls. All outcome variables were obtained during the on-site screening in either 2017 (12-month follow-up) or 2018 (24-month follow-up). \newline $^{b}$ Adjusted P values account for the number of hypotheses tested in each domain. We tested 12 hypotheses in the health beliefs domain and 16 hypotheses in the self-reported health behaviors domain.\newline $^{c}$ The standardized treatment effect gives equal weight to each outcome within a domain and includes both the 2017 and 2018 outcomes. The standardized treatment effect for health beliefs excludes height and weight. \newline $^{d}$ N/A indicates a censored value."
		
		* Export into latex
		texsave * using "$Wellness_Public_JAMA/results/tables/etable`table_num'.tex", hlines(1) headerlines("`headerlines'") nofix nonames title("`title'") footnote("`fn'", size("tiny")) size("tiny") preamble("\usepackage{caption}" "\captionsetup{labelformat=empty}")  landscape align(lCClCClCC) replace
		
		
		
	}








	**# Table 3 & eTable 5 & eTable 8 & eTable 11 & eTable 14 & eTable 17 & eTable 20 & eTable 23
	
	clear
	frame change default
	
	* Open the main dataset
	use "$WellnessPublic/data/stata/biometrics.dta", clear
	

	* Variable labels
	foreach year of numlist 2017 2018{
		label variable height_`year'   	"Height, cm"
		label variable weight_`year'  		"Weight, kg"
		label variable waist_`year'   		"Waist, cm"
		label variable bmi_`year'   		"BMI"
		label variable systolic_`year' 	"Systolic"
		label variable diastolic_`year'	 "Diastolic"
		label variable tc_`year'   		"Total cholesterol, mg/dL"
		label variable tchdl_`year'   		"Total cholesterol / HDL cholesterol"
		label variable hdl_`year'   		"HDL cholesterol, mg/dL"
		label variable ldl_`year'   		"LDL cholesterol, mg/dL"
		label variable trg_`year'   		"Triglycerides, mg/dL"
		label variable glucose_`year'  	"Glucose, mg/dL"
		label variable high_bmi_`year'		"Obesity (BMI ≥30)"
		label variable high_bp_`year'		"Hypertension (systolic ≥130 or diastolic ≥80)"
		label variable high_chol_`year'	"High LDL cholesterol (≥100 mg/dL)"
		label variable high_gluc_`year'	"High glucose (≥100 mg/dL)"
	}
	
	* Generate AnalysisID variable
	gen AnalysisID = _n
	rename Strata_biometrics Strata
	
	* Leave LDL out because it is approximately collinear with TC, HDL, and TRG
	local bio_vars     "height weight waist bmi systolic diastolic tc hdl tchdl ldl trg glucose high_bmi high_bp high_chol high_gluc"
	local bio_vars_ste "	   weight waist     systolic diastolic tc hdl           trg glucose"

	forvalues yyyy = 2017/2018 {
		foreach var_group in bio_vars bio_vars_ste {
			local `var_group'_`yyyy'
			foreach var of local `var_group' {
				local `var_group'_`yyyy' ``var_group'_`yyyy'' `var'_`yyyy'
			}
		}
	}
	
	* Make a regression table
	regression_table
	add_group_label "Biometric outcomes, 2017"
	add_group_label "Continuous measures"
	
	
	
	* Group Mean (SD)
	group_mean_sd `bio_vars_2017' `bio_vars_2018', treated(treat) format(%2.1f) 
	
	* Adding group titles in the middle
	frame change result
	gen temp = _n, after(group0_pct)
	foreach n of numlist 1/2{
		add_group_label "Blood pressure, mm Hg"
		add_group_label "Lipid panel"
		add_group_label "Binary measures, %"
	}
	add_group_label "Biometric outcomes, 2018"
	add_group_label "Continuous measures"
	frame change result
	replace temp = 7.5 in 36
	replace temp = 9.5 in 37
	replace temp = 15.5 in 38
	replace temp = 23.5 in 39
	replace temp = 25.5 in 40
	replace temp = 31.5 in 41
	replace temp = 19.2 in 42
	replace temp = 19.3 in 43
	sort temp
	drop temp
	
	
	* ITT
	frame change default
	itt_effect `bio_vars_2017' `bio_vars_2018', treated(treat) controls(i.Strata) format(%3.2f)
	
	* Add wyoung adjusted pvalues
	local family_models
	local family_models_iv
	foreach y in `bio_vars_2017' `bio_vars_2018' {
		local model _regress `y' treat, a(Strata) robust
		local family_models `"`family_models' "`model'""'
		
		local model_iv ivreg2 `y' (completed_screening_nomiss_2016 = treat) i.Strata, robust partial(i.Strata)
		local family_models_iv `"`family_models_iv' "`model_iv'""'
	}
	
	
	wy_testing_itt `family_models', treated(treat) rep($nboot)
	
	
	* Standardized treatment effect
	add_group_label "Standardized treatment effect"
	ste_itt  `bio_vars_ste_2017' `bio_vars_ste_2018', treated(treat) controls(i.Strata) format(%3.2f) 
	add_missing_label "Biometric outcomes"
		
	
	* Combine columns
	frame change result
	combine_columns group1_point group1_pct group1
	combine_columns group0_point group0_pct group0
	gen effect_ci = effect +" "+ ci, after(ci)
	drop effect ci
	
	* Revise for latex
	replace name = subinstr(name, "%", "\%", .)
	replace name = subinstr(name, "_", "\_", .)
	replace name = subinstr(name, "#", "\#", .)
	replace name = subinstr(name, ">", "$>$", .)
	replace name = subinstr(name, "≥", "$\geq$", .)
	replace name = "\hspace{2em} " + name if !missing(group1)
	foreach v of numlist 3 18 24 39{
		replace name = "\hspace{1em} " + name in `v'
	}
	foreach v of numlist 9 10 12/16 30 31 33/37{
		replace name = subinstr(name, "2em", "3em", .) in `v'
	}
	replace name = "Outcome" in 1
	replace name = "\hspace{1em} " + name in 45
	
	* Footnote in the table
	replace adj_pval = adj_pval + "$^{b}$" in 1
	replace name = name + "$^{c}$" in 44
	
	* Title and footnote
	local title "Table 3. Mean Values and Effect of Wellness Program on Biometrics$^{a}$" 
	local headerlines "& \multicolumn{2}{c}{Group mean (SD)} & \multicolumn{3}{c}{Effect of Wellness Program Eligibility}" "\cmidrule(lr){2-3} \cmidrule(lr){4-6}"

	local fn "$^{a}$ This table reports effects of the wellness program. All regressions included stratification variables as controls. All outcome variables were obtained during the on-site screening in either 2017 (12-month follow-up) or 2018 (24-month follow-up). The sample size of the regressions ranged from 1,662 to 2,004 because fewer subjects participated in the 2018 screening than in the 2017 screening and because some outcomes were occasionally missing or illegible.\newline $^{b}$ Adjusted P values account for the 32 hypotheses tested in this domain. \newline $^{c}$ The standardized treatment effect gives equal weight to each outcome within a domain and includes both the 2017 and 2018 outcomes. It excludes height, BMI, total cholesterol / HDL cholesterol, LDL cholesterol, and the four binary measures."
	
	* Export into latex
	texsave * using "$Wellness_Public_JAMA/results/tables/table3.tex", hlines(1) headerlines("`headerlines'") nofix nonames title("`title'") footnote("`fn'", size("tiny")) size("tiny") preamble("\usepackage{caption}" "\captionsetup{labelformat=empty}") align(lCClCC) replace
	
	

	************************
	*** eTable 5
	************************
	
	* Add columns
	add_iv_columns
	
	
	* iv effect
	iv_effect `bio_vars_2017' `bio_vars_2018', endog(completed_screening_nomiss_2016) instrument(treat) controls(i.Strata) format(%3.2f)

	* wy_testing_iv
	wy_testing_iv  `family_models_iv', treated(completed_screening_nomiss_2016) rep($nboot)
	
	
	* ste_iv
	ste_iv `bio_vars_ste_2017' `bio_vars_ste_2018',  endog(completed_screening_nomiss_2016) instrument(treat) controls(i.Strata) format(%3.2f) row(45)
	

	* Combine columns
	frame change result
	gen iv_effect_ci = iv_effect +" "+ iv_ci, after(iv_ci)
	drop iv_effect iv_ci
	
	* Title and footnote
	local title "eTable 5. Local Mean Treatment Effect of Wellness Program on Biometrics$^{a}$" 
	local headerlines "& \multicolumn{2}{c}{Group mean (SD)} & \multicolumn{3}{c}{Effect of Wellness Program Eligibility} & \multicolumn{3}{c}{Effect of Wellness Program Participation}" "\cmidrule(lr){2-3} \cmidrule(lr){4-6} \cmidrule(lr){7-9}"

	local fn "$^{a}$ This table reports effects of program eligibility and local mean treatment effects of program participation. All regressions included stratification variables as controls. All outcome variables were obtained during the on-site screening in either 2017 (12-month follow-up) or 2018 (24-month follow-up). The sample size of the regressions ranged from 1,662 to 2,004 because fewer subjects participated in the 2018 screening than in the 2017 screening and because some outcomes were occasionally missing or illegible. \newline $^{b}$ Adjusted P values account for the 32 hypotheses tested in this domain. \newline $^{c}$ The standardized treatment effect gives equal weight to each outcome within a domain and includes both the 2017 and 2018 outcomes. It excludes height, BMI, total cholesterol / HDL cholesterol, LDL cholesterol, and the four binary measures."
	
	* Export into latex
	texsave * using "$Wellness_Public_JAMA/results/tables/etable5.tex", hlines(1) headerlines("`headerlines'") nofix nonames title("`title'") footnote("`fn'", size("tiny")) size("tiny") preamble("\usepackage{caption}" "\captionsetup{labelformat=empty}")  landscape align(lCClCClCC) replace
	



	
	************************
	*** Heterogeneity tables
	************************
		
	local het_group "male age50 white AP CS salaryM2"	
	local title1 "Male"
	local title2 "Age 50 and Over"
	local title3 "White"
	local title4 "Academic Professional Employees"
	local title5 "Civil Service Employees"
	local title6 "Above Median Salary"
	local table_numlist "8 11 14 17 20 23"
	
	
	foreach i of numlist 1/6{
		local het: word `i' of `het_group'
		local table_num: word `i' of `table_numlist'
		
		* Drop iv columns and create new iv columns
		drop iv_*
		add_iv_columns
		
		* Heterogeneity itt (`het')
		capture itt_subgroup  `bio_vars_2017' `bio_vars_2018', treated(treat) subgroup(`het') controls(i.Strata) format(%3.2f)
		frame change result
		foreach v of varlist iv_effect iv_pval iv_adj_pval{
			replace `v' = "N/A" if !missing(adj_pval) & (`table_num' > 15) in 2/45
		}

		
		* wyoung
		frame change default
		capture gen interaction = `het' * treat
		local family_models
		foreach y in `bio_vars_2017' `bio_vars_2018' {
			local model _regress `y' i.`het' i.treat interaction i.Strata, robust
			local family_models `"`family_models' "`model'""'
		}
		
		
		capture wy_testing_iv `family_models',  treated(interaction) rep($nboot)
		
		capture drop interaction
		
		* Heterogeneity ste_itt (male)
		capture ste_itt_subgroup `bio_vars_2017' `bio_vars_2018', treated(treat) subgroup(`het') row(45) controls(i.Strata) format(%3.2f)
		frame result: replace iv_effect = "N/A" if `table_num' > 15 in 45
		frame result: replace iv_effect = iv_effect + "$^{d}$" in 4 if `table_num' > 15
		
		* Combine columns
		frame change result
		gen iv_effect_ci = iv_effect +" "+ iv_ci, after(iv_ci)
		drop iv_effect iv_ci
		
		* Title and footnote
		local title "eTable `table_num'. Heterogeneity: `title`i'': Interaction Effect of Wellness Program on Health Beliefs and Self-Reported Health Behaviors$^{a}$" 
		local headerlines "& \multicolumn{2}{c}{Group mean (SD)} & \multicolumn{3}{c}{Effect of Wellness Program Eligibility (Main Effect)} & \multicolumn{3}{c}{Effect of Wellness Program Eligibility (Interaction Effect)}" "\cmidrule(lr){2-3} \cmidrule(lr){4-6} \cmidrule(lr){7-9}"

		if `table_num'<=15 local fn "$^{a}$ This table reports interaction effects for the effect of the wellness program. For reference, the table also includes the group means and effects of program eligibility from Table 3 reported in the main text. All outcome variables were obtained during the on-site screening in either 2017 (12-month follow-up) or 2018 (24-month follow-up). \newline $^{b}$ Adjusted P values account for the 32 hypotheses tested in this domain.\newline $^{c}$ The standardized treatment effect gives equal weight to each outcome within a domain and includes both the 2017 and 2018 outcomes. It excludes height, BMI, total cholesterol / HDL cholesterol, LDL cholesterol, and the four binary measures."
		
		if `table_num' > 15 local fn "$^{a}$ This table reports interaction effects for the effect of the wellness program. For reference, the table also includes the group means and effects of program eligibility from Table 3 reported in the main text. All outcome variables were obtained during the on-site screening in either 2017 (12-month follow-up) or 2018 (24-month follow-up). \newline $^{b}$ Adjusted P values account for the 32 hypotheses tested in this domain.\newline $^{c}$ The standardized treatment effect gives equal weight to each outcome within a domain and includes both the 2017 and 2018 outcomes. It excludes height, BMI, total cholesterol / HDL cholesterol, LDL cholesterol, and the four binary measures. \newline $^{d}$ N/A indicates a censored value."
		
		* Export into latex
		texsave * using "$Wellness_Public_JAMA/results/tables/etable`table_num'.tex", hlines(1) headerlines("`headerlines'") nofix nonames title("`title'") footnote("`fn'", size("tiny")) size("tiny") landscape  preamble("\usepackage{caption}" "\captionsetup{labelformat=empty}") align(lCClCClCC) replace	
	}
	


	
	
	
	


	**# Table 4 & eTable 6 & eTable 9 & eTable 12 & eTable 15 & eTable 18 & eTable 21 & eTable 24

	clear
	frame change default
	
	* Open the main dataset
	use "$WellnessPublic/data/stata/claims.dta", clear
	
	
	* Variable label
	foreach v of numlist 17 18{
		label variable diabetes_0816_07`v' "Diabetes"
		label variable hypertension_0816_07`v' "Hypertension"
		label variable hyperlipidemia_0816_07`v' "Hyperlipidemia"
		label variable pos_office_outpatient_0816_07`v' "Office/outpatient (# days with at least 1 claim)"
		label variable pos_hospital_0816_07`v' "Inpatient (# days with at least 1 claim)"
		label variable pos_er_critical_0816_07`v' "ER (# days with at least 1 claim)"
	}
	
	* Generate AnalysisID variable
	gen AnalysisID = _n
	rename Strata_claims Strata
	
	* Make regression table
	regression_table

	
	* Dx outcomes
	local CC_dx_2016 diabetes_1015_0716 hypertension_1015_0716 hyperlipidemia_1015_0716
	local CC_dx_2017 diabetes_0816_0717 hypertension_0816_0717 hyperlipidemia_0816_0717
	local CC_dx_2018 diabetes_0816_0718 hypertension_0816_0718 hyperlipidemia_0816_0718

	* POS outcomes
	local pos_2016 pos_office_outpatient_1015_0716 pos_hospital_1015_0716 pos_er_critical_1015_0716
	local pos_2017 pos_office_outpatient_0816_0717 pos_hospital_0816_0717 pos_er_critical_0816_0717
	local pos_2018 pos_office_outpatient_0816_0718 pos_hospital_0816_0718 pos_er_critical_0816_0718

	* Weights
	local covg_2016 covg_1015_0716
	local covg_2017 covg_0816_0717
	local covg_2018 covg_0816_0718
	
	
	add_group_label "Medical diagnoses, 2017, %"
	group_mean_sd `CC_dx_2017' [aw=`covg_2017'], treated(treat) format(%3.1f) 
	
	add_group_label "Medical diagnoses, 2018, %"
	group_mean_sd `CC_dx_2018' [aw=`covg_2018'], treated(treat) format(%3.1f) 

	
	add_group_label "Medical utilization, 2017"
	group_mean_sd `pos_2017' [aw=`covg_2017'], treated(treat) format(%3.2f) 
	
	add_group_label "Medical utilization, 2018"
	group_mean_sd `pos_2018' [aw=`covg_2018'], treated(treat) format(%3.2f) 
	
	
	* ITT
	
	itt_effect `CC_dx_2017' [aw=`covg_2017'], treated(treat) controls(i.Strata `CC_dx_2016' `pos_2016') format(%3.2f) 
	itt_effect `CC_dx_2018' [aw=`covg_2018'], treated(treat) controls(i.Strata `CC_dx_2016' `pos_2016') format(%3.2f) 
	
	itt_effect `pos_2017' [aw=`covg_2017'], treated(treat) controls(i.Strata `CC_dx_2016' `pos_2016') format(%3.2f) 
	itt_effect `pos_2018' [aw=`covg_2018'], treated(treat) controls(i.Strata `CC_dx_2016' `pos_2016') format(%3.2f) 
	
	
	* wyoung
	
	* Dx outcomes
	local family_models
	foreach y in `CC_dx_2017' `CC_dx_2018' {
	
		if strpos("`y'","0717") local wt covg_0816_0717
		else                    local wt covg_0816_0718
		
		local model _regress `y' treat `CC_dx_2016' `pos_2016' i.Strata [aw=`wt'], robust
		local family_models `"`family_models' "`model'""'

	}
	
	wy_testing_itt `family_models', treated(treat) rep($nboot)

	* POS outcomes
	local family_models
	foreach y in `pos_2017' `pos_2018' {
	
		if strpos("`y'","0717") local wt covg_0816_0717
		else                    local wt covg_0816_0718
		
		local model _regress `y' treat `CC_dx_2016' `pos_2016' i.Strata [aw=`wt'], robust
		local family_models `"`family_models' "`model'""'
	}

	wy_testing_itt `family_models', treated(treat) rep($nboot)
	
	
	
	* Standardized treatment effect
	add_group_label "Standardized treatment effect"
	ste_itt `CC_dx_2017' `CC_dx_2018', treated(treat) controls(i.Strata `CC_dx_2016' `pos_2016') format(%3.2f)  weights
	ste_itt `pos_2017' `pos_2018', treated(treat) controls(i.Strata `CC_dx_2016' `pos_2016') format(%3.2f)  weights
	

	* Add labels
	frame change result
	replace name = "Medical diagnoses" in 19
	replace name = "Medical utilization" in 20
	
	
	* Combine columns
	combine_columns group1_point group1_pct group1
	combine_columns group0_point group0_pct group0
	gen effect_ci = effect +" "+ ci, after(ci)
	drop effect ci
	
	* Revise for latex
	replace name = subinstr(name, "%", "\%", .)
	replace name = subinstr(name, "_", "\_", .)
	replace name = subinstr(name, "#", "\#", .)
	replace name = subinstr(name, ">", "$>$", .)
	replace name = subinstr(name, "≥", "$\geq$", .)
	replace name = "\hspace{1em} " + name if !missing(pval)
	replace name = "Outcome" in 1

	
	* Footnote in the table
	replace adj_pval = adj_pval + "$^{b}$" in 1
	replace name = name + "$^{c}$" in 18

	
	* Title and footnote
	local title "Table 4. Mean Values and Effect of Wellness Program on Medical Diagnoses and Utilization$^{a}$" 
	local headerlines "& \multicolumn{2}{c}{Group mean (SD)} & \multicolumn{3}{c}{Effect of Wellness Program Eligibility}" "\cmidrule(lr){2-3} \cmidrule(lr){4-6}"

	local fn "$^{a}$ This table reports effects of the wellness program. All regressions included stratification variables, baseline medical diagnoses, and baseline medical utilization as controls. All regressions were weighted by the employee's number of months of insurance coverage in the post-intervention period. The 2017 period is defined as August 2016 to July 2017, and the 2018 period is defined as August 2016 to July 2018. The sample size of the regressions ranged from 3,164 to 3,167.\newline $^{b}$ Adjusted P values account for the number of hypotheses tested in each domain. We tested 6 hypotheses in the medical diagnoses domain and 6 hypotheses in the medical utilization domain. \newline $^{c}$ The standardized treatment effect gives equal weight to each outcome within a domain and includes both the 2017 and 2018 outcomes."
	
	* Export into latex
	texsave * using "$Wellness_Public_JAMA/results/tables/table4.tex", hlines(1) headerlines("`headerlines'") nofix nonames title("`title'") footnote("`fn'", size("tiny")) size("tiny") preamble("\usepackage{caption}" "\captionsetup{labelformat=empty}") align(lCClCC)  replace
	

	************************
	*** eTable 6
	************************
	
	* Add columns
	add_iv_columns
	
	
	* iv effect
	iv_effect `CC_dx_2017' [aw=`covg_2017'], endog(completed_screening_nomiss_2016) instrument(treat) controls(i.Strata `CC_dx_2016' `pos_2016') format(%3.2f) 
	iv_effect `CC_dx_2018' [aw=`covg_2018'], endog(completed_screening_nomiss_2016) instrument(treat) controls(i.Strata `CC_dx_2016' `pos_2016') format(%3.2f) 
	
	iv_effect `pos_2017' [aw=`covg_2017'], endog(completed_screening_nomiss_2016) instrument(treat) controls(i.Strata `CC_dx_2016' `pos_2016') format(%3.2f) 
	iv_effect `pos_2018' [aw=`covg_2018'], endog(completed_screening_nomiss_2016) instrument(treat) controls(i.Strata `CC_dx_2016' `pos_2016') format(%3.2f) 
	
	
	* wy_testing_iv
	
	local family_models_iv
	foreach y in `CC_dx_2017' `CC_dx_2018' {
	
		if strpos("`y'","0717") local wt covg_0816_0717
		else                    local wt covg_0816_0718

		local model_iv ivreg2 `y' (completed_screening_nomiss_2016 = treat) i.Strata `CC_dx_2016' `pos_2016' [aw=`wt'], robust partial(i.Strata `CC_dx_2016' `pos_2016')
		local family_models_iv `"`family_models_iv' "`model_iv'""'

	}
	
	
	wy_testing_iv  `family_models_iv', treated(completed_screening_nomiss_2016) rep($nboot)
	
	
	
	
	local family_models_iv
	foreach y in `pos_2017' `pos_2018' {
	
		if strpos("`y'","0717") local wt covg_0816_0717
		else                    local wt covg_0816_0718
		
		local model_iv ivreg2 `y' (completed_screening_nomiss_2016 = treat) i.Strata `CC_dx_2016' `pos_2016' [aw=`wt'], robust partial(i.Strata `CC_dx_2016' `pos_2016')
		local family_models_iv `"`family_models_iv' "`model_iv'""'
	}
	
	wy_testing_iv  `family_models_iv', treated(completed_screening_nomiss_2016) rep($nboot)
	
	
	* ste_iv
	ste_iv `CC_dx_2017' `CC_dx_2018', endog(completed_screening_nomiss_2016) instrument(treat) controls(i.Strata `CC_dx_2016' `pos_2016') format(%3.2f)  weights row(19)	
	ste_iv `pos_2017' `pos_2018', endog(completed_screening_nomiss_2016) instrument(treat) controls(i.Strata `CC_dx_2016' `pos_2016') format(%3.2f)  weights row(20)
	
	
	* Combine columns
	frame change result
	gen iv_effect_ci = iv_effect +" "+ iv_ci, after(iv_ci)
	drop iv_effect iv_ci

	
	* Title and footnote
	local title "eTable 6. Local Mean Treatment Effect of Wellness Program on Medical Diagnoses and Utilization$^{a}$" 
	local headerlines "& \multicolumn{2}{c}{Group mean (SD)} & \multicolumn{3}{c}{Effect of Wellness Program Eligibility} & \multicolumn{3}{c}{Effect of Wellness Program Participation}" "\cmidrule(lr){2-3} \cmidrule(lr){4-6} \cmidrule(lr){7-9}"

	local fn "$^{a}$ This table reports effects of program eligibility and local mean treatment effects of program participation. All regressions included stratification variables, baseline medical diagnoses, and baseline medical utilization as controls. All regressions were weighted by the employee's number of months of insurance coverage in the post-intervention period. The 2017 period is defined as August 2016 to July 2017, and the 2018 period is defined as August 2016 to July 2018. The sample size of the regressions ranged from 3,164 to 3,167. \newline $^{b}$ Adjusted P values account for the number of hypotheses tested in each domain. We tested 6 hypotheses in the medical diagnoses domain and 6 hypotheses in the medical utilization domain. \newline $^{c}$ The standardized treatment effect gives equal weight to each outcome within a domain and includes both the 2017 and 2018 outcomes."
	
	* Export into latex
	texsave * using "$Wellness_Public_JAMA/results/tables/etable6.tex", hlines(1) headerlines("`headerlines'") nofix nonames title("`title'") footnote("`fn'", size("tiny")) size("tiny") landscape  preamble("\usepackage{caption}" "\captionsetup{labelformat=empty}") align(lCClCClCC)  replace
	


	************************
	*** Heterogeneity tables
	************************
	
	local het_group "male age50 white AP CS salaryM2"	
	local title1 "Male"
	local title2 "Age 50 and Over"
	local title3 "White"
	local title4 "Academic Professional Employees"
	local title5 "Civil Service Employees"
	local title6 "Above Median Salary"
	local table_numlist "9 12 15 18 21 24"
	
	
	foreach i of numlist 1/6{
		local het: word `i' of `het_group'
		local table_num: word `i' of `table_numlist'
	
		* Drop iv columns and create new iv columns
		drop iv_*
		add_iv_columns
		
		* Heterogeneity itt (`het')
		capture itt_subgroup  `CC_dx_2017'  [aw=`covg_2017'], treated(treat) subgroup(`het') controls(i.Strata `CC_dx_2016' `pos_2016') format(%3.2f)
		capture itt_subgroup  `CC_dx_2018'  [aw=`covg_2018'], treated(treat) subgroup(`het') controls(i.Strata `CC_dx_2016' `pos_2016') format(%3.2f)
		

		capture itt_subgroup  `pos_2017'  [aw=`covg_2017'], treated(treat) subgroup(`het') controls(i.Strata `CC_dx_2016' `pos_2016') format(%3.2f)
		capture itt_subgroup  `pos_2018'  [aw=`covg_2018'], treated(treat) subgroup(`het') controls(i.Strata `CC_dx_2016' `pos_2016') format(%3.2f)
		frame change result
		foreach v of varlist iv_effect iv_pval iv_adj_pval{
			replace `v' = "N/A" in 3/17 if !missing(adj_pval) & `table_num' > 16
		}

			

		
		* wyoung
		frame change default
		capture gen interaction = `het' * treat
		local family_models
		foreach y in  `CC_dx_2017' `CC_dx_2018' {
			
				if strpos("`y'","0717") local wt covg_0816_0717
				else                    local wt covg_0816_0718
			
			local model _regress `y' i.`het' i.treat interaction `CC_dx_2016' `pos_2016' i.Strata [aw = `wt'], robust
			local family_models `"`family_models' "`model'""'
		}
		
		capture wy_testing_iv `family_models',  treated(interaction) rep($nboot)


		* POS outcomes
		local family_models
		foreach y in `pos_2017' `pos_2018' {
		
			if strpos("`y'","0717") local wt covg_0816_0717
			else                    local wt covg_0816_0718
			
			local model _regress `y' i.`het' i.treat interaction `CC_dx_2016' `pos_2016' i.Strata [aw = `wt'], robust
			local family_models `"`family_models' "`model'""'
		}

		capture wy_testing_iv `family_models', treated(interaction) rep($nboot)
		
		capture drop interaction
		
		
		
		* Heterogeneity ste_itt (`het')
		capture ste_itt_subgroup `CC_dx_2017' `CC_dx_2018', treated(treat) subgroup(`het') row(19) controls(i.Strata  `CC_dx_2016' `pos_2016') format(%3.2f) weights
		capture ste_itt_subgroup `pos_2017' `pos_2018', treated(treat) subgroup(`het') row(20) controls(i.Strata  `CC_dx_2016' `pos_2016') format(%3.2f) weights
		frame result: replace iv_effect = "N/A" if `table_num' > 16 in 19/20
		frame result: replace iv_effect = iv_effect + "$^{d}$" in 3 if `table_num' > 16
		
		* Combine columns
		frame change result
		gen iv_effect_ci = iv_effect +" "+ iv_ci, after(iv_ci)
		drop iv_effect iv_ci
		
		
		* Title and footnote
		local title "eTable `table_num'. Heterogeneity: `title`i'': Interaction Effect of Wellness Program on Health Beliefs and Self-Reported Health Behaviors$^{a}$" 
		local headerlines "& \multicolumn{2}{c}{Group mean (SD)} & \multicolumn{3}{c}{Effect of Wellness Program Eligibility (Main Effect)} & \multicolumn{3}{c}{Effect of Wellness Program Eligibility (Interaction Effect)}" "\cmidrule(lr){2-3} \cmidrule(lr){4-6} \cmidrule(lr){7-9}"

		if `table_num' <= 16 local fn "$^{a}$ This table reports interaction effects for the effect of the wellness program. For reference, the table also includes the group means and effects of program eligibility from Table 4 reported in the main text. All regressions included stratification variables, baseline medical diagnoses, and baseline medical utilization as controls. All regressions and means were weighted by the employee's number of months of insurance coverage in the post-intervention period. The 2017 period is defined as August 2016 to July 2017, and the 2018 period is defined as August 2016 to July 2018. \newline $^{b}$ Adjusted P values account for the number of hypotheses tested in each domain. We tested 6 hypotheses in the medical diagnoses domain and 6 hypotheses in the medical utilization domain.\newline $^{c}$ The standardized treatment effect gives equal weight to each outcome within a domain and includes both the 2017 and 2018 outcomes."
		
		if `table_num' > 16 local fn "$^{a}$ This table reports interaction effects for the effect of the wellness program. For reference, the table also includes the group means and effects of program eligibility from Table 4 reported in the main text. All regressions included stratification variables, baseline medical diagnoses, and baseline medical utilization as controls. All regressions and means were weighted by the employee's number of months of insurance coverage in the post-intervention period. The 2017 period is defined as August 2016 to July 2017, and the 2018 period is defined as August 2016 to July 2018. \newline $^{b}$ Adjusted P values account for the number of hypotheses tested in each domain. We tested 6 hypotheses in the medical diagnoses domain and 6 hypotheses in the medical utilization domain.\newline $^{c}$ The standardized treatment effect gives equal weight to each outcome within a domain and includes both the 2017 and 2018 outcomes. \newline $^{d}$ N/A indicates a censored value."
		
		* Export into latex
		texsave * using "$Wellness_Public_JAMA/results/tables/etable`table_num'.tex", hlines(1) headerlines("`headerlines'") nofix nonames title("`title'") footnote("`fn'", size("tiny")) size("tiny") landscape preamble("\usepackage{caption}" "\captionsetup{labelformat=empty}") align(lCClCClCC) replace
	}




	
	
	
	**# eTable 1
	* Frame
	clear
	frame change default

	* Load the dataset
	use "$WellnessPublic/data/stata/biometrics.dta", clear
	
	* Generate AnalysisID variable
	gen AnalysisID = _n
	rename Strata_biometrics Strata
	
	* Variable labels
	foreach year of numlist 2017 2018{
		label variable self_weight_`year'		 "Weight, kg"
		label variable self_height_`year'	 "Height, cm"
		label variable self_cholesterol_`year' "Chance of high cholesterol, %"
		label variable self_bp_`year'			 "Chance of high blood pressure, %"
		label variable self_glucose_`year'	 "Chance of impaired glucose, %"
		label variable self_bmi_`year'		 "Chance of BMI > 30, %"
		
		label variable has_pcp_`year'         "Has primary physician"
		label variable tobacco_none_`year'    "No tobacco use"
		label variable activity_any_`year'    "Exercise 1+ times/week"
		label variable activity_more_`year'   "Exercise 3+ times/week"
		label variable activity_20min_`year'  "Exercise for 20 minutes"
		label variable activity_40min_`year'  "Exercise for 40 minutes"
		label variable anxiety_some_`year'    "Never or sometimes anxious/depressed"
		label variable anxiety_never_`year'   "Never anxious/depressed"
		
		label variable height_`year'   	"Height, cm"
		label variable weight_`year'  		"Weight, kg"
		label variable waist_`year'   		"Waist, cm"
		label variable bmi_`year'   		"BMI"
		label variable systolic_`year' 	"Systolic"
		label variable diastolic_`year'	 "Diastolic"
		label variable tc_`year'   		"Total cholesterol, mg/dL"
		label variable tchdl_`year'   		"Total cholesterol / HDL cholesterol"
		label variable hdl_`year'   		"HDL cholesterol, mg/dL"
		label variable ldl_`year'   		"LDL cholesterol, mg/dL"
		label variable trg_`year'   		"Triglycerides, mg/dL"
		label variable glucose_`year'  	"Glucose, mg/dL"
		label variable high_bmi_`year'		"Obesity (BMI ≥30)"
		label variable high_bp_`year'		"Hypertension (systolic ≥130 or diastolic ≥80)"
		label variable high_chol_`year'	"High LDL cholesterol (≥100 mg/dL)"
		label variable high_gluc_`year'	"High glucose (≥100 mg/dL)"
	}
	

	
	local expect_vars     "self_height self_weight self_bmi self_cholesterol self_bp self_glucose"
	local survey_vars     "has_pcp tobacco_none activity_any activity_more activity_20min activity_40min anxiety_never anxiety_some"
	local bio_vars     "height weight waist bmi systolic diastolic tc hdl tchdl ldl trg glucose high_bmi high_bp high_chol high_gluc"
	forvalues yyyy = 2017/2017 {
		foreach var_group in expect_vars survey_vars bio_vars {
			local `var_group'_`yyyy'
			foreach var of local `var_group' {
				local `var_group'_`yyyy' ``var_group'_`yyyy'' `var'_`yyyy'
			}
		}
	}
	
	* Build the character table
	character_table
	frame result: gen str mde = ""
	
	* Mean and sd
	add_group_label "Health beliefs"
	group_mean_sd `expect_vars_2017', treated(treat)  format(%3.2f)
	add_group_label "Self‐reported health behaviors, %"
	group_mean_sd `survey_vars_2017', treated(treat) format(%3.2f)
	add_group_label "Biometric outcomes"
	group_mean_sd `bio_vars_2017', treated(treat) format(%3.2f)
	
	
	* MDE
	itt_mde `expect_vars_2017' `survey_vars_2017' `bio_vars_2017', treated(treat) controls(i.Strata) format(%3.2f) 
	
	* Load the new dataset
	use "$WellnessPublic/data/stata/claims.dta", clear
	
	* Generate AnalysisID variable
	gen AnalysisID = _n
	rename Strata_claims Strata
	
	foreach v of numlist 17 18{
		label variable diabetes_0816_07`v' "Diabetes"
		label variable hypertension_0816_07`v' "Hypertension"
		label variable hyperlipidemia_0816_07`v' "Hyperlipidemia"
		label variable pos_office_outpatient_0816_07`v' "Office/outpatient (# days with at least 1 claim)"
		label variable pos_hospital_0816_07`v' "Inpatient (# days with at least 1 claim)"
		label variable pos_er_critical_0816_07`v' "ER (# days with at least 1 claim)"
	}
	
	local CC_dx_2016 "diabetes_1015_0716 hypertension_1015_0716 hyperlipidemia_1015_0716"
	local pos_2016 "pos_office_outpatient_1015_0716 pos_hospital_1015_0716 pos_er_critical_1015_0716"
		
	local covg_2017 covg_0816_0717
	local CC_dx_2017 "diabetes_0816_0717 hypertension_0816_0717 hyperlipidemia_0816_0717"
	local pos_2017 "pos_office_outpatient_0816_0717 pos_hospital_0816_0717 pos_er_critical_0816_0717"
	
	
	
	add_group_label "Medical diagnoses, %"
	group_mean_sd `CC_dx_2017', treated(treat) format(%3.2f)
	add_group_label "Medical utilization"
	group_mean_sd `pos_2017', treated(treat) format(%3.2f)
	
	
	* MDE
	
	itt_mde `CC_dx_2017' `pos_2017' [aw=`covg_2017'], treated(treat) controls(i.Strata `CC_dx_2016' `pos_2016') format(%3.2f)
	

	* Editing the table
	frame change result
	drop in 2
	drop group1_point group1_pct
	replace name = "Outcome" in 1
	replace group0_point = "Mean" in 1
	replace group0_pct = "Stdev" in 1
	replace mde = "MDE" in 1
	
	local N = _N + 4
	set obs `N'
	replace name = "Continuous measures" in 43
	replace name = "Blood pressure, mm Hg" in 44
	replace name = "Lipid panel" in 45
	replace name = "Binary measures, %" in 46
	
	gen temp = _n
	replace temp = 18.5 in 43
	replace temp = 22.5 in 44
	replace temp = 24.5 in 45
	replace temp = 30.5 in 46
	sort temp
	drop temp
	

	* Revise for latex
	replace name = subinstr(name, "%", "\%", .)
	replace name = subinstr(name, "_", "\_", .)
	replace name = subinstr(name, "#", "\#", .)
	replace name = subinstr(name, ">", "$>$", .)
	replace name = subinstr(name, "≥", "$\geq$", .)
	replace name = "\hspace{1em} " + name if !missing(mde)
	replace name = subinstr(name, "1em", "2em", .) in 20/33 
	replace name = subinstr(name, "1em", "2em", .) in 35/38
	replace name = subinstr(name, "2em", "3em", .) in 25/26
	replace name = subinstr(name, "2em", "3em", .) in 28/32
	replace name = "\hspace{1em} " + name in 19
	replace name = "\hspace{1em} " + name in 34
	replace name = "\hspace{2em} " + name in 24
	replace name = "\hspace{2em} " + name in 27
	replace name = "Outcome" in 1

	
	* Title and footnote
	local title "eTable 1. Ex Post Minimum Detectable Effects (MDE)$^{a}$" 

	local fn "$^{a}$ MDE calculated assuming 80\% power, i.e., a probability of 0.8 that effect will be statistically significant at the 95\% level."
	
	* Export into latex
	texsave * using "$Wellness_Public_JAMA/results/tables/etable1.tex", hlines(1) nofix nonames title("`title'") footnote("`fn'", size("scriptsize")) size("scriptsize") preamble("\usepackage{caption}" "\captionsetup{labelformat=empty}")  replace
	
	


	
	
	**# eTable 2 & eTable 3

	* Loop for creating eTable 2 and eTable 3
	clear
	local varlist "screening2017_c screening2018_c"
	local tablelist "eTable2 eTable3"
	local yearlist "2017 2018"
	local treat_num "1409 1205"
	local control_num "595 556"
	local insurance_treat "1030 885"
	local insurance_control "435 406"
	
	foreach v of numlist 1/2{
		
		local var: word `v' of `varlist'
		local table: word `v' of `tablelist'
		local year: word `v' of `yearlist'
		local treat: word `v' of `treat_num'
		local control: word `v' of `control_num'
		local in_treat: word `v' of `insurance_treat'
		local in_control: word `v' of `insurance_control'
		local i = `v' + 1
		
		frame change default
		use "$WellnessPublic/data/stata/participation.dta", clear
		
		keep if `var'==1
	

		* Generate variables
		gen age36 = (age37_49!= 1) & (age50!=1)
		
		* Variable labels 
		label variable age36 "$<$37 yr"
		label variable age37_49 "37-50 yr"
		label variable age50 "$\geq$ 50 yr"
		label variable male "Male"
		label variable white "White"
		capture label variable salary_40k "$<$40,000"
		capture label variable salary_50k "40,000 to $<$50,000"
		capture label variable salary_75k "50,000 to $<$75,000"
		capture label variable salary_over_75k "$\geq$ 75,000"
		label variable faculty "Faculty"
		label variable AP "Academic professional"
		label variable CS "Civil service"
	
		* Create a table
		frame change default
		character_table
		frame change result
		replace group1_point = "(n=`treat')" in 2
		replace group0_point = "(n=`control')" in 2
		
		* Age variables
		add_group_label "Age group, No.(%)"
		group_cnt_pct age36 age37_49 age50, treated(treat)
		capture group_mean_sd age, treated(treat) 
		add_na
		add_missing_label "Age, mean (SD)"
		
		* Sex
		add_group_label "Sex, No.(%)"
		gen female = 1-male
		label variable female "Female"
		group_cnt_pct male, treated(treat)
		group_cnt_pct female, treated(treat)

		* Race
		add_group_label "Race, No.(%)"
		gen nonwhite = 1-white
		label variable nonwhite "Non-white"
		group_cnt_pct white nonwhite, treated(treat)
		frame change result
		frame change default
		
		* Annual salary
		add_group_label "Annual salary, \textdollar, No.(%)"
		capture group_cnt_pct salary_40k salary_50k salary_75k salary_over_75k, treated(treat)
		foreach v in "$<$40,000" "40,000 to $<$50,000" "50,000 to $<$75,000" "$\geq$ 75,000"{
			add_na
			add_missing_label "`v'"
		}
		
		* Employee class
		add_group_label "Employee class, No.(%)"
		group_cnt_pct faculty AP CS, treated(treat)

		
		/*
		label variable diabetes_1015_0716 "Diabetes"
		label variable hypertension_1015_0716 "Hypertension"
		label variable hyperlipidemia_1015_0716 "Hyperlipidemia"
		label variable pos_office_outpatient_1015_0716 "Office/outpatient"
		label variable pos_hospital_1015_0716 "Inpatient"
		label variable pos_er_critical_1015_0716 "ER"
		
		* Health insurance
		local covg_2016 covg_1015_0716
		assert !missing(`covg_2016')
		gen covg_months_2016 = `covg_2016'*10
		assert inlist(covg_months_2016, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
		gen covg_any_2016 = (`covg_2016' > 0)
		group_cnt_pct covg_any_2016, treated(treat) 
		group_mean_sd covg_months_2016, treated(treat) 
		*/
		add_group_label  "Health Alliance insurance, Oct 2015 to Jul 2016"
		foreach v in "Any coverage, No. (%)" "Months of coverage, mean (SD)"{
			add_na
			add_missing_label "`v'"
		}
		
		
		* Insurance claim title
		frame change result
		local N = _N+2
		set obs `N'
		foreach v in 1 0{
			replace group`v'_point = "Insurance Claims" in `N'
		}
		local N = _N+1
		set obs `N'
		replace group1_point = "Subsample (n=`in_treat')" in `N'
		replace group0_point = "Subsample (n=`in_control')" in `N'
		
		/*
		* Medical diagnoses
		add_group_label "Medical diagnoses, No.(%)"
		group_cnt_pct diabetes_1015_0716 hypertension_1015_0716 hyperlipidemia_1015_0716, treated(treat)
		
		* Medical utilization
		add_group_label "Medical utilization, mean (SD), days"
		group_mean_sd pos_office_outpatient_1015_0716 pos_hospital_1015_0716 pos_er_critical_1015_0716, treated(treat)
		*/
		
		
		foreach v in "Medical diagnoses, No.(%)" "Diabetes" "Hypertension" "Hyperlipidemia" "Medical utilization, mean (SD), days" "Office/outpatient" "Inpatient" "ER"{
			add_na
			add_missing_label "`v'"
		}
		frame change result
		foreach v of varlist group1_point group0_point{
			replace `v' = "" if inlist(name, "Medical diagnoses, No.(%)", "Medical utilization, mean (SD), days")
		}
		
		
		* Combine columns
		frame change result
		combine_columns group1_point group1_pct group1
		combine_columns group0_point group0_pct group0

		* Revise for latex
		replace name = subinstr(name, "%", "\%", .)
		replace name = subinstr(name, "_", "\_", .)
		replace name = subinstr(name, "#", "\#", .)
		replace name = "\hspace{1em} " + name if !missing(group1)
		replace group1 = group1 + "$^{b}$" in 7
		
		* Title and footnote
		local title "eTable `i'. Baseline Characteristics, for Participants Who Completed the `year' Biometric Screening$^{a}$" 
		local fn "$^{a}$ Age, salary, and employee class are defined as of June 2016, two months prior to the start of the intervention. Medical diagnoses and medical utilization are measured over the period October 2015 to July 2016 and are derived from the insurance claims subsample, which includes all study participants enrolled in the Health Alliance plan. \newline $^{b}$ N/A indicates a censored value."
		
		* Export into latex
		texsave * using "$Wellness_Public_JAMA/results/tables/`table'.tex", hlines(2 -8) nofix nonames title("`title'") footnote("`fn'")  preamble("\usepackage{caption}" "\captionsetup{labelformat=empty}") replace
		clear
	}

	
	
	

	*------------------------------------------------------------------------------
	**# eTable 25
		* PCP analysis
	*------------------------------------------------------------------------------
	* Change to default frame 
	clear
	frame change default
	
	* Load the dataset
	use "$WellnessPublic/data/stata/claims.dta", clear
	
	* Generate AnalysisID variable
	gen AnalysisID = _n
	rename Strata_claims Strata
	
	* PCP outcomes
	local pcp_vars_2016 pcp_total_visits_1015_0716 pcp_total_office_1015_0716 pcp_any_visits_1015_0716 pcp_any_office_1015_0716
	local pcp_vars_2017 pcp_total_visits_0816_0717 pcp_total_office_0816_0717 pcp_any_visits_0816_0717 pcp_any_office_0816_0717
	local pcp_vars_2018 pcp_total_visits_0816_0718 pcp_total_office_0816_0718 pcp_any_visits_0816_0718 pcp_any_office_0816_0718

	* Weights
	local covg_2016 covg_1015_0716
	local covg_2017 covg_0816_0717
	local covg_2018 covg_0816_0718

	
	* Data labels
	foreach v of varlist pcp_total_visits_1015_0716 pcp_total_visits_0816_0717 pcp_total_visits_0816_0718{
		label variable `v' "Total PCP visits"
	}
	foreach v of varlist pcp_total_office_1015_0716 pcp_total_office_0816_0717 pcp_total_office_0816_0718{
		label variable `v' "Total PCP office visits"
	}
	foreach v of varlist pcp_any_visits_1015_0716 pcp_any_visits_0816_0717 pcp_any_visits_0816_0718{
		label variable `v' "Any PCP visits"
	}
	foreach v of varlist pcp_any_office_1015_0716 pcp_any_office_0816_0717 pcp_any_office_0816_0718{
		label variable `v' "Any PCP office visits"
	}
	
	* Construct regression table
	regression_table
	
	
	* ----------------------------------------------------------------------------
	* Group Mean (SD)
	add_group_label "PCP utilization, 2017"
	group_mean_sd `pcp_vars_2017' [aw=`covg_2017'], treated(treat) format(%3.1f) 
	add_group_label "PCP utilization, 2018"
	group_mean_sd `pcp_vars_2018' [aw=`covg_2018'], treated(treat) format(%3.1f) 

	
	* ----------------------------------------------------------------------------
	* ITT
	itt_effect `pcp_vars_2017' [aw=`covg_2017'], treated(treat) controls(i.Strata `pcp_vars_2016') format(%3.2f) 
	itt_effect `pcp_vars_2018' [aw=`covg_2018'], treated(treat) controls(i.Strata `pcp_vars_2016') format(%3.2f) 

	
	
	* wyoung
	local family_models
	foreach y in `pcp_vars_2017' `pcp_vars_2018' {
	
		if strpos("`y'","0717") local wt covg_0816_0717
		else                    local wt covg_0816_0718
		
		local model _regress `y' treat `pcp_vars_2016' [aw=`wt'], a(Strata) robust
		local family_models `"`family_models' "`model'""'
	}
	
	qui wy_testing_itt `family_models', treated(treat) rep($nboot)
		
	
	* Standardized treatment effect
	add_group_label "Standardized treatment effect"
	
	capture ste_itt `pcp_vars_2017' `pcp_vars_2018', treated(treat) controls(i.Strata `pcp_vars_2016') format(%3.2f) weights
	

	
		
	* ----------------------------------------------------------------------------
	* IV
	add_iv_columns
	
	
	iv_effect `pcp_vars_2017' [aw=`covg_2017'], endog(completed_screening_nomiss_2016) instrument(treat) controls(i.Strata `pcp_vars_2016') format(%3.2f) 
	iv_effect `pcp_vars_2018' [aw=`covg_2018'], endog(completed_screening_nomiss_2016) instrument(treat) controls(i.Strata `pcp_vars_2016') format(%3.2f) 
		
	* wyoung
	local family_models_iv
	foreach y in `pcp_vars_2017' `pcp_vars_2018' {
	
		if strpos("`y'","0717") local wt covg_0816_0717
		else                    local wt covg_0816_0718

		local model_iv ivreg2 `y' (completed_screening_nomiss_2016 = treat) i.Strata `pcp_vars_2016' [aw=`wt'], robust partial(i.Strata `pcp_vars_2016')
		local family_models_iv `"`family_models_iv' "`model_iv'""'
	}
	
	qui wy_testing_iv `family_models_iv', treated(completed_screening_nomiss_2016) rep($nboot)

	* Standardized treatment effect
	ste_iv `pcp_vars_2017' `pcp_vars_2018', endog(completed_screening_nomiss_2016) instrument(treat) controls(i.Strata `pcp_vars_2016') format(%3.2f) row(13)
	add_missing_label "PCP utilization"

	* ----------------------------------------------------------------------------
	* Revise table

	
	* Combine columns
	frame change result
	combine_columns group1_point group1_pct group1
	combine_columns group0_point group0_pct group0
	gen effect_ci = effect +" "+ ci, after(ci)
	gen iv_effect_ci = iv_effect + " " + iv_ci, after(iv_ci)
	drop effect ci iv_effect iv_ci

	* Add missing labels
	foreach i of numlist 1/2{
		add_group_label "Continuous measures"
		add_group_label "Binary measures, %"
	}
	
	* Sorting
	frame change result
	gen temp=_n
	replace temp = 2.5 in 14
	replace temp = 4.5 in 15
	replace temp = 7.5 in 16
	replace temp = 9.5 in 17
	sort temp
	drop temp
	
	
	* Revise for latex
	replace name = subinstr(name, "%", "\%", .)
	replace name = subinstr(name, "_", "\_", .)
	replace name = subinstr(name, "#", "\#", .)
	replace name = "\hspace{2em} " + name if !missing(pval)
	replace name = "\hspace{1em} " + name if inlist(name, "Continuous measures","Binary measures, \%" )
	replace adj_pval = adj_pval + "$^{b}$" in 1
	replace name = name + "$^{c}$" in 16
	replace name = "Outcome" in 1
		
	
		
	* Title and footnote
	local title "eTable 25. Mean Values and Effect of Wellness Program on Primary Care Physician (PCP) Utilization$^{a}$" 
	local headerlines "& \multicolumn{2}{c}{Mean Value} & \multicolumn{3}{c}{Effect of Wellness Program Eligibility} & \multicolumn{3}{c}{Effect of Wellness Program Participation}" "\cmidrule(lr){2-3} \cmidrule(lr){4-6} \cmidrule(lr){7-9}"
	local fn "$^{a}$ This table reports effects of program eligibility and local mean treatment effects of program participation. All regressions included stratification variables as controls. All regressions included stratification variables and baseline PCP utilization as controls. All regressions and means were weighted by the employee's number of months of insurance coverage in the postintervention period. The 2017 period is defined as August 2016 to July 2017, and the 2018 period is defined as August 2016 to July 2018. \newline $^{b}$ Adjusted P values account for the 8 hypotheses tested in this domain. \newline $^{c}$ The standardized treatment effect gives equal weight to each outcome within a domain and includes both the 2017 and 2018 outcomes."
	
	* Export into latex
	texsave * using "$Wellness_Public_JAMA/results/tables/etable25.tex", hlines(1) headerlines("`headerlines'")  nofix nonames title("`title'") footnote("`fn'", size("tiny")) size("tiny") landscape  preamble("\usepackage{caption}" "\captionsetup{labelformat=empty}") align(lCClCClCC) replace
		
	clear frames
** EOF
	
	
	