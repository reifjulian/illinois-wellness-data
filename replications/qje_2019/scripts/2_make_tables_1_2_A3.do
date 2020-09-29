************
* SCRIPT:   2_make_tables_1_2_A3.do
* PURPOSE:  Creates Table 1, Table 2, and Appendix Tables 3a-3d
************

local texsave_settings "replace autonumber nofix"

*****************************
*** Balance tables 1a and 1b
*****************************

tempfile descriptive_stats t

use "$WellnessPublic/data/stata/online_surveys.dta", clear
count if treat==0
local Control_N = r(N)

count if treat==1
local Treatment_N = r(N)

append using "$WellnessPublic/data/stata/claims.dta"
append using "$WellnessPublic/data/stata/firm_admin.dta"
append using "$WellnessPublic/data/stata/marathon.dta"
append using "$WellnessPublic/data/stata/participation.dta"
	
***
* Define variable sets for main analysis: strata vars, survey health vars, survey utilization vars, and claims vars
***

* Core variable sets (correspond to families used for multiple hypothesis testing)
local spending_var "spend_0715_0716"
local weighting_var "covg_0715_0716"

local strata_vars "male age50 age37_49 white salaryQ1 salaryQ2 salaryQ3 faculty AP"

local svy_hvars "everscreen_0716 active_0716 active_try_0716 cursmk_0716 othersmk_0716 formsmk_0716 drink_0716 drinkhvy_0716 chronic_0716 health1_0716 health2_0716 problems_0716 energy_0716 ehealth_0716 overweight_0716 badhealth_0716 sedentary_0716"
local svy_uvars "druguse_0716 physician_0716 hospital_0716"
local svy_prodvars "sickdays_0716 hrsworked50_0716 jobsatisf1_0716 jobsatisf2_0716 mgmtsafety_0716"

local claims_vars "`spending_var' spendOff_0715_0716 spendHosp_0715_0716 spendRx_0715_0716 nonzero_spend_0715_0716"

local admin_prodvars "sickleave_0815_0716 salary_0616"
local admin_hvars "marathon_2014_2016 gym_0815_0716"

* Aggregated sets (admin followed by survey)
local medical_spending "`claims_vars' `svy_uvars'"
local employment_productivity "`admin_prodvars' `svy_prodvars'"
local health_behaviors "`admin_hvars' `svy_hvars'"

* salary is censored; set equal to a random number so that code does not break
assert mi(salary_0616)
replace salary_0616 = uniform() if !mi(sickleave_0815_0716)

* Public use datasets above were stacked, thus duplicating the strata variables. Set it so that we only have one copy of these vars
foreach v of local strata_vars {
   replace `v' = . if mi(salaryQ4)
}

preserve 

***
* Calculate unweighted means of each var for each group, and test for equality across control and treatment groups
***
local run_no = 1
qui foreach v in `strata_vars' `svy_hvars' `svy_uvars' `svy_prodvars' `claims_vars' `admin_prodvars' `admin_hvars' {

	* Sample size for the variable
	count if !mi(`v')
	local sample_size = `r(N)'
	
	count if mi(`v') & inlist(treat,0,1)
	local num_missing = r(N)

	* Calculate means for all groups
	reg `v' ibn.treat, nocons robust
	
	* Test for equality across those enrolled in study
	test i0.treat==i1.treat
	local pval = `r(p)'
		
	* Round spending estimates to nearest dollar for better display in table
	regsave
	keep var coef
	if (strpos("`v'","spend") | "`v'"=="salary_0616") & !strpos("`v'","nonzero") replace coef = round(coef)
	
	gen outcome="`v'"
	replace var = subinstr(var,".treat","",.)
	reshape wide coef, i(outcome) j(var) string
	
	* Label results and save to a tempfile
	gen run_no = `run_no'
	gen pval = `pval'
	gen num_miss = `num_missing'
	gen sample_size = `sample_size'
	
	if `run_no'>1 append using `descriptive_stats'
	save `descriptive_stats', replace
	
	local run_no = `run_no'+1
	restore, preserve
}
restore, not

***
* Joint balance tests: for each panel, are the regressors jointly predictive of enrollment into treatment/control groups? 
* Regress indicator on all panel variables. Do this simultaneously for all 7 groups using seemingly unrelated regression
***

foreach panel in A B C {

	     if "`panel'"=="A" local joint_test_vars "`strata_vars'"
	else if "`panel'"=="B" local joint_test_vars "`svy_hvars' `svy_uvars' `svy_prodvars'"
	else if "`panel'"=="C" local joint_test_vars "`claims_vars'"
	else if "`panel'"=="D" local joint_test_vars "`admin_prodvars' `admin_hvars'"
	else error 1
	
	reg treat `joint_test_vars', robust
	test `joint_test_vars'
	
	local joint_test_`panel' = string(`r(p)',"%5.3f")
	local sample_size_`panel' = e(N)
	
}

* Panel D is omitted due to censored variables
local joint_test_D = -1
local sample_size_D = 4834

***
* Format results from descriptive statistics and joint balance tests, and output the table
***
use "`descriptive_stats'", clear
sort run_no
drop run_no

format coef* pval %5.3f
ren coef0bn   Control
ren coef1     Treatment
gen nonparticipants = .
order outcome nonparticipants Control

set obs `=_N+6'
replace outcome = "Sample size"                                     if _n==_N-4
replace outcome = "Joint balance test for panel A (\(p\)-value)"    if _n==_N-3
replace outcome = "Joint balance test for panel B (\(p\)-value)"    if _n==_N-2
replace outcome = "Joint balance test for panel C (\(p\)-value)"    if _n==_N-1
replace outcome = "Joint balance test for panel D (\(p\)-value)"    if _n==_N
foreach group in Control Treatment {
	replace `group' = ``group'_N'      if outcome=="Sample size"
}

replace pval            = `joint_test_A'        if outcome=="Joint balance test for panel A (\(p\)-value)"
replace pval            = `joint_test_B'        if outcome=="Joint balance test for panel B (\(p\)-value)"
replace pval            = `joint_test_C'        if outcome=="Joint balance test for panel C (\(p\)-value)"
replace pval            = `joint_test_D'        if outcome=="Joint balance test for panel D (\(p\)-value)"

replace sample_size     = `sample_size_A'       if outcome=="Joint balance test for panel A (\(p\)-value)"
replace sample_size     = `sample_size_B'       if outcome=="Joint balance test for panel B (\(p\)-value)"
replace sample_size     = `sample_size_C'       if outcome=="Joint balance test for panel C (\(p\)-value)"
replace sample_size     = `sample_size_D'       if outcome=="Joint balance test for panel D (\(p\)-value)"

tostring nonparticipants-pval, format(%12.2fc) gen(tmp1 tmp2 tmp3 tmp4) force
tostring nonparticipants-pval, format(%12.3fc) replace force

local run_no=1
foreach v of varlist nonparticipants Control Treatment {
	replace `v' = subinstr(`v',".000","",.)
	replace `v' = tmp`run_no' if inlist(outcome,"gym_0815_0716","sickleave_0815_0716")
	local run_no = `run_no'+1
}
drop num_miss tmp*
tostring sample_size, format(%12.0gc) replace force

ingap 1 10 10 35 35 40 40
replace outcome = "A. Stratification Variables"                   in 1  if mi(outcome)
replace outcome = "B. 2016 Survey Variables"                      in 12 if mi(outcome)

replace outcome = "C. Health Claims Variables (2015--2016)"       in 39 if mi(outcome)
replace outcome = "D. Health Behavior and Productivity Variables" in 46 if mi(outcome)

cleanvars outcome
replace outcome = subinstr(outcome," [admin]","",1)
replace outcome = subinstr(outcome," [survey]","",1)
replace outcome = "\ \ \ \ " + outcome if strpos(outcome,"Drug s") | strpos(outcome,"Office s") | strpos(outcome,"Hospital s") | strpos(outcome,"Other s")

label var outcome ""
label var nonparticipants "Not in Study"
label var pval "\(p\)-value"
label var sample_size "Sample Size"
foreach v of varlist Control Treatment {
	label var `v' `v'
}
foreach v of varlist * {
	replace `v' = "" if `v'=="."
}

* Insert "N/A" for statistics that can't be calculated in public use dataset
replace nonparticipants = "N/A" in 2/10
replace nonparticipants = "N/A" in 52
replace nonparticipants = "N/A" in 40/44
replace sample_size     = "N/A" in 40/44
replace nonparticipants = "N/A" in 47/50
replace sample_size     = "N/A" in 47/50

foreach v in Control Treatment pval {
	replace `v' = "N/A" if outcome=="Annual salary (dollars)"
}
replace pval = "N/A" if outcome=="Joint balance test for panel D (\(p\)-value)"

* Balance table 1a: Panels A and B; Balance table 1b: Panels C, and D
gen table1 = inrange(_n,1,38) | inrange(_n,52,54)
gen table2 = !table1 | strpos(outcome,"Sample size")

local fn_part1 "This table replicates"
local fn_part2 "from Jones, Molitor, and Reif (QJE 2019) using public-use data. N/A indicates a censored value."

local fn "`fn_part1' Table 1a `fn_part2'"
texsave outcome-sample_size using "`t'" if table1, `texsave_settings' hlines(1 12 -3) bold("A. " "B. " ) preamble("\renewcommand\thetable{1a}") title("Means of Study Variables at Baseline") varlabels headerlines("& & \multicolumn{3}{c}{Enrolled in Study} &" "\cmidrule(lr){3-5}") size(scriptsize) footnote(`"`fn'"', size(scriptsize)) marker(tab:balance_tests1) landscape geometry("left=.2in,right=.2in")
filefilter "`t'" "$WellnessPublic_QJE_2019/results/tables/table_1a.tex", from("\BScmidrule(lr){3-5} \BStabularnewline") to("\BScmidrule(lr){3-5}") replace

* Second table: Panels C and D
local fn "`fn_part1' Table 1b  `fn_part2'"
texsave outcome-sample_size using "`t'" if table2, `texsave_settings' hlines(1 -8 -3) bold("C. " "D. ") preamble("\renewcommand\thetable{1b}") title("Means of Study Variables at Baseline, Continued") varlabels headerlines("& & \multicolumn{3}{c}{Enrolled in Study} &" "\cmidrule(lr){3-5}") size(scriptsize) footnote(`"`fn'"', size(scriptsize)) marker(tab:balance_tests2) landscape geometry("left=.2in,right=.2in")
filefilter "`t'" "$WellnessPublic_QJE_2019/results/tables/table_1b.tex", from("\BScmidrule(lr){3-5} \BStabularnewline") to("\BScmidrule(lr){3-5}") replace


*****************************
*** Table 2 and Appendix Tables 3a-3d
*****************************

use "$WellnessPublic/data/stata/online_surveys.dta", clear
append using "$WellnessPublic/data/stata/claims.dta"
append using "$WellnessPublic/data/stata/firm_admin.dta"
append using "$WellnessPublic/data/stata/marathon.dta"
append using "$WellnessPublic/data/stata/participation.dta"

tempfile selection_results
tempfile selection_average

* Selection regressions are only estimated using those assigned to treatment
keep if treat==1

***
* Define variable sets for main analysis: strata vars, survey health vars, survey utilization vars, and claims vars
***

* Core variable sets (correspond to families used for multiple hypothesis testing)
local spending_var "spend_0715_0716"
local weighting_var "covg_0715_0716"

local strata_vars "male age50 age37_49 white salaryQ1 salaryQ2 salaryQ3 faculty AP"

local svy_hvars "everscreen_0716 active_0716 active_try_0716 cursmk_0716 othersmk_0716 formsmk_0716 drink_0716 drinkhvy_0716 chronic_0716 health1_0716 health2_0716 problems_0716 energy_0716 ehealth_0716 overweight_0716 badhealth_0716 sedentary_0716"
local svy_uvars "druguse_0716 physician_0716 hospital_0716"
local svy_prodvars "sickdays_0716 hrsworked50_0716 jobsatisf1_0716 jobsatisf2_0716 mgmtsafety_0716"

local claims_vars "`spending_var' spendOff_0715_0716 spendHosp_0715_0716 spendRx_0715_0716 nonzero_spend_0715_0716"

local admin_prodvars "sickleave_0815_0716 salary_0616"
local admin_hvars "marathon_2014_2016 gym_0815_0716"

local index_prodvars "prod_index_yr0"

* Aggregated sets (admin followed by survey)
local medical_spending "`claims_vars' `svy_uvars'"
local employment_productivity "`admin_prodvars' `svy_prodvars'"
local health_behaviors "`admin_hvars' `svy_hvars'"

gen constant=1

* salary is censored; set equal to a random number so that code does not break
assert mi(salary_0616)
replace salary_0616 = uniform() if !mi(sickleave_0815_0716)

* Public use datasets above were stacked, thus duplicating the number of strata variables. Set it so that we only have one copy of these vars
foreach v of local strata_vars {
   replace `v' = . if mi(activity_s_c_yr2)
}

***
* For each completion outcome (screening, HRA, activities), see if there is selection on a variable
***
preserve
local run_no = 1
local replace1 replace

* Loop over each family
foreach group in strata_vars claims_vars svy_uvars admin_prodvars svy_prodvars admin_hvars svy_hvars index_prodvars {

	* domain_weight_list will hold a list of weights to be used by the wyoung command. This will allow regressions within a family of domains to have different weights
	local domain_weight_list ""

	* Loop over each variable in the domain
	qui foreach xvar in ``group'' {

		* Use weights if LHS var is continuous measure of medical spending or sickleave
		if strpos("`xvar'","spend") & !strpos("`xvar'","nonzero") {
			local weights "[aw=`weighting_var']"
			local domain_weight_list "`domain_weight_list' `weighting_var'"
		}
		else if "`xvar'"=="sickleave_0815_0716" {
			local weights "[aw=sickdays_eligible_0815_0716]"
			local domain_weight_list "`domain_weight_list' sickdays_eligible_0815_0716"
		}
		else {
			local weights ""
			local domain_weight_list "`domain_weight_list' constant"
		}

		summ `xvar' `weights', meanonly
		local mean_xvar=r(mean)
		
		local replace replace
		foreach p in hra_c_yr1 {
		
			* Regression to see if participation measure, on average, is predictive of a variable.
			gen pvar = `p'
			reg `xvar' i.pvar `weights', robust		
			
			* Use different -format- option for spending vars when saving results
			local tblformat "%5.3f"
			if (strpos("`xvar'","spend") | strpos("`xvar'","salary_")) & !strpos("`xvar'","nonzero") local tblformat "%5.1f"
			regsave 1.pvar using `selection_average', t p table("`p'", asterisk(10 5 1) format(`tblformat')) addlabel(outcome,"`xvar'", regressor,"`p'") `replace'
		
			local num_obs = e(N)
			local replace append
			restore, preserve
		}
		
		use "`selection_average'", clear

		* Add placeholder for the family-wise p-values (calculated below)
		keep if strpos(var,"_coef") | strpos(var,"_stderr") | strpos(var,"_pval")
		replace var = subinstr(var,"1.pvar","`xvar'",.)
		set obs `=_N+1'
		replace var = "`xvar'_wypval" if _n==_N
		
		gen run_no = `run_no'
		gen mean_sample = `mean_xvar'
		gen num_obs = `num_obs'
		gen outcome = "`xvar'"
		gen group = "`group'"
	
		if `run_no'>1 append using "`selection_results'"
		save "`selection_results'", replace
		
		local run_no = `run_no'+1
		restore, preserve	
	}

	* Calculate and store family-wise p-values for this domain
	foreach p in hra_c_yr1  {
			
		wyoung ``group'', familyp(`p') bootstraps($nboot) seed(11) cmd("_regress OUTCOMEVAR `p' [aw=WEIGHTVAR], robust") weights(`domain_weight_list') replace
		keep outcome p pwyoung
		gen group = "`group'"
		ren p pval_`p'
		
		merge 1:m outcome group using "`selection_results'", assert(match using)

		* Ensure we are merging on right outcome / regressor combination
		destring `p', force gen(tmp)
		assert abs(pval_`p' - tmp)<.1 if strpos(var,"_pval") & _merge==3
		
		tostring pwyoung, replace force format(%5.3f)
		replace `p' = pwyoung if strpos(var,"wypval") & mi(`p')

		drop pwyoung tmp _merge
		save "`selection_results'", replace
		restore, preserve
	}
}
restore

* Sort order: coef, std error, family-wise pval
use "`selection_results'", clear
sort run_no var
save "$WellnessPublic_QJE_2019/results/intermediate/selection_results.dta", replace

use "$WellnessPublic_QJE_2019/results/intermediate/selection_results.dta", clear

foreach v in hra_c_yr1 {
	replace `v' = "("+`v'+")" if strpos(var,"_stderr")
	replace `v' = "["+`v'+"]" if strpos(var,"_pval")
	replace `v' = "["+`v'+"]" if strpos(var,"_wypval")
}

replace var = "" if strpos(var,"stderr")
replace var = subinstr(var,"_coef","",.)
replace var = "p" if strpos(var,"_pval")
replace var = "" if strpos(var,"_wypval")
drop if var=="p"
drop run_no pval_*
order var mean_sample num_obs
replace num_obs = . if mi(var)
replace mean_sample = . if mi(var)
tostring mean_sample, format(%7.0fc) gen(tmp) force
tostring mean_sample, format(%5.3f) replace force
replace mean_sample = tmp if (inlist(var,"salary_0616") | strpos(var,"spend")) & !strpos(var,"nonzero")
tostring num_obs, format(%7.0fc) replace force
drop tmp

* Replace salary result with "N/A" since this variable was censored
replace hra_c_yr1   = "N/A" if outcome=="salary_0616"
replace mean_sample = "N/A" if var=="salary_0616"
replace num_obs     = "N/A" if var=="salary_0616"

* Label variables and write out the tables
cleanvars var
foreach v of varlist * {
	cap replace `v' = "" if `v'=="."
}

* Activity variables are not available in the public use dataset
gen activity_f_c = "N/A"
gen activity_s_c = "N/A"

label var var "Selection Variable"
label var mean_sample "Mean"
label var num_obs "\(N\)"
cap label var screening_r "Registered Screening"
label var hra_c_yr1 "Completed Screening and HRA"
cap label var activity_f_r "Registered Fall Activity" 
label var activity_f_c "Completed Fall Activity"
cap label var activity_s_r "Registered Spring Activity"
label var activity_s_c "Completed Spring Activity"

* Selection table in main text: claims vars, behavior vars, and salary vars. The rest will be in appendix tables.
gen main_text_vars =  inlist(outcome,"spend_0715_0716","nonzero_spend_0715_0716","salary_0616","salaryQ1","marathon_2014_2016","gym_0815_0716","hrsworked50_0716","sickleave_0815_0716","prod_index_yr0")
drop outcome

preserve
foreach g in strata_vars medical_spending employment_productivity health_behaviors main_text_vars {
		
	* Strata vars (Appendix Table 3a)
	if "`g'"=="strata_vars" {	
		
		keep if inlist(group,"strata_vars")
		
		local title = "Selection on Strata Variables"
		local options = "size(scriptsize)"
		local filename "appendix_table_3a"
		local fn_num "A.3a"
	}
	
	* Spending measures (Appendix Table 3b)
	if "`g'"=="medical_spending" {
	
		keep if inlist(group,"claims_vars","svy_uvars")

		replace var = "\ \ \ \ " + var if strpos(var,"Drug s") | strpos(var,"Office s") | strpos(var,"Hospital s") | strpos(var,"Other s")
	
		local title = "Selection on Health Care Utilization Variables"
		local filename "appendix_table_3b"
		local options = "size(scriptsize)"
		local fn_num "A.3b"
	}	
	
	* Employment and productivity measures (Appendix Table 3c)
	if "`g'"=="employment_productivity" {

		keep if inlist(group,"admin_prodvars","svy_prodvars","index_prodvars")
	
		local title = "Selection on Employment and Productivity Variables"
		local options = "size(scriptsize)"
		local filename "appendix_table_3c"
		local fn_num "A.3c"
	}

	* Health and behaviors (Appendix Table 3d)
	if "`g'"=="health_behaviors" {
	
		keep if inlist(group,"admin_hvars","svy_hvars")

		local title = "Selection on Health and Behavior Variables"
		local options = "size(tiny)"
		local filename "appendix_table_3d"
		local fn_num "A.3d"
	}
	
	* Main text table: claims, income, behaviors (Table 2)
	if "`g'"=="main_text_vars" {
		
		keep if main_text_vars

		sortobs, values(4/9 25/27 10/12 16/18 13/15 1/3 19/24)
		
		ingap 1 7 7 22 22
		replace var = "\textbf{A. Baseline Medical Spending}"        in 1 if mi(var)
		replace var = "\textbf{B. Baseline Productivity}"            in 9 if mi(var)		
		replace var = "\textbf{C. Baseline Health Behaviors}"        in 26 if mi(var)		
		
		local title = "Selection on Medical Spending, Productivity, and Health Behaviors"
		local options = "size(scriptsize) hlines(1 9 -6)"
		local filename "table_2"
		local fn_num "2"
	}	
	
	local app ""
	if strpos("`filename'","appendix") local app "Appendix "
	local fn_part1 "This table replicates `app'Table"
	local fn_part2 "from Jones, Molitor, and Reif (QJE 2019) using public-use data. The Westfall-Young \(p\)-values, reported in brackets, do not exactly replicate the values from the original paper due to censoring in the public-use data. N/A indicates a censored value."
	local fn "`fn_part1' `fn_num' `fn_part2'"

	drop group main_text_vars
	texsave using "$WellnessPublic_QJE_2019/results/tables/`filename'.tex", `texsave_settings' preamble("\renewcommand\thetable{`fn_num'}") varlabels title("`title'") footnote("`fn'") `options' 
	restore, preserve
}

restore, not


** EOF
