************
* SCRIPT:   3_make_tables_4_A4_A5.do
* PURPOSE:  Create Table 4, Appendix Tables A.4a-g, and Appendix Tables A.5a-g
************

*---------------------------------------------------------------------------------------------------------------------------------------
*** Setup
*---------------------------------------------------------------------------------------------------------------------------------------

* Redo Lasso controls, regressions, and Westfall-Young p-values?
*	1 = redo all
* 0 = redo only if results files do not exist
local redo_all 0

* Program to replace text string from tex file
program drop _all
program define text_replace, nclass
  local tex_file `1'
  local from `2'
  local to `3'
  tempfile t
  filefilter "`tex_file'" "`t'", from("`from'") to("`to'") replace
  capture copy "`t'" "`tex_file'", replace
  while _rc > 0 {
    noisily di "...sleep..."
    sleep 1000
    capture copy "`t'" "`tex_file'", replace
  }
end

* Time frame of output variables
local timeframe 0816_0717

* List of censored variables
local censored_vars salaryRaise_0616_0717 titleChange_0616_0717

* Number of Westfall-Young bootstraps
assert !missing($nboot)

*---------------------------------------------------------------------------------------------------------------------------------------
*** ITT/IV/OLS
*---------------------------------------------------------------------------------------------------------------------------------------
qui {
* Step 0.a.  Code chunk locals and options
if 1 {

  * Input data
  use "$WellnessPublic/data/stata/online_surveys.dta", clear
  append using "$WellnessPublic/data/stata/claims.dta"
  append using "$WellnessPublic/data/stata/firm_admin.dta"
  append using "$WellnessPublic/data/stata/marathon.dta"
  append using "$WellnessPublic/data/stata/participation.dta"
  tempfile wellness_analysis_dta
  save `wellness_analysis_dta'

	local results_intermediate_files "$WellnessPublic_QJE_2019/results/intermediate/`timeframe'"
	cap mkdir "`results_intermediate_files'"
    cap mkdir "`results_intermediate_files'/treat_effects"
      cap mkdir "`results_intermediate_files'/treat_effects/estimates"
      cap mkdir "`results_intermediate_files'/treat_effects/wyoung"

  local results_tables "$WellnessPublic_QJE_2019/results/tables/`timeframe'"
  cap mkdir "`results_tables'"

	* Re-estimate ITT/IV/OLS regressions? 1=Yes, 0=No
	local redo_regressions `redo_all'

	* Re-estimate Westfall-Young p-values? 1=Yes, 0=No
	local redo_wyoung `redo_all'
}

* Step 0.b.  Select observations to use from wellness_analysis.dta, save to tempfile `selection_data'
if 1 {
	* Drop individuals NOT in the treatment or control groups
	use if inlist(treat, 0, 1) using "`wellness_analysis_dta'", clear
	assert _N == 4834 * 5
  
  * HRA completion variable with missing coded as zero
  gen hra_c_nomiss = hra_c_yr1
  replace hra_c_nomiss = 0 if missing(hra_c_nomiss)
  label var hra_c_nomiss "Completed 2016 Wellsource HRA (missing coded as 0)"
  
  * Weights for unweighted regressions
  gen aw = 1
  
  * For censored variables, missing coded as random
  foreach var of local censored_vars {
    set seed 100
    assert missing(`var')
    replace `var' = runiform()
  }
  
	tempfile selection_data
	saveold `selection_data', replace
}

* Step 0.c.  Define outcomes & domains, treatment, instruments, Lasso controls, and regression weights
if 1 {
	* (Endogenous) Treatment Variable
	local d hra_c_nomiss

	* Instruments
	local instruments treat

	* Define lists of potential outcomes
	local spend_vars_admin    spend_0816_0717 spendRx_0816_0717 spendOff_0816_0717 spendHosp_0816_0717 nonzero_spend_0816_0717
	local spend_vars_survey   druguse_0717 physician_0717 hospital_0717
	local job_vars_admin      salaryRaise_0616_0717 promotion_0616_0717 titleChange_0616_0717 terminated_0717 sickleave_0816_0717
	local job_vars_survey     sickdays_0717 hrsworked50_0717 jobsatisf1_0717 jobsatisf2_0717 mgmtsafety_0717 happywork_0717 presenteeism_0717 productive_0717 promotion_0717 jobsearch1_0717 jobsearch2_0717
	local job_vars_index      prod_index_yr1
	local health_vars_admin   marathon_2017 gym_0816_0717
	local health_vars_survey  everscreen_0717 active_0717 active_try_0717 cursmk_0717 drink_0717 drinkhvy_0717 chronic_0717 health1_0717 health2_0717 problems_0717 energy_0717 ehealth_0717 overweight_0717 badhealth_0717 sedentary_0717

	* Quantity outcomes for Appendix (not in public use data)
	local quantity_vars_admin 
  
	* Domains of outcomes
	local spend_vars "`spend_vars_admin' `spend_vars_survey'"
	local job_vars "`job_vars_admin' `job_vars_survey' `job_vars_index'"
	local health_vars "`health_vars_admin' `health_vars_survey'"
	local quantity_vars "`quantity_vars_admin'"

	* Define weighting variable, if any, for each outcome
	foreach domain in spend_vars job_vars health_vars quantity_vars {
		foreach y in ``domain'' {
			if (strpos("`y'", "spend")==1) & (strpos("`y'", "0816_0717")>0)           local aw_`y' "covg_0816_0717"
			else if (strpos("`y'", "visit")==1) & (strpos("`y'", "0816_0717")>0)      local aw_`y' "covg_0816_0717"
			else if (strpos("`y'", "sickleave")==1) & (strpos("`y'", "0816_0717")>0)  local aw_`y' "sickdays_eligible_0816_0717"
			else                                                                      local aw_`y'
		}
	}
}
 
* Step 1. [N/A FOR PUBLIC USE REPLICATION] Select controls that predict y or d, for both IV and OLS samples 

* Step 2.  Estimate ITT/IV/OLS regressions, saving regression results
foreach domain in spend_vars job_vars health_vars quantity_vars {
foreach y in ``domain'' {
  noisily di "domain: `domain', y = `y'"
	
  * Notation
	*	y:   Outcome variable
	*	d:   (Endogenous) treatment variable
	*	aw:  Regression weights - leave empty for unweighted regression (aw=1)

	* Output data and results files
	local regsave_file "`results_intermediate_files'/treat_effects/estimates/all_estimates_`y'.dta"

	* Skip loop iteration unless either (a) an output/results file does not exist, or (b) redo_regressions flag set to 1
	capture confirm file "`regsave_file'"
	if _rc==0 & `redo_regressions'==0 {
		di "Using prior regression results for y = `y': all estimation files exist, and redo_regressions flag = `redo_regressions'"
		continue
	}
	else {
		di "Calculating regression results for y = `y': _rc=`=_rc', redo_regressions=`redo_regressions'"
	}

	* Weights
	local aw `aw_`y''
	if missing("`aw'") {
		local aw aw
	}
  
	* Load the  data, adding in controls selected by Lasso
	use "`selection_data'", clear
	if ("`aw'"=="aw") assert `aw'==1
  
	* Table settings
	local format format(%5.3f)
	if inlist(1, strpos("`y'", "spend"), strpos("`y'", "salary_")) local format format(%9.1f)
	local tbl_settings "`format' parentheses(stderr) asterisk(10 5 1)"
	local tbl_settings_mean "`format' parentheses(stderr)"
	tempfile `y'


	* ----------------------------------------------------------------------------------------
	* 0. Mean
	* ----------------------------------------------------------------------------------------
	cap drop rhs
	gen rhs = 1
	qui _regress `y' rhs [aw=`aw'], noconst robust
	estimates save "`results_intermediate_files'/treat_effects/estimates/ITT_Mean_`y'", replace
	regsave rhs using ``y'', p nose table(itt_mean,`tbl_settings_mean') replace

	* ----------------------------------------------------------------------------------------
	* 1. ITT
	* ----------------------------------------------------------------------------------------
	local effect IV
	replace rhs = treat

	* (a) No Controls
	qui _regress `y' rhs [aw=`aw'], robust
	estimates save "`results_intermediate_files'/treat_effects/estimates/ITT_NoControl_`y'", replace
	regsave rhs using ``y'', p table(itt_no_controls,`tbl_settings') addlabel(wy_pval,"[]") append

	* ----------------------------------------------------------------------------------------
	* 2. IV (z = treat)
	* ----------------------------------------------------------------------------------------
	local effect IV
	local z treat
	replace rhs = `d'

	* (a) No Controls
	qui ivregress 2sls `y' (rhs = i.`z') [aw=`aw'], robust
	estimates save "`results_intermediate_files'/treat_effects/estimates/IV_z`z'_NoControl_`y'", replace
	regsave rhs using ``y'', p table(iv1_no_controls,`tbl_settings') addlabel(wy_pval,"[]") append

	* ----------------------------------------------------------------------------------------
	* 3. OLS (treatment group only)
	* ----------------------------------------------------------------------------------------
	local effect OLS
	replace rhs = `d'

	* (a) No Controls
	qui _regress `y' rhs [aw=`aw'] if treat, robust
	estimates save "`results_intermediate_files'/treat_effects/estimates/OLS_NoControl_`y'", replace
	regsave rhs using ``y'', p table(ols_no_controls,`tbl_settings') addlabel(wy_pval,"[]") append

	* Convert regsaved tempfile to permanent file
	preserve
	use ``y'', clear
	saveold "`regsave_file'", replace
	restore
}
}

* Step 3.  Westfall-Young corrected p-values
foreach domain in spend_vars quantity_vars job_vars health_vars {
foreach group in `domain'_admin `domain'_survey `domain'_index {
if !missing("``group''") {
	* Notation
	*	y:   Outcome variable
	*	aw:  Regression weights - leave empty for unweighted regression (aw=1)

	* Treatment
	local rhs treat

	* Create a "wide" data set with all outcomes in the family, along with relevant Lasso controls
	* Load the lasso data, adding in controls selected by Lasso
	foreach y in ``group'' {
		* Load data
    use "`selection_data'", clear
    gen AnalysisID = _n

		* If no controls were selected, generate a placeholder control equal to 0
		capture confirm variable v_dbl_y_1
		if _rc {
			gen v_dbl_y_1 = 0
		}

		* Rename variables to keep them unique
		foreach var of varlist v_dbl_y_* {
			rename `var' `=subinstr("`var'", "v_dbl_", "v_`y'_", .)'
		}
		
		* Keep variables for use in the post-Lasso regression
		keep AnalysisID `y' `rhs' v_`y'_y_* `aw_`y''

		tempfile wy_`y'
		qui save `wy_`y''
	}
	tokenize ``group''
	while !missing("`*'") {
		if "`*'"=="``group''" use `wy_`1'', clear
		else merge 1:1 AnalysisID using `wy_`1'', assert(match) nogen noreport
		macro shift
	}

	* Models
	local family_models_itt_no_controls
	foreach y in ``group'' {
		* Weights
		if !missing("`aw_`y''") local aw [aw=`aw_`y'']
		else                    local aw

		* (a) No Controls
		local model _regress `y' `rhs' `aw', robust
		local family_models_itt_no_controls `"`family_models_itt_no_controls' "`model'""'
	}

	* Westfall-Young
	qui foreach model_spec in itt_no_controls {
		capture confirm file "`results_intermediate_files'/treat_effects/wyoung/wy_`group'_`model_spec'.dta"
		if _rc | `redo_wyoung'==1 {
			noisily {
				di ""
				di "Calculating Westfall-Young p-values"
				di "Group: `group'"
				di "Model spec: `model_spec'"
			}
			timer on 1
			preserve
			wyoung, familyp(`rhs') bootstraps($nboot) seed(11) cmd(`family_models_`model_spec'') detail replace
			noisily list outcome coef stderr p pwyoung, sep(0)
			saveold "`results_intermediate_files'/treat_effects/wyoung/wy_`group'_`model_spec'.dta", replace
			timer off 1
			timer list 1
			noisily di "Time to complete: " %8.2f `=`r(t1)'/60' " minutes, $nboot bootstraps"
			timer clear 1
			restore
		}
	}
}
}
}

* Step 4.  MASTER table of treatment effects: all outcomes and specifications
if 1 {
	* Append all estimates into a single table
	clear
	gen outcome = ""
	local spend_vars_table `spend_vars'
	local quantity_vars_table `quantity_vars'
	local job_vars_table `job_vars'
	local health_vars_table `health_vars'
	local spend_vars_table_label     "+ Medical Spending----------------------------"
	local quantity_vars_table_label  "+ Medical Utilization (Quantity)--------------"
	local job_vars_table_label       "+ Employment and Productivity-----------------"
	local health_vars_table_label    "+ Health Status and Behaviors-----------------"
	local prev_domain
	foreach domain in spend_vars quantity_vars job_vars health_vars {
		if !missing("``domain'_table'") {
			foreach y in ``domain'_table' {
				append using "`results_intermediate_files'/treat_effects/estimates/all_estimates_`y'.dta"
				ingap -6
				replace outcome = "`y'" if missing(outcome) & (_n>_N-6)
				replace var = "`y'" if var=="rhs_coef"
				replace var = "wy_pval_`y'" if var=="wy_pval"
				sortobs, values(`=_N') before(`=_N-2')
			}
			replace var = "``domain'_table_label'" in `=``prev_domain'_table_nrow'+1'
			ingap `=``prev_domain'_table_nrow'+1'
			local `domain'_table_nrow = _N
			local prev_domain `domain'
		}
	}

	* Merge in Westfall-Young p-values
	preserve
	tempfile wyoung_table
	foreach domain in spend_vars quantity_vars job_vars health_vars {
		foreach group in `domain'_admin `domain'_survey `domain'_index {
			if !missing("``group''") {
				foreach model_spec in itt_no_controls {
					use outcome pwyoung using "`results_intermediate_files'/treat_effects/wyoung/wy_`group'_`model_spec'.dta", clear

					* Format and rename variables for merging into table of results
					replace outcome = "wy_pval_" + outcome
					tostring pwyoung, replace force format(%5.3f)
					replace pwyoung = "[" + pwyoung + "]"
					rename outcome var
					rename pwyoung `model_spec'

					* Save as tempfile
					capture confirm file `wyoung_table'
					if !_rc merge 1:1 var using `wyoung_table', nogen noreport
					save `wyoung_table', replace
				}
			}
		}
	}
	restore
	gen sortorder = _n
	merge m:1 var using `wyoung_table', nogen noreport update replace
	sort sortorder
	drop sortorder

  * Add variable time horizon (months since treatment start, 07/2017)
  gen time_horizon = ""
  order outcome var time_horizon
  gen end_date = substr(outcome, -4, 4) if !missing(outcome) & missing(outcome[_n-1])
  assert inlist(end_date, "0117", "0717", "0816", "0916", "1016", "2017", "_yr1", "")
  replace time_horizon = string(mofd(date(end_date, "M20Y")) - mofd(date("0716", "M20Y")))  if !inlist(end_date, "2017", "")
  replace time_horizon = "12"                                                               if inlist(end_date, "2017", "_yr1") & !strpos(outcome, "marathon")
  replace time_horizon = "12"                                                               if inlist(end_date, "2017") & strpos(outcome, "marathon")
	drop end_date

	* Keep specified regression output stats, and clean row names
	drop if inlist(var, "r2")
	replace var = "pval" if strpos(var, "rhs_pval")
	replace var = "stderr" if strpos(var, "rhs_stderr")
	replace var = "wy_pval" if strpos(var, "wy_pval")
	replace var = "N_obs" if var=="N"
	cleanvars var
  
  * Add in censored results, coded as N/A
  foreach spec in itt iv1 ols {
    foreach controls in dbl_lasso strata_fe {
      capture confirm variable `spec'_`controls'
      assert _rc > 0
      gen `spec'_`controls' = ""
      replace `spec'_`controls' = "N/A" if !missing(`spec'_no_controls)
      order `spec'_`controls', after(`spec'_no_controls)
    }
  }
  foreach var of local censored_vars {
    foreach spec in itt_mean itt_no_controls iv1_no_controls ols_no_controls {
      replace `spec' = "N/A" if outcome == "`var'" & !missing(`spec')
    }
  }
  
	label var outcome "Outcome Variable Name"
	label var var "Outcome Variable"
	label var itt_mean "Mean"
	label var itt_no_controls "No Controls"
	label var itt_strata_fe "Strata FEs"
	label var itt_dbl_lasso "Post-Lasso"
	label var iv1_no_controls "No Controls"
	label var iv1_strata_fe "Strata FEs"
	label var iv1_dbl_lasso "Post-Lasso"
	label var ols_no_controls "No Controls"
	label var ols_strata_fe "Strata FEs"
	label var ols_dbl_lasso "Post-Lasso"

	* Save results table
	local treat_effects_all "`results_intermediate_files'/treat_effects/treat_effects_all"
	save "`treat_effects_all'", replace

	* Export results table as text file
	use "`treat_effects_all'", clear
	gen sortorder = _n
	local addlines 1
	set obs `=_N+1'
	replace sortorder = _n-(_N) if _n>_N-1
	sort sortorder
	drop sortorder

	desc, varlist
	foreach var in `r(varlist)' {
		local lab: variable label `var'
		if inlist("`var'", "var", "outcome", "time_horizon") replace `var' = "`lab'" if _n==1
		if strpos("`var'", "itt")==1 replace `var' = "[ITT] `lab'" if _n==1
		if strpos("`var'", "iv1")==1 replace `var' = "[IV1] `lab'" if _n==1
		if strpos("`var'", "ols")==1 replace `var' = "[OLS] `lab'" if _n==1
	}
	outfile using "`treat_effects_all'.txt", noquote replace wide rjs
}
 
* Step 5. Latex tables
if 1 {
	* List of ITT/IV/OLS tables to create
	local table_type_list IV_OLS_main
	foreach domain in spend_vars quantity_vars job_vars health_vars {
		foreach class in admin survey index {
			if !missing("``domain'_`class''") {
				local table_type_list `table_type_list' ITT_A_`domain'_`class' IV_OLS_A_`domain'_`class'
			}
		}
	}
  
	* Make the tables
	foreach table_type in `table_type_list' {
    * local table_type IV_OLS_main 
    noisily di "`table_type'"
    
		* For appendix tables, determine which group of variables goes in the table
		local group_pos = strpos("`table_type'","ITT_A_")+6
		if `group_pos'==6 local group_pos = strpos("`table_type'","IV_OLS_A_")+9
		if `group_pos'==9 local group
		else local group = substr("`table_type'", `group_pos', .)
		local class = substr("`group'", strrpos("`group'","_")+1, .)
		if !missing("`class'") local class = " [`class']"
		if "`class'" == " [index]" local class = " [admin/survey]"

		* Specify which outcomes, specifications, and statistics to report in the table
		if "`table_type'"=="IV_OLS_main" {
			local spend_vars_table `spend_vars_admin'

      local job_vars_admin_drop titleChange_0616_0717 salaryRaise_0616_0717
      local job_vars_admin_keep : list job_vars_admin - job_vars_admin_drop
			local job_vars_table `job_vars_admin_keep' mgmtsafety_0717 `job_vars_index'

      local health_vars_table `health_vars_admin' everscreen_0717
      local keep_vars `spend_vars_table' `job_vars_table' `health_vars_table'

			* Specifications
			local show_mean 0
			local show_itt 0
			local show_iv1 1
			local show_ols 1

			* Regression statistics
			local show_stderr 1
			local show_pval 0
			local show_wy_pval 0
			local show_N_obs 1
		}
		else if strpos("`table_type'", "ITT_A_") {
			local keep_vars ``group''

			* Specifications
			local show_mean 1
			local show_itt 1
			local show_iv1 0
			local show_ols 0

			* Regression statistics
			local show_stderr 1
			local show_pval 0
			local show_wy_pval 1
			local show_N_obs 1
		}
		else if strpos("`table_type'", "IV_OLS_A_") {
			local keep_vars ``group''

			* Specifications
			local show_mean 0
			local show_itt 0
			local show_iv1 1
			local show_ols 1

			* Regression statistics
			local show_stderr 1
			local show_pval 0
			local show_wy_pval 0
			local show_N_obs 1
		}

		* Make list of outcomes to keep in the table
		clear
		local nobs = wordcount("`keep_vars'")
		set obs `=`nobs'+1'
		gen outcome = "buffer" if _n==1
		if `nobs'>0 {
			tokenize "`keep_vars'"
			forvalues i = 1/`nobs' {
				replace outcome = "`1'" if _n==`i'+1
				macro shift
			}
		}
		tempfile keeprows
		save `keeprows'

		* Load MASTER table of effects, keeping only selected outcomes, specifications, and stats
		use "`treat_effects_all'.dta", clear
    drop time_horizon
		replace outcome = "buffer" if missing(outcome)
		gen sortorder = _n
		merge m:1 outcome using "`keeprows'", assert(match master) keep(match) nogen noreport
		sort sortorder
		drop if outcome=="buffer" & outcome[_n-1]=="buffer" & missing(var)
		drop if outcome=="buffer" & !missing(var) & (outcome[_n+1]=="buffer" | missing(var[_n+1]))
		drop if outcome=="buffer" & (_n==_N)
		if `show_mean'==0 drop itt_mean
		if `show_itt'==0  drop itt_no_controls itt_strata_fe itt_dbl_lasso
		if `show_iv1'==0  drop iv1_no_controls iv1_strata_fe iv1_dbl_lasso
		if `show_ols'==0  drop ols_no_controls ols_strata_fe ols_dbl_lasso
    
    * Revised table format for main IV/OLS table
    if "`table_type'"=="IV_OLS_main" & `show_iv1'==1 drop iv1_strata_fe
    if "`table_type'"=="IV_OLS_main" & `show_ols'==1 drop ols_strata_fe
    
		cap replace itt_mean = "N="+itt_mean if var=="N_obs"
		foreach effect in itt iv1 ols {
			foreach model_spec in `effect'_no_controls {
				cap replace `model_spec' = "N="+`model_spec' if var=="N_obs"
			}
		}
		foreach stat in stderr pval wy_pval N_obs {
			if `show_`stat''==0 drop if var=="`stat'"
			cap replace var = "" if var=="`stat'"
		}
		drop outcome sortorder
		* browse

		* Create LaTeX table
		if "`table_type'"=="IV_OLS_main" {
			local filename iv_ols_`timeframe'
			local using using "`results_tables'/`filename'.tex"
			local texsave_settings "replace autonumber nofix"
			local marker marker("tab:`filename'")
			local preamble preamble("\renewcommand\thetable{4}")
      local title title(`"First-Year Treatment Effects: Experimental versus Observational Estimates"')
			local fn footnote("Notes: This table replicates Table 4 from Jones, Molitor, and Reif (QJE 2019) using public-use data. N/A indicates a censored value.")
			local size = "size(scriptsize)"

			* Export as LaTeX table
			local hline1 = 2
			local hline2 = `hline1' + wordcount("`spend_vars_table'")*4 - 1
			local hline3 = `hline2' + 2
			local hline4 = `hline3' + wordcount("`job_vars_table'")*4 - 1
			local hline5 = `hline4' + 2
			local hlines hlines(`hline1' `hline3' `hline5')
			texsave `using', `texsave_settings' varlabels `marker' `hlines' headlines("\setlength{\tabcolsep}{6pt}") `preamble' `title' `fn' `size'

			* Additional LaTeX table tweaks
			local texfile "`results_tables'/`filename'.tex"
			local line1 &{(1)}&{(2)}&{(3)}&{(4)} \BStabularnewline
			local line2 \BSmidrule
			local line3 & \BSmulticolumn{2}{c}{Experimental (IV)} & \BSmulticolumn{2}{c}{Observational (OLS)} \BStabularnewline
			local line4 \BScmidrule(l){2-3} \BScmidrule(l){4-5}
			text_replace "`texfile'" "`line1' \BSmidrule" "`line1' \n `line2' \n `line3' \n `line4'"
			text_replace "`texfile'" "\BSbottomrule \BSaddlinespace[\BSbelowrulesep]" "\BSbottomrule\BSaddlinespace[-1.5ex]"
		}
		else if strpos("`table_type'", "ITT_A_") {
			local filename appendix_itt_`group'_`timeframe'
			local using using "`results_tables'/`filename'.tex"
			local texsave_settings "replace autonumber nofix"
			local marker marker("tab:`filename'")
			local title title(`"First-Year Treatment Effects (ITT)"')
      if "`group'" == "spend_vars_admin"   local panel a
      if "`group'" == "spend_vars_survey"  local panel b
      if "`group'" == "job_vars_admin"     local panel c
      if "`group'" == "job_vars_survey"    local panel d
      if "`group'" == "job_vars_index"     local panel e
      if "`group'" == "health_vars_admin"  local panel f
      if "`group'" == "health_vars_survey" local panel g
      local preamble preamble("\renewcommand\thetable{A.4`panel'}")
      local fn footnote("Notes: This table replicates Appendix Table A.4`panel' from Jones, Molitor, and Reif (QJE 2019) using public-use data. The Westfall-Young \(p\)-values, reported in brackets, do not exactly replicate the values from the original paper due to censoring in the public-use data. N/A indicates a censored value.")
      local size = "size(scriptsize)"

			* Export as LaTeX table
			local hline1 = 2
			local hlines hlines(`hline1')
			texsave `using', `texsave_settings' varlabels `marker' `hlines' headlines("\setlength{\tabcolsep}{6pt}") `preamble' `title' `fn' `size'

			* Additional LaTeX table tweaks
			local texfile "`results_tables'/`filename'.tex"
			local addspace "1.4ex"
			if "`group'"=="health_vars_survey" local addspace "1.2ex"
			text_replace "`texfile'" "\BSaddlinespace[\BSbelowrulesep]" ""
			text_replace "`texfile'" "\BSmidrule &&&& \BStabularnewline" "\BSmidrule\BSaddlinespace[1.5ex]"
			text_replace "`texfile'" "\BSmidrule\W&&&& \BStabularnewline" "\BSmidrule\BSaddlinespace[1.5ex]"
			text_replace "`texfile'" "\BSbottomrule" "\BSbottomrule\BSaddlinespace[-1.5ex]"
			text_replace "`texfile'" "\W&&&& \BStabularnewline" "\W\BSaddlinespace[`addspace']"
		}
		else if strpos("`table_type'", "IV_OLS_A_") {
			local filename appendix_iv_ols_`group'_`timeframe'
			local using using "`results_tables'/`filename'.tex"
			local texsave_settings "replace autonumber nofix"
			local marker marker("tab:`filename'")
			local title title(`"First-Year Treatment Effects: Experimental versus Observational Estimates"')
			if "`group'" == "spend_vars_admin"   local panel a
      if "`group'" == "spend_vars_survey"  local panel b
      if "`group'" == "job_vars_admin"     local panel c
      if "`group'" == "job_vars_survey"    local panel d
      if "`group'" == "job_vars_index"     local panel e
      if "`group'" == "health_vars_admin"  local panel f
      if "`group'" == "health_vars_survey" local panel g
      local preamble preamble("\renewcommand\thetable{A.5`panel'}")
      local fn footnote("Notes: This table replicates Appendix Table A.5`panel' from Jones, Molitor, and Reif (QJE 2019) using public-use data. N/A indicates a censored value.")
      local size = "size(scriptsize)"

			* Export as LaTeX table
			local hline1 = 2
			local hlines hlines(`hline1')
			texsave `using', `texsave_settings' varlabels `marker' `hlines' headlines("\setlength{\tabcolsep}{6pt}") `preamble' `title' `fn' `size'

			* Additional LaTeX table tweaks
			local texfile "`results_tables'/`filename'.tex"
			local line1 &{(1)}&{(2)}&{(3)}&{(4)}&{(5)}&{(6)} \BStabularnewline
			local line2 \BSmidrule
			local line3 & \BSmulticolumn{3}{c}{Experimental (IV)} & \BSmulticolumn{3}{c}{Observational (OLS)} \BStabularnewline
			local line4 \BScmidrule(l){2-4} \BScmidrule(l){5-7}
			text_replace "`texfile'" "`line1' \BSmidrule" "`line1' \n `line2' \n `line3' \n `line4'"
			local addspace "1.5ex"
			text_replace "`texfile'" "\BSaddlinespace[\BSbelowrulesep]" ""
			text_replace "`texfile'" "\BSmidrule &&&&&& \BStabularnewline" "\BSmidrule\BSaddlinespace[1.5ex]"
			text_replace "`texfile'" "\BSmidrule\W&&&&&& \BStabularnewline" "\BSmidrule\BSaddlinespace[1.5ex]"
			text_replace "`texfile'" "\BSbottomrule" "\BSbottomrule\BSaddlinespace[-1.5ex]"
			text_replace "`texfile'" "\W&&&& \BStabularnewline" "\W\BSaddlinespace[`addspace']"
		}

		* Make general replacements
		text_replace "`texfile'" "+ Medical Spending----------------------------&&&&" "\BSmulticolumn{3}{l}{\BStextbf{A. Medical Spending`class'}}"
		text_replace "`texfile'" "+ Employment and Productivity-----------------&&&&" "\BSmulticolumn{3}{l}{\BStextbf{B. Employment and Productivity`class'}}"
		text_replace "`texfile'" "+ Health Status and Behaviors-----------------&&&&" "\BSmulticolumn{3}{l}{\BStextbf{C. Health Status and Behaviors`class'}}"
		text_replace "`texfile'" "+ Medical Utilization (Quantity)--------------&&&&" "\BSmulticolumn{3}{l}{\BStextbf{D. Medical Utilization (Quantity)`class'}}"
		text_replace "`texfile'" "Drug spending [admin]" "\BS \BS \BS \BS Drug spending [admin]"
		text_replace "`texfile'" "Office spending [admin]" "\BS \BS \BS \BS Office spending [admin]"
		text_replace "`texfile'" "Hospital spending [admin]" "\BS \BS \BS \BS Hospital spending [admin]"
		text_replace "`texfile'" "Other spending [admin]" "\BS \BS \BS \BS Other spending [admin]"
		text_replace "`texfile'" "N=" "\BStextit{N=}"
	}

}
}


*---------------------------------------------------------------------------------------------------------------------------------------
*** Rename table files for public use
*---------------------------------------------------------------------------------------------------------------------------------------

* Table 4: First-Year Treatment Effects: Experimental versus Observational Estimates
cp $WellnessPublic_QJE_2019/results/tables/0816_0717/iv_ols_0816_0717.tex $WellnessPublic_QJE_2019/results/tables/table_4.tex, replace
sleep 1000
rm $WellnessPublic_QJE_2019/results/tables/0816_0717/iv_ols_0816_0717.tex

* Appendix Table A.4a-g: First-Year Treatment Effects (ITT)
cp $WellnessPublic_QJE_2019/results/tables/0816_0717/appendix_itt_spend_vars_admin_0816_0717.tex   $WellnessPublic_QJE_2019/results/tables/appendix_table_4a.tex, replace
cp $WellnessPublic_QJE_2019/results/tables/0816_0717/appendix_itt_spend_vars_survey_0816_0717.tex  $WellnessPublic_QJE_2019/results/tables/appendix_table_4b.tex, replace
cp $WellnessPublic_QJE_2019/results/tables/0816_0717/appendix_itt_job_vars_admin_0816_0717.tex     $WellnessPublic_QJE_2019/results/tables/appendix_table_4c.tex, replace
cp $WellnessPublic_QJE_2019/results/tables/0816_0717/appendix_itt_job_vars_survey_0816_0717.tex    $WellnessPublic_QJE_2019/results/tables/appendix_table_4d.tex, replace
cp $WellnessPublic_QJE_2019/results/tables/0816_0717/appendix_itt_job_vars_index_0816_0717.tex     $WellnessPublic_QJE_2019/results/tables/appendix_table_4e.tex, replace
cp $WellnessPublic_QJE_2019/results/tables/0816_0717/appendix_itt_health_vars_admin_0816_0717.tex  $WellnessPublic_QJE_2019/results/tables/appendix_table_4f.tex, replace
cp $WellnessPublic_QJE_2019/results/tables/0816_0717/appendix_itt_health_vars_survey_0816_0717.tex $WellnessPublic_QJE_2019/results/tables/appendix_table_4g.tex, replace
sleep 1000
rm $WellnessPublic_QJE_2019/results/tables/0816_0717/appendix_itt_spend_vars_admin_0816_0717.tex   
rm $WellnessPublic_QJE_2019/results/tables/0816_0717/appendix_itt_spend_vars_survey_0816_0717.tex  
rm $WellnessPublic_QJE_2019/results/tables/0816_0717/appendix_itt_job_vars_admin_0816_0717.tex     
rm $WellnessPublic_QJE_2019/results/tables/0816_0717/appendix_itt_job_vars_survey_0816_0717.tex    
rm $WellnessPublic_QJE_2019/results/tables/0816_0717/appendix_itt_job_vars_index_0816_0717.tex     
rm $WellnessPublic_QJE_2019/results/tables/0816_0717/appendix_itt_health_vars_admin_0816_0717.tex  
rm $WellnessPublic_QJE_2019/results/tables/0816_0717/appendix_itt_health_vars_survey_0816_0717.tex 

* Appendix Table A.5a-g: First-Year Treatment Effects: Experimental versus Observational Estimates
cp $WellnessPublic_QJE_2019/results/tables/0816_0717/appendix_iv_ols_spend_vars_admin_0816_0717.tex   $WellnessPublic_QJE_2019/results/tables/appendix_table_5a.tex, replace
cp $WellnessPublic_QJE_2019/results/tables/0816_0717/appendix_iv_ols_spend_vars_survey_0816_0717.tex  $WellnessPublic_QJE_2019/results/tables/appendix_table_5b.tex, replace
cp $WellnessPublic_QJE_2019/results/tables/0816_0717/appendix_iv_ols_job_vars_admin_0816_0717.tex     $WellnessPublic_QJE_2019/results/tables/appendix_table_5c.tex, replace
cp $WellnessPublic_QJE_2019/results/tables/0816_0717/appendix_iv_ols_job_vars_survey_0816_0717.tex    $WellnessPublic_QJE_2019/results/tables/appendix_table_5d.tex, replace
cp $WellnessPublic_QJE_2019/results/tables/0816_0717/appendix_iv_ols_job_vars_index_0816_0717.tex     $WellnessPublic_QJE_2019/results/tables/appendix_table_5e.tex, replace
cp $WellnessPublic_QJE_2019/results/tables/0816_0717/appendix_iv_ols_health_vars_admin_0816_0717.tex  $WellnessPublic_QJE_2019/results/tables/appendix_table_5f.tex, replace
cp $WellnessPublic_QJE_2019/results/tables/0816_0717/appendix_iv_ols_health_vars_survey_0816_0717.tex $WellnessPublic_QJE_2019/results/tables/appendix_table_5g.tex, replace
sleep 1000
rm $WellnessPublic_QJE_2019/results/tables/0816_0717/appendix_iv_ols_spend_vars_admin_0816_0717.tex   
rm $WellnessPublic_QJE_2019/results/tables/0816_0717/appendix_iv_ols_spend_vars_survey_0816_0717.tex  
rm $WellnessPublic_QJE_2019/results/tables/0816_0717/appendix_iv_ols_job_vars_admin_0816_0717.tex     
rm $WellnessPublic_QJE_2019/results/tables/0816_0717/appendix_iv_ols_job_vars_survey_0816_0717.tex    
rm $WellnessPublic_QJE_2019/results/tables/0816_0717/appendix_iv_ols_job_vars_index_0816_0717.tex     
rm $WellnessPublic_QJE_2019/results/tables/0816_0717/appendix_iv_ols_health_vars_admin_0816_0717.tex  
rm $WellnessPublic_QJE_2019/results/tables/0816_0717/appendix_iv_ols_health_vars_survey_0816_0717.tex 

* Remove timeframe directory (command ends in error if directory not empty)
local datafiles: dir "$WellnessPublic_QJE_2019/results/tables/0816_0717" files "*.tmp"
foreach datafile of local datafiles {
  rm "$WellnessPublic_QJE_2019/results/tables/0816_0717/`datafile'"
}
sleep 1000
rmdir $WellnessPublic_QJE_2019/results/tables/0816_0717

** EOF
