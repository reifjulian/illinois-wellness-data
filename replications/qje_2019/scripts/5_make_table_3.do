************
* SCRIPT:   5_make_table_3.do
* PURPOSE:  Create Table 3
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


* Variables to report
if 1 {	
  * 12-month variables
  local spend_vars_table spend_0816_0717 spendRx_0816_0717 spendOff_0816_0717 spendHosp_0816_0717 nonzero_spend_0816_0717
  local job_vars_table promotion_0616_0717 terminated_0717 sickleave_0816_0717 mgmtsafety_0717 prod_index_yr1
  local health_vars_table marathon_2017 gym_0816_0717 everscreen_0717
  local keep_vars_0816_0717 `spend_vars_table' `job_vars_table' `health_vars_table'
  
  * Longer-run variables
  local spend_vars_table spend_0816_0119 spendRx_0816_0119 spendOff_0816_0119 spendHosp_0816_0119 nonzero_spend_0816_0119
  local job_vars_table promotion_0616_0119 terminated_0119 sickleave_0816_0119 mgmtsafety_0718 prod_index_yr2
  local health_vars_table marathon_2018 gym_0816_0119 everscreen_0718
  local keep_vars_0816_0119 `spend_vars_table' `job_vars_table' `health_vars_table'
}

*---------------------------------------------------------------------------------------------------------------------------------------
*** ITT: short- and longer-run; mean, no-controls, post-lasso
*---------------------------------------------------------------------------------------------------------------------------------------
if 1 {
  * Specifications to report in the ITT table
  if 1 {
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
  
  * Make tables of selected results for short- and longer-run time frames
  foreach timeframe in 0816_0717 0816_0119 {
    * local timeframe 0816_0717
    * local timeframe 0816_0119
    
    * Make list of outcomes to keep in the table
    clear
    local nobs = wordcount("`keep_vars_`timeframe''")
    set obs `=`nobs'+1'
    gen outcome = "buffer" if _n==1
    if `nobs'>0 {
      tokenize "`keep_vars_`timeframe''"
      forvalues i = 1/`nobs' {
        replace outcome = "`1'" if _n==`i'+1
        macro shift
      }
    }
    gen outcome_base = outcome
    foreach str in _0717 _0616 _0816 _2017 _yr1 _0119 _0718 _2018 _yr2 {
      replace outcome_base = subinstr(outcome_base, "`str'", "", .)
    }
    tempfile keeprows
    save `keeprows'
    
    * Load MASTER table of effects
    local results_intermediate_files "$WellnessPublic_QJE_2019/results/intermediate/`timeframe'"
    local treat_effects_all "`results_intermediate_files'/treat_effects/treat_effects_all"
    use "`treat_effects_all'.dta", clear
    replace outcome = "buffer" if missing(outcome)
    gen sortorder = _n
    
    * Keep selected outcomes
    merge m:1 outcome using "`keeprows'", assert(match master) keep(match) nogen noreport
    sort sortorder
    replace var = subinstr(var, "IL Marathon/10K/5K 2017 [admin]", "IL Marathon/10K/5K [admin]", 1)
    replace var = subinstr(var, "IL Marathon/10K/5K 2018 [admin]", "IL Marathon/10K/5K [admin]", 1)
    
    * Drop extra rows
    drop if outcome=="buffer" & outcome[_n-1]=="buffer" & missing(var)
    drop if outcome=="buffer" & !missing(var) & (outcome[_n+1]=="buffer" | missing(var[_n+1]))
    drop if outcome=="buffer" & (_n==_N)
    cap replace itt_mean = "N="+itt_mean if var=="N_obs"
    
    * Keep selected specifications
    if `show_mean'==0 drop itt_mean
    if `show_itt'==0  drop itt_no_controls itt_strata_fe itt_dbl_lasso
    if `show_iv1'==0  drop iv1_no_controls iv1_strata_fe iv1_dbl_lasso
    if `show_ols'==0  drop ols_no_controls ols_strata_fe ols_dbl_lasso
    foreach effect in itt iv1 ols {
      foreach model_spec in `effect'_no_controls `effect'_strata_fe `effect'_dbl_lasso {
        cap replace `model_spec' = "N="+`model_spec' if var=="N_obs"
      }
    }
    
    * Drop extra specs
    foreach stat in stderr pval wy_pval N_obs {
      if `show_`stat''==0 drop if var=="`stat'"
      cap replace var = "" if var=="`stat'"
    }
    
    drop outcome time_horizon itt_strata_fe
     * drop T
    replace sortorder = _n
    order sortorder outcome_base
    foreach v in outcome_base var itt_mean itt_no_controls itt_dbl_lasso {
      rename `v' `v'_`timeframe'
    }
    
    tempfile itt_`timeframe'
    save `itt_`timeframe''
  }
  
  * Combine short- and longer-run results into single table
  use `itt_0816_0717', clear
  merge 1:1 sortorder using `itt_0816_0119', assert(match) nogen
  assert outcome_base_0816_0717 == outcome_base_0816_0119
  assert var_0816_0717 == var_0816_0119
  drop sortorder outcome_base_????_???? var_0816_0119
  
  * Create LaTeX table
  if 1 {
    local filename itt
    local using using "$WellnessPublic_QJE_2019/results/tables/`filename'.tex"
    local texsave_settings "replace autonumber nofix"
    local marker marker("tab:`filename'")
    local preamble preamble("\renewcommand\thetable{3}")
    local title title(`"Treatment Effects (Intention-to-Treat)"')
    local headerlines headerlines("\addlinespace[1.0ex] & \multicolumn{3}{c}{First Year (12 months)} & \multicolumn{3}{c}{Longer-run (24--30 months)}" "\cmidrule(lr){2-4} \cmidrule{5-7}\addlinespace[-2.5ex]")
    local fn footnote("Notes: This table replicates Table 3 from Jones, Molitor, and Reif (QJE 2019) using public-use data. The Westfall-Young \(p\)-values, reported in brackets, do not exactly replicate the values from the original paper due to censoring in the public-use data. N/A indicates a censored value.")
    local size = "size(scriptsize)"
  
    * Export as LaTeX table
    local hline1 = 2
    local hline2 = `hline1' + wordcount("`spend_vars_table'")*5 - 1
    local hline3 = `hline2' + 2
    local hline4 = `hline3' + wordcount("`job_vars_table'")*5 - 1
    local hline5 = `hline4' + 2
    local hlines hlines(`hline1' `hline3' `hline5')
    texsave `using', `texsave_settings' varlabels `marker' `hlines' `headerlines' headlines("\setlength{\tabcolsep}{6pt}") `preamble' `title' `fn' `size'
  
    * Additional LaTeX table tweaks
    local texfile "$WellnessPublic_QJE_2019/results/tables/`filename'.tex"
    text_replace "`texfile'" "\BSaddlinespace[\BSbelowrulesep]" ""
    text_replace "`texfile'" "\BStabularnewline\W&&&&&& \BStabularnewline" "\BStabularnewline\BSaddlinespace[0.75ex]"
    text_replace "`texfile'" "\BSbottomrule" "\BSbottomrule\BSaddlinespace[-1.7ex]"
  }
  
  * Make general replacements
  text_replace "`texfile'" "+ Medical Spending----------------------------&&&&" "\BSmulticolumn{3}{l}{\BStextbf{A. Medical Spending`class'}}"
  text_replace "`texfile'" "+ Employment and Productivity-----------------&&&&" "\BSmulticolumn{3}{l}{\BStextbf{B. Employment and Productivity`class'}}"
  text_replace "`texfile'" "+ Health Status and Behaviors-----------------&&&&" "\BSmulticolumn{3}{l}{\BStextbf{C. Health Status and Behaviors`class'}}"
  text_replace "`texfile'" "+ Medical Utilization (Quantity)--------------&&&&" "\BSmulticolumn{3}{l}{\BStextbf{D. Medical Utilization (Quantity)`class'}}"
  text_replace "`texfile'" "\BStabularnewline\BSaddlinespace[0.75ex]\W\BSmulticolumn{5}{l}" "\BStabularnewline\BSaddlinespace[0ex]\W\W\BSmulticolumn{5}{l}"
  text_replace "`texfile'" "\BSmidrule\BSaddlinespace[1.5ex]\W&&&&&& \BStabularnewline\W\BSmulticolumn{5}{l}" "\BSmidrule\BSaddlinespace[1.5ex]\W\W\BSmulticolumn{5}{l}"
  text_replace "`texfile'" "Drug spending [admin]" "\BS \BS \BS \BS Drug spending [admin]"
  text_replace "`texfile'" "Office spending [admin]" "\BS \BS \BS \BS Office spending [admin]"
  text_replace "`texfile'" "Hospital spending [admin]" "\BS \BS \BS \BS Hospital spending [admin]"
  text_replace "`texfile'" "Other spending [admin]" "\BS \BS \BS \BS Other spending [admin]"
  text_replace "`texfile'" "N=" "\BStextit{N=}"
  text_replace "`texfile'" "T=" "\BStextit{T=}"
}


*---------------------------------------------------------------------------------------------------------------------------------------
*** Rename table files for public use
*---------------------------------------------------------------------------------------------------------------------------------------

* Table 3: Treatment Effects (Intention-to-Treat)
cp $WellnessPublic_QJE_2019/results/tables/itt.tex $WellnessPublic_QJE_2019/results/tables/table_3.tex, replace
sleep 1000
rm $WellnessPublic_QJE_2019/results/tables/itt.tex 

** EOF
