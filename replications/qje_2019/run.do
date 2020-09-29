**********************
* OVERVIEW
*   This script generates selected tables and figures for the paper:
*       "What Do Workplace Wellness Programs Do? Evidence from the Illinois Workplace Wellness Study" (Damon Jones, David Molitor, and Julian Reif)
*   All raw data are stored in /data
*   All tables and figures are outputted to /replications/qje_2019/results
* 
* SOFTWARE REQUIREMENTS
*   Stata version 16 or newer
*
* TO PERFORM A CLEAN RUN, DELETE THE FOLLOWING FOLDER:
*   $WellnessPublic/replications/results
**********************

**********************
* Parameters defined by user
**********************

* "WellnessPublic" points to the root directory, which contains the subfolders "data" and "replications"
global WellnessPublic ""

* Number of bootstraps for Westfall-Young
* QJE published paper used nboot = 10000. Runtime is approximately 3 hours on a Windows 10 Desktop with Stata MP-6 and an i7-8700 CPU 3.20 GHz processor
* Use smaller value to run replication code faster. Runtime is approximately 1 minute with nboot = 2.
global nboot = 10000

**********************
**********************

* Confirm that the global for the root directory has been defined
assert !missing("$WellnessPublic")

* Define location of the replication folder
global WellnessPublic_QJE_2019 "$WellnessPublic/replications/qje_2019"

* Initialize log and record system parameters
clear 
set more off
cap mkdir "$WellnessPublic_QJE_2019/scripts/logs"
cap log close
local datetime : di %tcCCYY.NN.DD!-HH.MM.SS `=clock("$S_DATE $S_TIME", "DMYhms")'
local logfile "$WellnessPublic_QJE_2019/scripts/logs/`datetime'.log.txt"
log using "`logfile'", text

di "Begin date and time: $S_DATE $S_TIME"
di "Stata version: `c(stata_version)'"
di "Updated as of: `c(born_date)'"
di "Variant:       `=cond( c(MP),"MP",cond(c(SE),"SE",c(flavor)) )'"
di "Processors:    `c(processors)'"
di "OS:            `c(os)' `c(osdtl)'"
di "Machine type:  `c(machine_type)'"

* All required Stata packages are available in the /libraries/stata folder
adopath ++ "$WellnessPublic_QJE_2019/scripts/libraries/stata"
mata: mata mlib index

* Stata programs are stored in /programs
adopath ++ "$WellnessPublic_QJE_2019/scripts/programs"

* Stata version control
version 16

* Create directories for output files
cap mkdir "$WellnessPublic_QJE_2019/results"
cap mkdir "$WellnessPublic_QJE_2019/results/figures"
cap mkdir "$WellnessPublic_QJE_2019/results/tables"
cap mkdir "$WellnessPublic_QJE_2019/results/intermediate"


**********************
* Run analysis
**********************
* Figures
*   Figure 2: Employee Participation Rates in the iThrive Workplace Wellness Program
*   Figure 3: Pre-Intervention Medical Spending among Treatment Group, by Participation Status
*   Figures 5a-5c: Post-Intervention Medical Spending by Treatment Status
do "$WellnessPublic_QJE_2019/scripts/1_make_figures.do"

* Balance and selection tables
*   Tables 1a and 1b: Means of Study Variables at Baseline
*   Table 2: Selection on Medical Spending, Productivity, and Health Behaviors
*   Appendix Tables A.3a-3d: Selection on Strata, Health Care Utilization, Employment and Productivity, and Health and Behavior Variables
do "$WellnessPublic_QJE_2019/scripts/2_make_tables_1_2_A3.do"

* First-Year Effects
*   Table 4: First-Year Treatment Effects: Experimental versus Observational Estimates
*   Appendix Tables A.4a-g: First-Year Treatment Effects (ITT)
*   Appendix Tables A.5a-g: First-Year Treatment Effects: Experimental versus Observational Estimates
do "$WellnessPublic_QJE_2019/scripts/3_make_tables_4_A4_A5.do"

* Longer-Run Effects
*   Appendix Tables A.7a-g: Longer-Run Treatment Effects (ITT)
do "$WellnessPublic_QJE_2019/scripts/4_make_tables_A7.do"

* Summary of First-Year and Longer-Run ITT Effects
*   Table 3: Treatment Effects (Intention-to-Treat)
do "$WellnessPublic_QJE_2019/scripts/5_make_table_3.do"

* End log
di "End date and time: $S_DATE $S_TIME"
log close

** EOF
