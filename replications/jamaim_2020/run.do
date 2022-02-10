**********************
* OVERVIEW
*   This script generates tables and figures for the paper:
*       "The Illinois Workplace Wellness Study: Effects on Clinical outcomes" (Julian Reif, Dave Chan, Damon Jones, Laura Payne, and David Molitor)
*   The macro $Wellness_Biometrics should be defined to point to the main directory for the analysis
*	All do files are stored in /scripts
*   All raw data are stored in /data
*   All tables are outputted to /results/tables
*   All figures are outputted to /results/figures
* 
* SOFTWARE REQUIREMENTS
*   Analyses run on Windows using Stata version 17
*
* TO PERFORM A CLEAN RUN, DELETE THE FOLLOWING FOLDER:
*   /results
**********************

**********************
* Parameters defined by user
**********************

* "WellnessPublic" points to the root directory, which contains the subfolders "data" and "replications"
global WellnessPublic ""

* Number of bootstraps for Westfall-Young
* JAMA:im published paper used nboot = 1000. Runtime is approximately 6 hours on a Windows 10 Desktop with Stata MP-2 and an AMD Ryzen 7 4700G processor
* Use smaller value to run replication code faster. Runtime is approximately 8 minutes with nboot = 2.
global nboot = 1000
**********************
**********************

* Confirm that the global for the root directory has been defined
assert !missing("$WellnessPublic")

* Define location of the replication folder
global Wellness_Public_JAMA "$WellnessPublic/replications/jamaim_2020"

* Initialize log and record system parameters
clear all
set more off
cap mkdir "$Wellness_Public_JAMA/scripts/logs"
cap log close
local datetime : di %tcCCYY.NN.DD!-HH.MM.SS `=clock("$S_DATE $S_TIME", "DMYhms")'
local logfile "$Wellness_Public_JAMA/scripts/logs/`datetime'.log.txt"
log using "`logfile'", text

di "Begin date and time: $S_DATE $S_TIME"
di "Stata version: `c(stata_version)'"
di "Updated as of: `c(born_date)'"
di "Variant:       `=cond( c(MP),"MP",cond(c(SE),"SE",c(flavor)) )'"
di "Processors:    `c(processors)'"
di "OS:            `c(os)' `c(osdtl)'"
di "Machine type:  `c(machine_type)'"

* All required Stata packages are available in the /libraries folder
tokenize `"$S_ADO"', parse(";")
while `"`1'"' != "" {
  if `"`1'"'!="BASE" cap adopath - `"`1'"'
  macro shift
}
adopath ++ "$Wellness_Public_JAMA/scripts/libraries"
mata: mata mlib index

* Stata programs are stored in /programs
adopath ++ "$Wellness_Public_JAMA/scripts/programs"

* Stata version control
version 17

* Create directories for output files
cap mkdir "$Wellness_Public_JAMA/results"
cap mkdir "$Wellness_Public_JAMA/results/tables"

* Run project analysis
do "$Wellness_Public_JAMA/scripts/make_tables.do"

* End log
di "End date and time: $S_DATE $S_TIME"
log close

** EOF



