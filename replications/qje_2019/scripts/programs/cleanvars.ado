
* Code for cleaning variable names for tables
program define cleanvars, nclass

  local v = "`0'"

  replace `v' = "Male [admin]"                                  if `v'=="male"
  replace `v' = "Age 50+ [admin]"                               if `v'=="age50"
  * 2019-07-31: add en-dash
  replace `v' = "Age 37--49 [admin]"                             if `v'=="age37_49"
  replace `v' = "White [admin]"                                 if `v'=="white"
  replace `v' = "Salary Q3 [admin]"                             if `v'=="salaryQ3"
  replace `v' = "Salary Q2 [admin]"                             if `v'=="salaryQ2"
  replace `v' = "Salary Q1 (bottom quartile) [admin]"           if `v'=="salaryQ1"
  replace `v' = "Faculty [admin]"                               if `v'=="faculty"
  * 2019-07-31: sentence case
  replace `v' = "Academic staff [admin]"                        if `v'=="AP"

  replace `v' = "Ever screened [survey]"                        if strpos(`v',"everscreen")
  replace `v' = "Physically active [survey]"                    if inlist(`v',"active_0716","active_0717","active_0718")
  replace `v' = "Trying to be active [survey]"                  if strpos(`v',"active_try")
  replace `v' = "Current smoker (cigarettes) [survey]"          if strpos(`v',"cursmk")
  replace `v' = "Former smoker [survey]"                        if strpos(`v',"formsmk")
  replace `v' = "Current smoker (other) [survey]"               if strpos(`v',"othersmk")
  replace `v' = "Heavy drinker [survey]"                        if strpos(`v',"drinkhvy")
  replace `v' = "Drinker [survey]"                              if inlist(`v',"drink_0716","drink_0717","drink_0718")
  replace `v' = "Chronic condition [survey]"                    if strpos(`v',"chronic")
  replace `v' = "Excellent or v. good health [survey]"          if strpos(`v',"health1")
  replace `v' = "Not poor health [survey]"                      if strpos(`v',"health2")
  replace `v' = "Physical problems [survey]"                    if strpos(`v',"problems")
  replace `v' = "Lots of energy [survey]"                       if strpos(`v',"energy")
  replace `v' = "Bad emotional health [survey]"                 if strpos(`v',"ehealth")
  replace `v' = "Overweight [survey]"                           if strpos(`v',"overweight")
  replace `v' = "High BP/cholesterol/glucose [survey]"          if strpos(`v',"badhealth")
  replace `v' = "Sedentary [survey]"                            if strpos(`v',"sedentary")
  replace `v' = "Pharmaceutical drug utilization [survey]"      if strpos(`v',"druguse")
  replace `v' = "Physician/ER utilization [survey]"             if strpos(`v',"physician")
  replace `v' = "Hospital utilization [survey]"                 if strpos(`v',"hospital")

  * 2019-07-31: remove hyphen in "nonzero"
  replace `v' = "Nonzero medical spending [admin]"             if strpos(`v',"nonzero_spend")
  replace `v' = "Total spending (dollars/month) [admin]"        if strpos(`v',"spend_")==1
  replace `v' = "Drug spending [admin]"                         if strpos(`v',"spendRx_")==1
  replace `v' = "Office spending [admin]"                       if strpos(`v',"spendOff_")==1
  replace `v' = "Hospital spending [admin]"                     if strpos(`v',"spendHosp_")==1
  replace `v' = "Other spending [admin]"                        if strpos(`v',"spendOthr_")==1

  replace `v' = "Time to first claim <= 1 month [admin]"            if strpos(`v',"anyspend_0816_0816")
  replace `v' = "Time to first claim <= 2 months [admin]"           if strpos(`v',"anyspend_0816_0916")
  replace `v' = "Time to first claim <= 3 months [admin]"           if strpos(`v',"anyspend_0816_1016")
  replace `v' = "Time to first claim <= 6 months [admin]"           if strpos(`v',"anyspend_0816_0117")
  replace `v' = "Time to first claim <= 12 months [admin]"          if strpos(`v',"anyspend_0816_0717")
  replace `v' = "Time to first claim <= 24 months [admin]"          if strpos(`v',"anyspend_0816_0718")
  replace `v' = "Time to first claim <= 29 months [admin]"          if strpos(`v',"anyspend_0816_1218")
  replace `v' = "Pharmaceutical events (days/month) [admin]"        if strpos(`v',"visitRx_0816_")
  replace `v' = "Physician office visits (days/month) [admin]"      if strpos(`v',"visitOff_0816_")
  replace `v' = "Hospital stays (days/month) [admin]"               if strpos(`v',"visitHosp_0816_")
  replace `v' = "Other medical events (days/month) [admin]"         if strpos(`v',"visitOthr_0816_")

  * 2019-07-31: add en-dash
  replace `v' = "IL Marathon/10K/5K (2014--2016) [admin]"            if strpos(`v',"marathon_2014_2016")
  replace `v' = "IL Marathon/10K/5K 2017 [admin]"                   if strpos(`v',"marathon_2017")
  replace `v' = "IL Marathon/10K/5K 2018 [admin]"                   if strpos(`v',"marathon_2018")
  replace `v' = "Campus gym visits (days/year) [admin]"             if strpos(`v',"gym_")==1

  replace `v' = "Sick leave (days/year) [admin]"                    if strpos(`v',"sickleave_")==1
  replace `v' = "Annual salary (dollars) [admin]"                   if strpos(`v',"salary_")
  replace `v' = "Annual salary (share of baseline salary) [admin]"  if strpos(`v',"salaryRaise_0616_")==1
  replace `v' = "Job title change [admin]"                          if strpos(`v',"titleChange_0616_")==1
  replace `v' = "Job promotion [admin]"                             if strpos(`v',"promotion_0616_")==1
  replace `v' = "Job terminated [admin]"                            if strpos(`v',"terminated_")==1
  replace `v' = "Any sick days in past year [survey]"               if strpos(`v',"sickdays_")==1
  replace `v' = "Any sick days in past year [survey]"               if `v'=="sickdays"

  replace `v' = "Worked 50+ hours/week [survey]"                    if strpos(`v',"hrsworked50")
  replace `v' = "Very satisfied with job [survey]"                  if strpos(`v',"jobsatisf1")
  replace `v' = "Very or somewhat satisfied with job [survey]"      if strpos(`v',"jobsatisf2")
  replace `v' = "Management priority on health/safety [survey]"     if strpos(`v',"mgmtsafety")
  
  replace `v' = "Productivity index [survey/admin]"     	    if strpos(`v',"prod_index_yr")

  * Variables only on 2017 survey
  replace `v' = "Happier at work than last year [survey]"           if strpos(`v',"happywork_")==1
  replace `v' = "Presenteeism [survey]"                             if strpos(`v',"presenteeism_")==1
  replace `v' = "Feel very productive at work [survey]"             if strpos(`v',"productive_")==1
  replace `v' = "Received promotion [survey]"                       if strpos(`v',"promotion_")==1
  replace `v' = "Job search very likely [survey]"                   if strpos(`v',"jobsearch1_")==1
  replace `v' = "Job search somewhat/very likely [survey]"          if strpos(`v',"jobsearch2_")==1

end

** EOF
