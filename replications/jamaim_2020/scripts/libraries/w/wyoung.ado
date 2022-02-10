*! wyoung 1.3.2 16jun2021 by Julian Reif
* 1.3.2: error handling code added for case where user specifies both detail and noresampling
* 1.3.1: new controls option functionality. Old functionality moved to controlsinteract
* 1.3: controls option added
* 1.2: familyp option now supports multiple variables. subgroup option added
* 1.1: familyp option now supports the testing of linear and nonlinear combinations of parameters
* 1.0.5: familyp option now supports factor variables and time-series operators
* 1.0.4: add support for commands that don't store p-values in r(table) (eg ivreg2)
* 1.0.3: better error handling for missing observations
* 1.0.2: cluster bootstrap now required when clustered standard errors are present; force option added
* 1.0.1: cluster bootstrap option added

***
* Notation
***

* K = number of hypotheses = num subgroups X num familyp X num outcomes X num controls
* N = number of bootstraps

program define wyoung, rclass

	version 12

	* Syntax 1: one model with multiple outcomes (and possibly multiple controls and subgroups)
	syntax [varlist(default=none)], cmd(string) BOOTstraps(int) familyp(string) [weights(varlist) noRESAMPling seed(string) strata(varlist) cluster(varlist) subgroup(varname numeric) controls(string asis) controlsinteract(string asis) force detail SINGLEstep familypexp replace]
	
	local outcome_vars "`varlist'"
	
	* Syntax 2: different models
	if "`outcome_vars'"=="" {
	
		* Perform a full trim to remove leading and trailing spaces from the cmd() option
		mata: st_local("cmd",strtrim(st_local("cmd")))
	
		* If user did NOT use compound double quotes in cmd(), pass through the string asis. This ensures the -tokenize- command below works properly.
		mata: if( substr(st_local("cmd"),1,1)!=char(96) ) stata("syntax, cmd(string asis) *");;
	}
		
	local user_cmd "wyoung `0'"
	
	tempfile bs
	tempname mat nlcom_b nlcom_V
	
	******
	* Error check syntax options
	******	

	* N = number of bootstraps
	local N = `bootstraps'
	capture assert `N' > 0
	if _rc {
		di as err "bootstrap size must be greater than zero"
		exit 198
	}	
	
	* Seed option
	if !mi("`seed'") {
		cap set seed `seed'
		if _rc {
			di as error "invalid syntax for option seed()"
			set seed `seed'
		}
	}
	
	* Strata option
	local bs_strata ""
	if !mi("`strata'") {
		local bs_strata "strata(`strata')"
	}
	
	* Cluster option
	if !mi("`cluster'") {
		tempname id_cluster
		local bs_cluster "cluster(`cluster') idcluster(`id_cluster')"
	}
	
	* Detail options
	if "`detail'"!="" & "`resampling'"=="noresampling" {
		di as error "cannot specify both the detail and noresampling options"
		exit 198
	}
	
	* Subgroup option
	local num_subgroups = 1
	qui if "`subgroup'"!="" {
		
		if mi("`outcome_vars'") {
			di as error "subgroup() option not allowed when employing Syntax 2 of wyoung"
			exit 198
		}
		
		* Ensure subgroups are all integers
		local subgroup_err_msg = 0
		cap confirm float var `subgroup'
		if _rc==0 local subgroup_err_msg = 1
		cap confirm double var `subgroup'
		if _rc==0 local subgroup_err_msg = 1
		if `subgroup_err_msg'==1 {
			di as error "float or double variables not allowed in option subgroup()"
			exit 109
		}

		* Subgroup option triggers stratified resampling, unless user overrides
		if mi("`strata'") local bs_strata "strata(`subgroup')"
		
		* Subgroup option not allowed with a command that contains an "if" 
		if strpos(`"`cmd'"', " if ") {
			di as error "if expressions not allowed when specifying subgroup()"
			exit 198
		}
		
		levelsof `subgroup'
		local subgroup_vals `"`r(levels)'"'
		local num_subgroups : word count `subgroup_vals'
	}
	
	* CONTROLVARS needs to be specified with either controls() or controlsinteract()
	if strpos(`"`cmd'"'," CONTROLVARS") & (`"`controlsinteract'"'=="" & `"`controls'"'=="") {
		di as error "cannot have {it:CONTROLVARS} without specifying option {cmd:controls()} or {cmd:controlsinteract()}"
		exit 198		
	}	
	
	* Controlsinteract option (multiplies number of hypotheses being tested)
	local num_sets_controls = 1
	qui if `"`controlsinteract'"'!="" {
		
		if mi("`outcome_vars'") {
			di as error "controlsinteract() option not allowed when employing Syntax 2 of wyoung"
			exit 198
		}
		
		if !strpos("`cmd'"," CONTROLVARS") {
			di as error "did not specify {it:CONTROLVARS} in option {cmd:cmd()}"
			exit 198
		}		
		
		* Split out the different control sets
		tokenize `"`controlsinteract'"'

		local k = 0
		while `"`1'"' != "" {
			local k = `k'+1

			* Perform a full trim to remove leading and trailing spaces
			mata: st_local("1",strtrim(st_local("1")))
			
			confirm variable `1'

			local controlvars_`k' `"`1'"'
			macro shift
		}
		
		local num_sets_controls = `k'		
	}

	* Controls option (simple 1:1 substitution, no increase in number of hypotheses)	
	qui if `"`controls'"'!="" {
		
		if mi("`outcome_vars'") {
			di as error "controls() option not allowed when employing Syntax 2 of wyoung"
			exit 198
		}
		
		if !strpos("`cmd'"," CONTROLVARS") {
			di as error "did not specify {it:CONTROLVARS} in option {cmd:cmd()}"
			exit 198
		}		
		
		* Split out the different control sets
		tokenize `"`controls'"'

		local k = 0
		while `"`1'"' != "" {
			local k = `k'+1

			* Perform a full trim to remove leading and trailing spaces
			mata: st_local("1",strtrim(st_local("1")))
			
			confirm variable `1'

			local controlvars_`k' `"`1'"'
			macro shift
		}
		local tmp : word count `outcome_vars'
		if `k'!=`tmp' {
			di as error "Number of varlists in controls() option does not equal the number of outcomes"
			exit 198
		}
	}		
	
	******
	* Syntax 1: user specifies varlist that will replace "OUTCOMEVAR"
	******
	if "`outcome_vars'"!="" {

		local num_outcomes : word count `outcome_vars'
		
		* If user specified familypexp (rare), then the input is a single lincom/nlcom expression, not a varlist
		local num_familypvars = 1
		if "`familypexp'"=="" {
			fvunab familyp : `familyp'
			local num_familypvars : word count `familyp'
		}

		* Ensure that "OUTCOMEVAR" is present in the command
		if !strpos("`cmd'"," OUTCOMEVAR ") {
			di as error "did not specify {it:OUTCOMEVAR} in option {cmd:cmd()}"
			exit 198
		}
		
		* If weights are specified, ensure there is exactly one weight variable for each outcome
		* Note: this option is undocumented
		if "`weights'"!="" {
			
			local num_weightvars : word count `weights'
			if "`num_weightvars'"!="`num_outcomes'" {
				di as error "number of weight vars = `num_weightvars' != `num_outcomes' = number of outcomes"
				exit 198
			}
			
			forval w = 1/`num_weightvars' {
				tokenize `weights'
				local weightvar_`w' ``w''
			}
		}

		* K = number of hypotheses = subgroups X outcomes X family pvars X control sets
		local K = `num_subgroups' * `num_familypvars' * `num_outcomes' * `num_sets_controls'
		
		* For each hypothesis k=1...K, define the outcome, familyp, and regression command
		local k = 1
		forval s = 1/`num_subgroups' {
			
			if "`subgroup'"!="" {
				tokenize `subgroup_vals'
				local subgroup_touse "``s''"
			}
					
			forval f = 1/`num_familypvars' {
				
				if `num_familypvars'==1 local familyp_touse "`familyp'"
				else {
					tokenize `familyp'
					local familyp_touse "``f''"
				}
				
				forval c = 1/`num_sets_controls' {
					
					* Default is blank; otherwise controlsinteract() option operates here; overwritten below if controls() was specified instead
					local controls_touse "`controlvars_`c''"
					
					forval i = 1/`num_outcomes' {
						
						if `"`controls'"'!="" local controls_touse "`controlvars_`i''"

						tokenize `outcome_vars'
						local outcomevar_`k' "``i''"
						local familyp_`k' "`familyp_touse'"
						local subgroup_`k' "`subgroup_touse'"
						local controls_`k' "`controls_touse'"
						
						* Baseline regression (note: WEIGHTVAR substitution here is an undocumented feature; WEIGHTVAR is numbered from i=1...num_outcomes)
						local cmdline_`k': subinstr local cmd         "OUTCOMEVAR" "`outcomevar_`k''", word
						local cmdline_`k': subinstr local cmdline_`k' "WEIGHTVAR"  "`weightvar_`i''"
						local cmdline_`k': subinstr local cmdline_`k' "CONTROLVARS"  "`controls_`k''"
						
						* Subgroup option: insert an if clause in front of the comma (if present)
						if "`subgroup'"!="" {
							local tmp `"`cmdline_`k''"'
							local index_comma = strpos(`"`tmp'"', ",")
							if `index_comma'==0 {
								local cmd_part1 `"`tmp'"'
							}
							else {
								mata: st_local("cmd_part1", substr(st_local("tmp"), 1, strpos(st_local("tmp"), ",")-1))
								mata: st_local("cmd_part2", substr(st_local("tmp"), strpos(st_local("tmp"), ","), .))
							}

							local cmdline_`k' `"`cmd_part1' if `subgroup'==`subgroup_touse'`cmd_part2'"'
						}

						local k = `k'+1
					}
				}
			}
		}
	}
	
	******
	* Syntax 2: user specifies each individual model
	******
	else {

		* Split out the different models
		tokenize `"`cmd'"'

		local k = 0
		while `"`1'"' != "" {
			local k = `k'+1

			* Perform a full trim to remove leading and trailing spaces
			mata: st_local("1",strtrim(st_local("1")))

			* Strip leading and trailing quotes, if present (occurs when user specifies compound double quotes)
			mata: if( substr(st_local("1"),1,1)==char(34) & substr(st_local("1"),-1,1)==char(34) ) st_local(  "1", substr(st_local("1"), 2, strlen(st_local("1"))-2)  );;

			local cmdline_`k' `"`1'"'
			macro shift
		}
		
		* K = number of hypotheses = number of models
		local K = `k'
		
		* Split out the familyp variable/exp that corresponds to each model
		tokenize `"`familyp'"'

		local k = 0
		while `"`1'"' != "" {
			local k = `k'+1

			* Perform a full trim to remove leading and trailing spaces
			mata: st_local("1",strtrim(st_local("1")))

			* Strip leading and trailing quotes, if present (occurs when user specifies compound double quotes)
			mata: if( substr(st_local("1"),1,1)==char(34) & substr(st_local("1"),-1,1)==char(34) ) st_local(  "1", substr(st_local("1"), 2, strlen(st_local("1"))-2)  );;

			local familyp_`k' `"`1'"'
			if "`familypexp'"=="" fvunab familyp_`k' : `familyp_`k''
			
			macro shift
		}		
		local num_familypvars = `k'
		
		* If user specifiy only a single family p command, assume it applies to all models
		if `num_familypvars'==1 {
			forval k = 2/`K' {
				local familyp_`k' `familyp_1'
			}				
		}
		else if `num_familypvars'!= `K' {
			di as error "Number of familyp commands, `num_familypvars', does not match number of models, `K'"
			exit 198
		}
		
		* num_outcomes = 0 indicates syntax 2 was called
		local num_outcomes = 0
	}
	
	******
	* Step 1: Estimate the initial, unadjusted models
	******
	di as text "Estimating family-wise adjusted {it:p}-values for " as result `K' as text " hypothesis tests"

	qui forval k = 1/`K' {

		if "`subgroup'"!="" & mod(`k',`num_outcomes')==1                         noi di as text _n "subgroup: " as result `"`subgroup_`k''"'
		else if (`num_outcomes'==0 | mod(`k',`num_outcomes')==1)                 noi di as text ""
		if `num_outcomes'==0 | mod(`k',`num_outcomes')==1                        noi di as text "familyp: " as result `"`familyp_`k''"'
		if `"`controlsinteract'"'!="" & (`num_outcomes'==0 | mod(`k',`num_outcomes')==1) noi di as text "controls: " as result `"`controls_`k''"'
		
		noi di in yellow _skip(4) `"`cmdline_`k''"'
	
		tempname p_`k' ystar_`k'

		* Run regression k
		`cmdline_`k''
		
		local N_`k' = e(N)
		if "`e(vce)'"=="cluster" local vce_cluster 1
		if !mi("`e(depvar)'") local outcomevar_`k' "`e(depvar)'"
		
		* Calculate _b, _se, and the associated unadjusted p-val using lincom (or nlcom if _rc==131, which indicates nonlinearity)
		cap lincom `familyp_`k''
		if !_rc {
			local beta_`k' = r(estimate)
			local stderr_`k' = r(se)
			scalar `p_`k'' = r(p)
		}
		else if _rc==131 {
			cap nlcom `familyp_`k''
			if !_rc {
				
				matrix `nlcom_b' = r(b)
				matrix `nlcom_V' = r(V)
				
				local beta_`k' = `nlcom_b'[1,1]
				local stderr_`k' = sqrt(`nlcom_V'[1,1])
				scalar `p_`k'' = 2*normal(-abs(`beta_`k''/`stderr_`k''))					
				matrix drop `nlcom_b' `nlcom_V'
			}
			else {
				noi di as error _n `"`familyp_`k'' is invalid syntax for {cmd:lincom} and {cmd:nlcom}"'
				error _rc
			}
		}
		else {
			noi di as error "The following error occurred when running the command " as result `"lincom `familyp_`k'':"'
			lincom `familyp_`k''
		}
		
		* Ensure that beta and pval are recovered
		if `beta_`k''==. {
			noi di as error "coefficient estimate for " as result "`familyp_`k''" as error " not available after running the command " as result `"`cmdline_`k''"'
			exit 504			
		}		
		
		if `p_`k''==. {

			local tstat = abs(`beta_`k'' / `stderr_`k'')
			
			cap local df = `e(df_r)'
			cap confirm numeric `df'
			if !_rc scalar `p_`k'' = tprob(`df', abs(`tstat'))
			else    scalar `p_`k'' = 2*(1-normprob(abs(`tstat')))

			if `p_`k''==. {
				noi di as error "p-value not available and could not be calculated when running the command " as result `"`cmdline_`k''"'
				exit 504
			}
		}		
		
	}

	* Issue error if user is estimating a model with clustered standard errors AND did not specify a bootstrap cluster (unless force option specified)
	if "`vce_cluster'"=="1" & mi("`cluster'") & mi("`force'") {
			di as error "estimating model with clustered standard errors, but {bf:cluster()} option was not specified"
			exit 198
	}

	preserve
	
	******
	* Use bootstrapping to resample the data, and calculated Westfall-Young adjusted p-vals
	******	
	if "`resampling'"!="noresampling" {

		***
		* Step 2(a). Loop over each bootstrap i and calculate pstar's
		***
		qui forval i = 1/`N' {

			* Draw a (possibly stratified, possibly clustered) random sample with replacement
			bsample, `bs_strata' `bs_cluster'
			if "`bs_cluster'"!="" {
				drop `cluster'
				ren `id_cluster' `cluster'
			}

			* Calculate pstar for each model, using test or testnl
			qui forval k = 1/`K' {

				cap `cmdline_`k''
				if _rc {
					noi di as error _n "The following error occurred when running the command " as result `"`cmdline_`k''"' as error " on a bootstrap sample:"
					error _rc
				}
				local Ni_`k' = e(N)
				
				cap test `familyp_`k'' == `beta_`k''
				if _rc==131 {
					cap testnl `familyp_`k'' == `beta_`k''
					if _rc {
						noi di as error _n `"`familyp_`k'' is invalid syntax for {cmd:test} and {cmd:testnl}"'
						error _rc
					}
				}
				else if _rc {
					noi di as error _n "The following error occurred when running the command " as result `"test `familyp_`k'' == `beta_`k''"' as error " on a bootstrap sample:"					
					test `familyp_`k'' == `beta_`k''
				}
				local pstar_`k' = r(p)
			}
			
			* Store results from each model k
			drop _all
			set obs `K'
			gen i = `i'
			gen k = _n
			gen pstar = .
			gen p = .
			if !mi("`detail'") gen N = .
			qui forval k = 1/`K' {
				replace pstar = `pstar_`k'' in `k'
				replace p  = `p_`k'' in `k'
				if !mi("`detail'") replace N = `Ni_`k'' in `k'
			}
			
			if `i'>1 append using "`bs'"
			save "`bs'", replace
			
			restore, preserve
		}

		***
		* Calculate Westfall-Young adjusted p-vals
		***
		use "`bs'", clear

		* Step 2(b). Enforce monotonicity with successive minima to produce step-down p-values. Include k in the sort to break ties.
		gsort i -p k	
		qui by i: gen qstar = pstar if _n==1
		qui by i: replace qstar = min(qstar[_n-1],pstar) if _n>1
		
		* Define minimum for single-step p-value
		qui by i: egen qstar1 = min(pstar)
		
		if !mi("`detail'") local Nstats "(mean) Navg=N (min) Nmin=N (max) Nmax=N"

		* Steps 3 and 4. Calculate step-down and single-step Westfall-Young adjusted p-value
		qui gen counter  = qstar <=p
		qui gen counter1 = qstar1<=p
		collapse (sum) counter* `Nstats', by(k) fast
		qui gen pwyoung  = counter /`N'
		qui gen pwyoung1 = counter1/`N'
		drop counter*
		
		assert pwyoung<=pwyoung1
	}
	qui else {
		drop *
		set obs `K'
		gen k = _n
	}
	
	******
	* Enforce monotonicity, format results, and calculate some additional adjustments
	******	
	
	* Fill in descriptive information
	qui gen model=""
	qui gen outcome=""
	qui gen double coef = .
	qui gen double stderr = .
	qui gen double p = .
	qui gen familyp = ""
	if !mi("`subgroup'") qui gen subgroup = .
	if !mi(`"`controlsinteract'"') qui gen controlspec = ""
	if !mi("`detail'") qui gen N = .

	qui forval k = 1/`K' {
		replace model = `"`cmdline_`k''"'      if k==`k'
		replace outcome = "`outcomevar_`k''"   if k==`k'
		replace coef = `beta_`k''              if k==`k'
		replace stderr = `stderr_`k''          if k==`k'
		replace p  = `p_`k''                   if k==`k'
		replace familyp = "`familyp_`k''"                                          if k==`k'
		if !mi("`subgroup'")           replace subgroup = `subgroup_`k''           if k==`k'
		if !mi(`"`controlsinteract'"') replace controlspec = "`controls_`k''" if k==`k'
		if !mi("`detail'")             replace N        = `N_`k''                  if k==`k'
	}	
	
	* Step 5. Enforce monotonicity using successive maximization.  Include k in the sort to break ties.
	sort p k
	qui cap replace pwyoung  = max(pwyoung[_n-1]  ,pwyoung)   if _n>1
	qui cap replace pwyoung1 = max(pwyoung1[_n-1] ,pwyoung1)  if _n>1

	* Calculate Holm-Bonferroni and Holm-Sidak step-down corrections
	tempname j
	qui gen `j' = _N-_n+1
	
	qui gen double pbonf = min(p*`j',1) if _n==1
	qui replace    pbonf = min(max(p*`j',pbonf[_n-1]),1) if _n>1

	qui gen double psidak = min((1-(1-p)^(`j')),1) if _n==1
	qui replace    psidak = min(max((1-(1-p)^(`j')),psidak[_n-1]),1) if _n>1
	
	if !mi("`detail'") local Ns "N Navg Nmin Nmax"
	if !mi("`subgroup'") local subgroup subgroup
	if !mi(`"`controlsinteract'"') local controlspec controlspec
	
	label var pbonf "Bonferroni-Holm p-value"
	label var psidak "Sidak-Holm p-value"
	label var stderr "Unadjusted standard error"
	label var p  "Unadjusted p-value"
	cap label var pwyoung   "Westfall-Young adjusted p-value"
	cap label var pwyoung1 "Westfall-Young adjusted p-value (single-step)"
	cap label var N "Number of obs"
	cap label var Navg "Average number of obs (bootstraps)"
	cap label var Nmin "Min number of obs (bootstraps)"
	cap label var Nmax "Max number of obs (bootstraps)"
	
	assert psidak<=pbonf+0.00000000001
	foreach v of varlist p* {
		assert `v'<=1
	}
	
	* Single-step values
	if !mi("`singlestep'")  {
		gen double pbonf1 = min(`K'*p,1)
		gen double psidak1 = 1-((1-p)^`K')
		label var pbonf1 "Bonferroni p-value (single-step)"
		label var psidak1 "Sidak p-value (single-step)"
	}
	else cap drop pwyoung1
	
	sort k
	drop `j'
	order k model outcome `controlspec' familyp `subgroup' coef stderr p `Ns'
	list
	
	mkmat coef stderr p*, matrix(`mat')
	
	if "`replace'"=="replace" restore, not
	
	******
	* Return values
	******
	return matrix table `mat'
	
	return local cmdline `user_cmd'
	return local cmd wyoung
end

** EOF

