{smcl}
help {hi:wyoung}
{hline}
{title:Title}

{p 4 4 2}{cmd:wyoung} {hline 2} Control the family-wise error rate when performing multiple hypothesis tests.


{title:Syntax}

{p 4 8 2}Syntax 1: multiple hypothesis testing {hline 2} one model with multiple outcomes

{p 8 14 2}{cmd:wyoung} {help varlist:varlist}, {cmd:cmd(}{it:model}{cmd:)} {cmd:familyp(}{help exp:exp}{cmd:)} {cmdab:boot:straps(}{it:#}{cmd:)} 
[{cmd:seed(}{it:#}{cmd:)} 
{cmd:strata(}{help varlist:varlist}{cmd:)} {cmd:cluster(}{help varlist:varlist}{cmd:)} {cmd:force} {cmd:singlestep} {cmd:detail} {cmd:noresampling} {cmd:replace}]

{p 4 8 2}Syntax 2: multiple hypothesis testing {hline 2} different models with multiple outcomes and multiple subgroups

{p 8 14 2}{cmd:wyoung}, {cmd:cmd("}{it:model1}{cmd:"} [{cmd:"}{it:model2}{cmd:"} ...]{cmd:)} {cmd:familyp(}{help exp:exp}{cmd:)} {cmdab:boot:straps(}{it:#}{cmd:)} 
[{cmd:seed(}{it:#}{cmd:)} 
{cmd:strata(}{help varlist:varlist}{cmd:)} {cmd:cluster(}{help varlist:varlist}{cmd:)} {cmd:force} {cmd:singlestep} {cmd:detail} {cmd:noresampling} {cmd:replace}]

{p 4 8 2}where

{p 8 14 2}{help exp:exp} is any linear combination of coefficients that is valid syntax for {help lincom:lincom} or {help nlcom:nlcom}.  
{help exp:exp} must not contain an equal sign.

{title:Options}

{p 4 8 2}
{cmd:cmd(}{cmd:)}

{p 8 8 2} Syntax 1: one model with multiple outcomes

{p 12 12 2}
{cmd:cmd(}{it:model}{cmd:)} specifies a single model with the multiple outcomes {help varlist:varlist}. The outcome (dependent) variable is indicated in {it:model} by "OUTCOMEVAR" (upper case).
{cmd:wyoung} will estimate multiple outcome specifications by substituting each variable from {help varlist:varlist} into "OUTCOMEVAR".
See example 1 below.

{p 8 8 2} Syntax 2: different models with multiple outcomes and multiple subgroups

{p 12 12 2}
{cmd:cmd("}{it:model1}{cmd:"} [{cmd:"}{it:model2}{cmd:"} ...]{cmd:)} specifies a list of models. See example 2 below.

{p 4 8 2}
{cmd:familyp(}{help exp:exp}{cmd:)} specifies a coefficient or a combination of coefficients.
{help exp:exp} follows the syntax of {help lincom:lincom} and {help nlcom:nlcom}.
For example, specifying {cmd:familyp(}{it:varname}{cmd:)} is equivalent to specifying {cmd:familyp(}{it:_b[varname]}{cmd:)}.

{p 4 8 2}
{cmd:bootstraps(}{it:#}{cmd:)} performs # bootstrap replications for resampling. Westfall and Young (1993) recommend using at least 10,000 bootstraps.

{p 4 8 2}
{cmd:seed(}{it:#}{cmd:)} sets the random-number seed. Specifying this option is equivalent to typing the following command prior to calling {cmd:wyoung}:

{phang2}
{cmd:. set seed} {it:#}

{p 4 8 2}
{cmd:strata(}{help varlist:varlist}{cmd:)} specifies variables identifying strata.  If {cmd:strata()} is specified, bootstrap samples are selected within each stratum.

{p 4 8 2}
{cmd:cluster(}{help varlist:varlist}{cmd:)} specifies variables identifying clusters.  
If {cmd:cluster()} is specified, the sample drawn during each replication is a bootstrap sample of clusters.

{p 4 8 2}
{cmd:force} allows the user to include a model with clustered standard errors without specifying the {cmd:cluster()} bootstrap option.

{p 4 8 2}
{cmd:singlestep} computes the single-step adjusted {it:p}-value in addition to the step-down value. Resampling-based single-step methods often control type III (sign) error rates. Whether their
step-down counterparts also control the type III error rate is unknown (Westfall and Young 1993, p. 51).

{p 4 8 2}
{cmd:detail} produces sample size statistics for the bootstrap samples.

{p 4 8 2}
{cmd:noresampling} computes only the Bonferroni-Holm and Sidak-Holm adjusted {it:p}-values (very fast).

{p 4 8 2}
{cmd:replace} replaces data in memory with {cmd:wyoung} results.


{title:Description}

{p 4 4 2}{cmd:wyoung} calculates adjusted {it:p}-values using the free step-down resampling methodology of Westfall and Young (1993). It also computes the Bonferroni-Holm and Sidak-Holm adjusted {it:p}-values.

{p 4 4 2}The family-wise error rate (FWER) is the probability of rejecting any true null hypothesis belonging to a "family" of hypotheses. In other words, the FWER is the probability of making at least one false discovery within the family. 
When subset pivotality holds, the resampling-based method of Westfall and Young (1993) controls the FWER regardless of which hypotheses in the family happen to be true.

{p 4 4 2}The sampling distribution of a pivotal statistic does not depend upon which distribution generated the data. The {it:t}-statistic is an example of a pivotal statistic.
Subset pivotality extends this concept to a multivariate setting. The subset pivotality condition requires that the multivariate distribution of any subvector of {it:p}-values is unaffected
by the truth or falsehood of hypotheses corresponding to {it:p}-values not included in the subvector. This condition is satisfied in many cases, including testing the significance of coefficients
in a general multivariate regression model with possibly non-normal distributions.

{p 4 4 2}{cmd:wyoung} performs bootstrap resampling (i.e., sampling with replacement). It does not support permutation sampling (rerandomization).


{title:Methods and formulas}

{p 4 4 2}The free step-down resampling method employed by {cmd:wyoung} is based on Algorithm 2.8 of Westfall and Young (1993). 
The single-step resampling method (see option {cmd:singlestep}) is based on Algorithm 2.5 of Westfall and Young (1993). Detailed documentation 
is available online at {browse "https://www.nber.org/workplacewellness/s/wyoung.pdf":www.nber.org/workplacewellness/s/wyoung.pdf}.
The accompanying Stata code is available at {browse "https://www.nber.org/workplacewellness/s/wyoung_simulations.do":www.nber.org/workplacewellness/s/wyoung_simulations.do}.

{p 4 4 2}The Bonferroni-Holm and Sidak-Holm step-down {it:p}-values are calculated as follows. Sort the {it:J} unadjusted {it:p}-values so that {it:p(1)<p(2)<...<p(J)}. 
The Bonferroni-Holm adjusted {it:p}-values are calculated as {it:{p(1)*J, max[p(1),p(2)*(J-1)],..., max[p(J-1),p(J)]}}. 
The Sidak-Holm adjusted {it:p}-values are calculated as {it:{1-(1-p(1))^J, max[p(1),1-(1-p(2))^(J-1)],..., max[p(J-1),p(J)]}}.
If the calculation yields a value larger than 1, then the adjusted {it:p}-value is set equal to 1.

{p 4 4 2}Following estimation of a model, {cmd:wyoung} obtains unadjusted {it:p}-values from {cmd:r(table)}.
If {cmd:r(table)} is unavailable, then {cmd:wyoung} calculates a two-tailed p-value using the t-statistic and the residual degrees of freedom (as retrieved from {cmd:e(df_r)}). 
If the residual degrees of freedom are unavailable, the unadjusted {it:p}-value is calculated by assuming normality.


{title:Remarks}

{p 4 4 2}The {cmd:cmd(}{cmd:)} option supports compound double quotes when employing Syntax 2. In this case,
the user should specify the list of models as 
{cmd:cmd(`" `"}{it:model1}{cmd:"'} [{cmd:`"}{it:model2}{cmd:"'} ...]{cmd: "')}.


{title:Citation}

{p 4 4 2}{cmd:wyoung} is not an official Stata command. It is a free contribution to the research community. You may cite it as:

{p 8 8 2}Jones, D., D. Molitor, and J. Reif. 2019. "What Do Workplace Wellness Programs Do? Evidence from the Illinois Workplace Wellness Study." {it:The Quarterly Journal of Economics}, November 2019, 134(4): 1747-1791.


{title:Examples}

{p 4 4 2}1. Test whether the outcome variables {it:mpg}, {it:headroom}, or {it:turn} are significantly associated with {it:displacement}, conditional on {it:length}.

{col 8}{cmd:. {stata sysuse auto.dta, clear}}

{col 8}{cmd:. {stata wyoung mpg headroom turn, cmd(regress OUTCOMEVAR displacement length) familyp(displacement) bootstraps(100) seed(20)}}

{p 4 4 2}2. Same as example 1, but employs an alternative syntax.

{col 8}{cmd:. {stata sysuse auto.dta, clear}}

{col 8}{cmd:. {stata wyoung, cmd("regress mpg displacement length" "regress headroom displacement length" "regress turn displacement length") familyp(displacement) bootstraps(100) seed(20)}}

{p 4 4 2}3. Same as example 1, but employs clustered standard errors.

{col 8}{cmd:. {stata sysuse auto.dta, clear}}

{col 8}{cmd:. {stata wyoung mpg headroom turn, cmd(regress OUTCOMEVAR displacement length, cluster(rep78)) cluster(rep78) familyp(displacement) bootstraps(100) seed(20)}}

{p 4 4 2}4. Test the linear restriction {it:_b[length] + 50*_b[displacement] = 0}.

{col 8}{cmd:. {stata sysuse auto.dta, clear}}

{col 8}{cmd:. {stata wyoung mpg headroom turn, cmd(regress OUTCOMEVAR displacement length) familyp(length+50*displacement) bootstraps(100) seed(20)}}


{title:Stored results}

{p 4 4 2}Specifying the {cmd: replace} option will replace data in memory with {cmd:wyoung} results.
In addition, {cmd: wyoung} stores the following in {cmd: r()}:

{p 4 4 2}Macros

{p 8 8 2}{cmd:r(cmd)}     {space 5} {cmd:wyoung}

{p 8 8 2}{cmd:r(cmdline)} {space 1} command as typed

{p 4 4 2}Matrices

{p 8 8 2}{cmd:r(table)}   {space 3}   matrix of results


{title:References}

{p 4 4 2}Jones, D., D. Molitor, and J. Reif. "What Do Workplace Wellness Programs Do? Evidence from the Illinois Workplace Wellness Study." {it:Quarterly Journal of Economics}, November 2019, 134(4): 1747-1791. 
Available from: {browse "https://www.nber.org/workplacewellness/s/IL_Wellness_Study_1.pdf":www.nber.org/workplacewellness/s/IL_Wellness_Study_1.pdf}.

{p 4 4 2}Westfall, P., and S. Young. 1993. {it:Resampling-based Multiple Testing: Examples and Methods for p-value Adjustment.} John Wiley & Sons, Inc.


{title:Author}

{p 4 4 2}Julian Reif, University of Illinois

{p 4 4 2}jreif@illinois.edu


{title:Also see}

{p 4 4 2}
{help test:test}, 
{help anova:anova}


