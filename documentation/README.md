
# Dataset summaries

This public repository includes five datasets:

- **biometrics**: clinician-collected biometric data, self-reported health beliefs, and self-reported health behaviors

- **claims**: monthly medical spending derived from health insurance claims

- **firm_admin**: employee sick leave, gym visits, promotions, terminations, and productivity

- **marathon**: participation in the [Illinois Marathon](https://illinoismarathon.com/)

- **online_surveys**: responses to the 2016, 2017, and 2018 online surveys

- **participation**: participation in the different components of the two-year wellness intervention


# Getting started

Datasets are available in both [CSV](../data/csv) and [Stata](../data/stata) formats. Users are encourage to use the Stata datasets, which include variable labels.

Each dataset includes 4,834 observations. Most outcomes are measured at baseline, one-year post-intervention, and two-years post-intervention. The variable `treat` is equal to 1 if the individual was assigned to treatment and 0 otherwise. The datasets include a limited number of demographic variables and cannot be linked. Example code is available [here](../README.md#examples).

Summary statistics for each variable are provided by [codebooks](../data/codebooks). Additional information about the specific codings used to create these variables is available in [Appendix Table A.14](qje.table.a.14.pdf) of Jones, Molitor, and Reif (2019).

# Additional materials

Section 2 and Appendix D of [Jones, Molitor, and Reif 2019](https://www.nber.org/workplacewellness/s/IL_Wellness_Study_1.pdf) provide detailed information about the experimental design of the Illinois Workplace Wellness Study and about the data that were collected for the study. Study materials such as the online and on-site surveys are available on the [study website](https://www.nber.org/programs-projects/projects-and-centers/workplace-wellness/illinois-workplace-wellness-downloads).

