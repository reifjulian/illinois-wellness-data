# Illinois Workplace Wellness Study: Public Use Data Repository

[`Overview`](#overview) [`Examples`](#examples) [`Restricted-use data`](#restricted-use-data) [`Terms of use`](#terms-of-use) [`References`](#references) [`Update history`](#update-history) 

-----------

## Overview 

This repository contains public use data for the [Illinois Workplace Wellness Study](https://www.nber.org/workplacewellness/), a randomized controlled trial run by Damon Jones, David Molitor, and Julian Reif. These data can be used for research, teaching, and replicating published results. For information about the study, publications, the research team, pre-analysis plans, original survey instruments, and more, please visit the study [website](https://www.nber.org/workplacewellness/).

The following diagram summarizes the organization of the repository.
```
illinois-wellness-data                # Public-use project folder
├── data                              #   Public-use data
|   ├── codebooks                     #     Codebooks
|   ├── csv                           #     Datasets (CSV format)
|   └── stata                         #     Datasets (Stata format)
├── documentation                     #   Public-use data documentation
└── replications                      #   Replication folders
    └── qje_2019                      #     Jones, Molitor, and Reif (2019)
```

The [documentation](/documentation/README.md) folder describes the datasets. The **replications** folder provides Stata code that uses the public use data to replicate a subset of results for the following publications:
  - [Jones, Molitor, and Reif (2019)](https://www.nber.org/workplacewellness/s/IL_Wellness_Study_1.pdf): [replication files](replications/qje_2019)
  - [Reif et al. (2020)](https://jamanetwork.com/journals/jamainternalmedicine/article-abstract/2765690?guestAccessKey=e5e8e875-c27f-44c4-a5b1-bea7ea27af57): replication files forthcoming

## Examples

1. Estimate the one-year causal effect of the Illinois workplace wellness program on medical spending (see Table 3 of [Jones, Molitor, and Reif 2019](https://www.nber.org/workplacewellness/s/IL_Wellness_Study_1.pdf))

```stata
* Stata code
use "https://reifjulian.github.io/illinois-wellness-data/data/stata/claims.dta", clear
reg spend_0816_0717 treat [aw=covg_0816_0717], robust
```

```R
# R code
library(haven)
library(estimatr)
my_data <- read_dta("https://reifjulian.github.io/illinois-wellness-data/data/stata/claims.dta")
lm_robust(spend_0816_0717 ~ treat, data = my_data, weights = covg_0816_0717, se_type = "HC1")
```

2. Create a frequency histogram of average monthly hospital spending for the August 2016 - July 2017 time period

```stata
* Stata code
use "https://reifjulian.github.io/illinois-wellness-data/data/stata/claims.dta", clear
histogram spendHosp_0816_0717, graphregion(fcolor(white)) freq
```

```R
# R code
library(haven)
my_data <- read_dta("https://reifjulian.github.io/illinois-wellness-data/data/stata/claims.dta")
hist(my_data$spendHosp_0816_0717)
```


## Restricted-use data

The restricted-use data include:
  - Anonymized identifiers that allow researchers to link individuals across different datasets
  - Uncensored variables such as salary
  - Raw responses to the [online and on-site surveys](https://www.nber.org/workplacewellness/downloads/)

These data are currently hosted on a non-networked computer located at:

	National Bureau of Economic Research
	1050 Massachusetts Ave.
	Cambridge, MA 
	02138

The non-networked computer includes Stata code that provides a full replication of the tables and figues from [Jones, Molitor, and Reif (2019)](https://www.nber.org/workplacewellness/s/IL_Wellness_Study_1.pdf) and [Reif et al. (2020)](https://jamanetwork.com/journals/jamainternalmedicine/article-abstract/2765690?guestAccessKey=e5e8e875-c27f-44c4-a5b1-bea7ea27af57).

Researchers interested in using the restricted-use data must:
  - Obtain consent from the study's principal investigators (Jones, Molitor, and Reif);
  - Obtain approval from the NBER Institutional Review Board; and
  - Sign a non-disclosure agreement with NBER

Inquiries regarding the restricted-use data can be directed to [David Molitor](mailto:dmolitor@illinois.edu).

## Terms of use

These study data contain information collected on 4,834 RESEARCH SUBJECTS. The PROMISE OF CONFIDENTIALITY promises to these individuals that the information they provided will not be disseminated without their permission; that the fact that they participated in the study will not be disclosed; and that disseminated information will include no linkages to their identities. Names and other identifying information regarding these individuals are presumed to be confidential.

Any intentional identification of a RESEARCH SUBJECT or unauthorized disclosure of his or her confidential information violates the PROMISE OF CONFIDENTIALITY given to the providers of the information. Therefore, by downloading these data, you hereby agree:

  - To not use these datasets for investigation of specific RESEARCH SUBJECTS
  - To make no use of the identity of any RESEARCH SUBJECT discovered inadvertently, and to advise us of any such discovery (jreif@illinois.edu)

Any materials (books, articles, conference papers, theses, dissertations, reports, and other such publications) created that employ, reference, or otherwise use these data (in whole or in part) should credit this source. Please cite it as:

Jones, D., D. Molitor, and J. Reif. "What Do Workplace Wellness Programs Do? Evidence from the Illinois Workplace Wellness Study." *Quarterly Journal of Economics*, November 2019, 134(4): 1747-1791.

## References

Jones, D., D. Molitor, and J. Reif. "What Do Workplace Wellness Programs Do? Evidence from the Illinois Workplace Wellness Study." *Quarterly Journal of Economics*, November 2019, 134(4): 1747-1791.

Reif, J., Chan, D., Jones, D., Payne, L., and Molitor, D. "Effects of a Workplace Wellness Program on Employee Health, Health Beliefs, and Medical Use: A Randomized Clinical Trial." *JAMA Internal Medicine*, May 2020, 180(7): 952-960.

## Update history

* **October 1, 2020**
  - Initial release
