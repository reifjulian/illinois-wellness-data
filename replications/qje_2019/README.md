# Public-Use Replication Files for Jones, Molitor, and Reif (2019)

[`Overview`](#overview) [`Replication folder structure`](#Replication-folder-structure ) [`Instructions`](#Instructions) [`Software, memory, and runtime requirements`](#Software-memory-and-runtime-requirements) [`Citation`](#citation)

-----------

## Overview 

This folder contains code that uses the public-use data to replicate results from the following paper:

Jones, Damon, David Molitor, and Julian Reif. 2019. “What do Workplace Wellness Programs do? Evidence from the Illinois Workplace Wellness Study.” *Quarterly Journal of Economics*, 134(4): 1747–1791.

The code does not replicate all results because some variables are suppressed in the public-use data. The datasets used in the original analysis are confidential and not publicly available. Instructions to obtain access to the restricted-use data are available at [`Restricted-use data`](https://github.com/reifjulian/wellness-data#Restricted-use-data).

Example code is available at [`Restricted-use data`](../../README.md#Restricted-use-data). [here](../README.md#examples).

## Replication folder structure 

The following diagram summarizes the organization of the replication files.

```
wellness-data                         # Public-use project folder
├── data                              #   Public-use data
├── documentation                     #   Public-use data documentation
└── replications/qje_2019             #   Public-use replication files for Jones, Molitor, and Reif (2019)
    ├── results                       #     Replication figures and tables
    ├── scripts                       #     Replication scripts
    |   ├── libraries/stata           #       Add-on Stata packages
    |   ├── programs                  #       Custom Stata ado files
    |   ├── 1_make_figures.do         #       Make figures
    |   ├── 2_make_tables_1_2_A3.do   #       Make tables 1, 2, A3
    |   ├── 3_make_tables_4_A4_A5.do  #       Make tables 4, A4, A5
    |   ├── 4_make_tables_A7.do       #       Make table A7
    |   └── 5_make_table_3.do         #       Make table 3
    └── run.do                        #     Master script that calls the replication scripts
```

## Instructions

First, [download this repository](https://github.com/reifjulian/illinois-wellness-data/archive/master.zip). Then, run the following Stata script: `replications/qje_2019/run.do`. This master script runs all the scripts required to replicate the main results. These other scripts are contained in the `replications/qje_2019/scripts` directory and are organized as follows:

- `scripts/libraries/stata` contains user-built Stata commands used in the analysis. 
- `scripts/programs` contains custom Stata ado files used in the analysis.
- `[1-5]_make_[figures/table(s)...].do` scripts create the indicated figures and tables from the published paper. Results that are omitted due to censoring in the public-use data are reported as `N/A`.

Before running `run.do`, the user must define the global macro `WellnessPublic` at the top of that script.

## Software, memory, and runtime requirements

Running this analysis requires Stata version 16 or higher. Add-on packages are included in `scripts/libraries/stata` and do not need to be installed by the user.

Memory requirements are minimal. Runtime is approximately 3 hours on a Windows 10 Desktop with Stata MP-6 and an i7-8700 CPU 3.20 GHz processor when running the default specification of `nboot = 10000` bootstraps. Runtime is approximately 1 minute for `nboot = 2`. 

## Citation

Any materials (books, articles, conference papers, theses, dissertations, reports, and other such publications) created that employ, reference, or otherwise use these data (in whole or in part) should credit this source. Please cite it as:

Jones, D., D. Molitor, and J. Reif. "What Do Workplace Wellness Programs Do? Evidence from the Illinois Workplace Wellness Study." *Quarterly Journal of Economics*, November 2019, 134(4): 1747-1791.
