# Public-Use Replication Files for Reif et al. (2020)

[`Overview`](#overview) [`Replication folder structure`](#replication-folder-structure ) [`Instructions`](#instructions) [`Software, memory, and runtime requirements`](#software-memory-and-runtime-requirements) [`Citation`](#citation)

-----------

## Overview 

This folder contains code that uses the public-use data to replicate results from the following paper:

Reif, Julian, David Chan, Damon Jones, Laura Payne, and David Molitor (2020). Effects of a workplace wellness program on employee health, health beliefs, and medical use: A randomized clinical trial. *JAMA Internal Medicine*, 180(7), 952-960.

The code does not replicate all results because some variables are suppressed in the public-use data. The datasets used in the original analysis are confidential and not publicly available. Instructions to obtain access to the restricted-use data are available on [our home page](../../README.md#restricted-use-data).


## Replication folder structure 

The following diagram summarizes the organization of the replication files.

```
wellness-data                         # Public-use project folder
├── data                              #   Public-use data
├── documentation                     #   Public-use data documentation
└── replications/jamaim_2020          #   Public-use replication files for Reif et al. (2020)
    ├── results                       #     Replication figures and tables
    ├── scripts                       #     Replication scripts
    |   ├── libraries/stata           #       Add-on Stata packages
    |   └── make_tables.do            #       Make tables
    └── run.do                        #     Master script that runs the whole analysis
```

## Instructions

First, [download this repository](https://github.com/reifjulian/illinois-wellness-data/archive/master.zip). Then, run the following Stata script: `replications/jamaim_2020/run.do`. This master script runs all of the analysis scripts required to replicate the main results. These analysis scripts are contained in the `replications/jamaim_2020/scripts` directory and are organized as follows:

- `scripts/libraries/stata` contains user-written (add-on) Stata commands used in the analysis. 
- `make_tables.do` creates the tables from the published paper. Results that are omitted due to censoring in the public-use data are reported as `N/A`.

Before running the master script, `run.do`, the user must first define the global macro `WellnessPublic` at the top of that script.

## Software, memory, and runtime requirements

Running this analysis requires Stata version 17 or higher. Add-on packages are included in `scripts/libraries/stata` and do not need to be installed by the user.

Memory requirements are minimal. Runtime is approximately 6 hours on a Windows 10 Desktop with Stata MP-2 and an AMD Ryzen 7 4700G processor when running the default specification of `boot = 1000` bootstraps. Runtime is approximately 8 minutes for `boot = 2`.

## Citation

Any materials (books, articles, conference papers, theses, dissertations, reports, and other such publications) created that employ, reference, or otherwise use these data (in whole or in part) should credit this source. Please cite it as:

Reif, J., Chan, D., Jones, D., Payne, L., & Molitor, D. (2020). Effects of a workplace wellness program on employee health, health beliefs, and medical use: A randomized clinical trial. *JAMA Internal Medicine*, 180(7), 952-960.
