# prepost-error-dynamics-lme

## Overview
Linear mixed-effects analyses of trial-by-trial behavior around errors. Uses large-scale datasets, residual-based RT metrics, and matched error vs no-error groups to quantify pre-error speeding and post-error slowing

## Directory Structure
```prepost-error-dynamics-lme/
├── 1_data_cleaning.R                    # Preprocess raw data, calculate RTs, filter users
├── 2_match_groups.R                     # Create matched error and no-error groups
├── 3_residuals_tests_plots.R            # LME residuals, per-trial tests, log-residual plots
├── 4_trial-specific_residuals_plots.R   # Residuals per trial, per-trial stats, plots
└── README.md                            # Project description and instructions```

## Script Descriptions
### `1. 1_data_cleaning.R`

### Purpose:
Initial preprocessing of the data. Calculates response times, performs log transformation, filters participants, and prepares trial-level variables for later analyses.
