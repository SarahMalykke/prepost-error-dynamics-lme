# prepost-error-dynamics-lme

## Overview
Linear mixed-effects analyses of trial-by-trial behavior around errors. Uses large-scale datasets, residual-based RT metrics, and matched error vs no-error groups to quantify pre-error speeding and post-error slowing

## Directory Structure
```prepost-error-dynamics-lme/
├── 1_data_cleaning.R                    # Preprocess raw data, calculate RTs, filter users
├── 2_match_groups.R                     # Create matched error and no-error groups
├── 3_residuals_tests_plots.R            # LME residuals, per-trial tests, log-residual plots
├── 4_trial-specific_residuals_plots.R   # Residuals per trial, per-trial stats, plots
└── README.md                            # Project description and instructions
```

### 1. `1_data_cleaning.R`

**Purpose:**  
- Performs initial preprocessing of the dataset.  

**Input:**  
- `Flash_95%_Sarah.csv`: Raw 95% Flash Challenge dataset (Airport Scanner mobile game).  
