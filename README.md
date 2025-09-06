# Pre- and Post-Error Dynamics: LME Analysis Pipeline

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
Initial preprocessing of raw Flash Challenge data. Calculates response times, performs log transformation, filters participants, and prepares trial-level variables for later analyses.  

**Input:**  
- `Flash_95%_Sarah.csv`: Raw 95% Flash Challenge dataset (Airport Scanner mobile game).  

**Cleaning Steps:**  
- **Compute Response Times:** Create *Touch Time*, *Swipe Time*, and *Total RT* from raw timestamps (`WaitTime`, `DragTime`, `ReleaseTime`).  
- **Log Transform RTs:** Add log-transformed versions for each RT measure (`Touch_Time_log`, `Swipe_Time_log`, `Total_RT_log`).  
- **Filter Trials:** Keep only Tier 1, Day 1, first challenge per user. Remove outlier trials (`Total_RT < 250 ms` or `Total_RT > 5000 ms`) and users with fewer than 8 correct trials.  
- **Assign Trial Numbers:** Add sequential trial numbers within each user.  
- **Add N−1 Accuracy:** Compute accuracy of the previous trial to capture sequential effects.  
- **Label Error Status:** Mark each trial as *Pre-Error*, *Error*, *Post-Error*, or *Other*.  
- **Compute Total Trials:** Record the total number of trials completed per user.  
- **Standardize RTs:** Create z-scored RTs within user for *Touch* and *Swipe* times.  

**Output:**  
- `FlashData_95pct.csv`: Preprocessed dataset.  
- `only1error_95pct.csv`: Subset with users who made exactly one error.  
- `noerror_95pct.csv`: Subset with users who made no errors.

---

### 2. `2_match_groups.R`

**Purpose:**  
Create matched **error** and **no-error** cohorts with identical trial-count distributions, add trial-relative indices, and save clean subsets for downstream modeling.

**Input:**  
- `FlashData_95pct.csv` — preprocessed dataset from `1_data_cleaning.R`.

**Steps:**  
- **Identify Groups:**  
  - *Error group*: users with exactly one error.  
  - *No-error group*: users with zero errors.  
- **Compute Total Trials:** Count trials per user in each group.  
- **Match on Trial Length:** Intersect available `TotalTrials` values and filter both cohorts to shared values only.  
- **Sample Users per Length:** For each `TotalTrials` value, randomly sample equal numbers of users from both groups (reproducible seed).  
- **Carry Over Error Trial Index:** From the error group, copy each user’s `ErrorTrial` to the matched no-error user with the same `TotalTrials`.  
- **Compute Trial Index:** Add `TrialsSinceError = TrialNumber - ErrorTrial` for both groups (the no-error cohort uses the borrowed `ErrorTrial` index).  
- **Save Matched Subsets:** User lists and matched trial-level data for both groups.

**Output:**  
- `subset_error_userIds.csv`, `subset_noerror_userIds.csv` — matched user lists by `TotalTrials`.  
- `only1error_95pct_subset_users.csv` — matched error cohort with `TrialsSinceError`.  
- `noerror_95pct_subset_users.csv` — matched no-error cohort with `TrialsSinceError`.

---

### 3. `3_residuals_tests_plots.R`

**Purpose:**  
Fit linear mixed-effects (LME) models on log RTs, compute residuals for **error** vs **no-error** groups, run per-trial group comparisons across ±15 trials, and visualize log-residual trajectories.

**Input:**  
- `only1error_95pct_subset_users.csv` — matched error cohort.  
- `noerror_95pct_subset_users.csv` — matched no-error cohort.

**Steps:**  
- **Preprocess for Modeling:**  
  - Optionally remap `ItemId` (e.g., add 10,000 if `isIllegal == 1`) to separate legal/illegal items.  
  - Keep finite values for `Touch_Time_log`, `Swipe_Time_log`, `Total_RT_log`.  
- **Fit LME Models (log scale):**  
  - Touch: `Touch_Time_log ~ TrialsSinceError + (1|UserId) + (1|TrialNumber) + (1|TotalTrials) + (1|ItemId)`  
  - Swipe: `Swipe_Time_log ~ TrialsSinceError + (1|UserId) + (1|TrialNumber) + (1|TotalTrials) + (1|ItemId)`  
  - Total: `Total_RT_log ~ TrialsSinceError + (1|UserId) + (1|TrialNumber) + (1|TotalTrials) + (1|ItemId)`  
- **Extract Residuals:** Add model residuals per trial for each RT (Touch/Swipe/Total), for both groups.  
- **Combine & Window:** Merge groups, add `Group` ∈ {Error, NoError}, keep `TrialsSinceError ∈ [−15, +15]`.  
- **Per-Trial Tests:** For each `TrialsSinceError`, run two-sample t-tests of residuals (Error vs NoError) for Touch, Swipe, and Total (using `broom::tidy`).  
- **Model-Level Coef p-values:** Extract p-values for `TrialsSinceError` from each LME summary (Touch/Swipe/Total).  
- **Plots (log residuals):** Line means with SE ribbons for Touch, Swipe, and Total as a function of `TrialsSinceError` (grouped by Error/NoError).

**Output:**  
- `pvals_touch.csv`, `pvals_swipe.csv`, `pvals_total.csv` — per-trial p-values (Error vs NoError) on log residuals.  
- `test_stats_touch.csv`, `test_stats_swipe.csv`, `test_stats_total.csv` — full per-trial test statistics (estimate, t, df, p) on log residuals.  
- *(Optional if saving)* `touch_residuals_log.png`, `swipe_residuals_log.png`, `total_residuals_log.png` — log-residual plots.

---

### 4. `4_trial-specific_residuals_plots.R`

**Purpose:**  
Recompute residuals **independently at each trial index** (i.e., for each `TrialsSinceError = k`) using intercept-only LMEs, then compare Error vs NoError per trial and visualize residual trajectories.

**Input:**  
- `only1error_95pct_subset_users.csv` — matched error group.  
- `noerror_95pct_subset_users.csv` — matched no-error group.

**Steps:**  
- **Combine & Prepare:** Bind cohorts; set `Group` ∈ {Error, NoError}. Optionally remap `ItemId` when `isIllegal == 1`.  
- **Loop Over Trial Indices:** For each `k ∈ [−15, +15]`, subset `TrialsSinceError == k` and fit intercept-only LMEs:  
  - Touch: `Touch_Time_log ~ 1 + (1|TrialNumber) + (1|ItemId)`  
  - Swipe: `Swipe_Time_log ~ 1 + (1|TrialNumber) + (1|ItemId)`  
  - Total: `Total_RT_log ~ 1 + (1|TrialNumber) + (1|ItemId)`  
  Extract residuals for each RT at trial index `k`.  
- **Bind Residual Sets:** Concatenate residual frames across all `k` for Touch/Swipe/Total.  
- **Per-Trial Tests:** For each `k`, run two-sample t-tests on residuals (Error vs NoError) for the three RTs; tidy outputs with `broom`.  
- **Plots (trial-specific residuals):** Mean residual trajectories with SE ribbons across `TrialsSinceError` for Touch, Swipe, and Total.

**Output:**  
- `pvals_touch_trial.csv`, `pvals_swipe_trial.csv`, `pvals_total_trial.csv` — per-trial p-values (Error vs NoError) using trial-specific residuals.  
- `test_stats_touch_trial.csv`, `test_stats_swipe_trial.csv`, `test_stats_total_trial.csv` — full per-trial test statistics.  

---

## Contact Information  

For any questions regarding this project, please contact:  

**Name:** Sarah Malykke 

**Email:** sarahmalykke@gwu.edu 
