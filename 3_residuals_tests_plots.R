#####RESIDUALS#####

output_dir <- "/Users/SarahMalykke/Documents/GW/AirportScanner/Manuscript1"

# Load necessary libraries
library(dplyr)
library(lme4)
library(lmerTest)
library(ggplot2)

# Create copies of the datasets to avoid modifying originals
only1error_residuals_log <- only1error_95pct_subset_users
noerror_residuals_log <- noerror_95pct_subset_users

# Modify ItemId by adding 10000 when isIllegal == 1
only1error_residuals_log <- only1error_residuals_log %>%
  mutate(ItemId = ifelse(isIllegal == 1, ItemId + 10000, ItemId))

noerror_residuals_log <- noerror_residuals_log %>%
  mutate(ItemId = ifelse(isIllegal == 1, ItemId + 10000, ItemId))

# Filter the dataset to include only rows with finite values
only1error_residuals_log <- only1error_residuals_log %>%
  filter(is.finite(Touch_Time_log) & is.finite(Swipe_Time_log) & is.finite(Total_RT_log))


# Fit LME models for Touch and Swipe residuals (log-transformed) - ERROR GROUP (Include ItemId)
touch_lme_model_log <- lmer(Touch_Time_log ~ TrialsSinceError + (1|UserId) + (1|TrialNumber) + (1|TotalTrials) + (1|ItemId), data = only1error_residuals_log)
swipe_lme_model_log <- lmer(Swipe_Time_log ~ TrialsSinceError + (1|UserId) + (1|TrialNumber) + (1|TotalTrials) + (1|ItemId), data = only1error_residuals_log)
total_lme_model_log <- lmer(Total_RT_log ~ TrialsSinceError + (1|UserId) + (1|TrialNumber) + (1|TotalTrials) + (1|ItemId), data = only1error_residuals_log)


# Add residuals to the new dataframe (Error group)
only1error_residuals_log <- only1error_residuals_log %>%
  mutate(Touch_Residuals_Log = resid(touch_lme_model_log),
         Swipe_Residuals_Log = resid(swipe_lme_model_log),
         Total_Residuals_Log = resid(total_lme_model_log))


noerror_residuals_log <- noerror_residuals_log %>%
  filter(is.finite(Touch_Time_log) & is.finite(Swipe_Time_log) & is.finite(Total_RT_log))

# Fit LME models for Touch and Swipe residuals (log-transformed) - NO ERROR GROUP (Include ItemId)
noerror_touch_lme_model_log <- lmer(Touch_Time_log ~ TrialsSinceError + (1|UserId) + (1|TrialNumber) + (1|TotalTrials) + (1|ItemId), data = noerror_residuals_log)
noerror_swipe_lme_model_log <- lmer(Swipe_Time_log ~ TrialsSinceError + (1|UserId) + (1|TrialNumber) + (1|TotalTrials) + (1|ItemId), data = noerror_residuals_log)
noerror_total_lme_model_log <- lmer(Total_RT_log ~ TrialsSinceError + (1|UserId) + (1|TrialNumber) + (1|TotalTrials) + (1|ItemId), data = noerror_residuals_log)


# Add residuals to the new dataframe (No-error group)
noerror_residuals_log <- noerror_residuals_log %>%
  mutate(Touch_Residuals_Log = resid(noerror_touch_lme_model_log),
         Swipe_Residuals_Log = resid(noerror_swipe_lme_model_log),
         Total_Residuals_Log = resid(noerror_total_lme_model_log))

# Combine the two datasets into a new one
combined_relevant_log_subset <- bind_rows(
  only1error_residuals_log %>%
    select(UserId, TrialNumber, TrialsSinceError, ItemId, Touch_Residuals_Log, Swipe_Residuals_Log, Total_Residuals_Log) %>%
    mutate(Group = "Error"),
  noerror_residuals_log %>%
    select(UserId, TrialNumber, TrialsSinceError, ItemId, Touch_Residuals_Log, Swipe_Residuals_Log, Total_Residuals_Log) %>%
    mutate(Group = "NoError")
)

# Ensure TrialsSinceError is numeric for plotting
combined_relevant_log_subset <- combined_relevant_log_subset %>%
  mutate(TrialsSinceError = as.numeric(TrialsSinceError))

# Filter the dataset to include only TrialsSinceError from -15 to 15
combined_relevant_log_subset <- combined_relevant_log_subset %>%
  filter(TrialsSinceError >= -15 & TrialsSinceError <= 15)

###p-values***

library(broom)

# Calculate p-values for Touch Residuals for each trial (comparing Error vs. NoError)
pvals_touch <- combined_relevant_log_subset %>%
  filter(!is.na(Touch_Residuals_Log)) %>%
  group_by(TrialsSinceError) %>%
  do(tidy(t.test(Touch_Residuals_Log ~ Group, data = .))) %>%
  ungroup() %>%
  select(TrialsSinceError, p.value)

print(pvals_touch)

# Calculate p-values for Swipe Residuals for each trial (comparing Error vs. NoError)
pvals_swipe <- combined_relevant_log_subset %>%
  filter(!is.na(Swipe_Residuals_Log)) %>%
  group_by(TrialsSinceError) %>%
  do(tidy(t.test(Swipe_Residuals_Log ~ Group, data = .))) %>%
  ungroup() %>%
  select(TrialsSinceError, p.value)

print(pvals_swipe)

# Calculate p-values for Total Residuals for each trial (comparing Error vs. NoError)
pvals_total <- combined_relevant_log_subset %>%
  filter(!is.na(Total_Residuals_Log)) %>%
  group_by(TrialsSinceError) %>%
  do(tidy(t.test(Total_Residuals_Log ~ Group, data = .))) %>%
  ungroup() %>%
  select(TrialsSinceError, p.value)

print(pvals_total)


##save as csv ##
library(readr)

# Save Touch p-values to CSV
write_csv(pvals_touch, "pvals_touch.csv")

# Save Swipe p-values to CSV
write_csv(pvals_swipe, "pvals_swipe.csv")

# Save Total p-values to CSV
write_csv(pvals_total, "pvals_total.csv")



### all test statistics ###

# Extract full test statistics for Touch Residuals for each trial
test_stats_touch <- combined_relevant_log_subset %>%
  filter(!is.na(Touch_Residuals_Log)) %>%
  group_by(TrialsSinceError) %>%
  do(tidy(t.test(Touch_Residuals_Log ~ Group, data = .))) %>%
  ungroup() %>%
  select(TrialsSinceError, estimate, statistic, parameter, p.value)

print(test_stats_touch, n = Inf)

# Extract full test statistics for Swipe Residuals for each trial
test_stats_swipe <- combined_relevant_log_subset %>%
  filter(!is.na(Swipe_Residuals_Log)) %>%
  group_by(TrialsSinceError) %>%
  do(tidy(t.test(Swipe_Residuals_Log ~ Group, data = .))) %>%
  ungroup() %>%
  select(TrialsSinceError, estimate, statistic, parameter, p.value)

print(test_stats_swipe, n = Inf)

# Extract full test statistics for Total Residuals for each trial
test_stats_total <- combined_relevant_log_subset %>%
  filter(!is.na(Total_Residuals_Log)) %>%
  group_by(TrialsSinceError) %>%
  do(tidy(t.test(Total_Residuals_Log ~ Group, data = .))) %>%
  ungroup() %>%
  select(TrialsSinceError, estimate, statistic, parameter, p.value)

print(test_stats_total, n = Inf)


### save as csv files ###

library(readr)

# Save the full test statistics for Touch, Swipe, and Total as CSV files
write_csv(test_stats_touch, file.path(output_dir, "test_stats_touch.csv"))
write_csv(test_stats_swipe, file.path(output_dir, "test_stats_swipe.csv"))
write_csv(test_stats_total, file.path(output_dir, "test_stats_total.csv"))


## Plots ##

# Define a custom theme for consistent aesthetics
log_theme <- theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    text = element_text(family = "Garamond", size = 20)  # Set font to Garamond and increase size
  )

### PLOT: TOUCH RESIDUALS (LOG)
touch_plot_log <- ggplot(combined_relevant_log_subset, aes(x = TrialsSinceError, y = Touch_Residuals_Log, color = Group)) +
  geom_line(stat = "summary", fun = mean, linewidth = 1.2) +  # Adjust line thickness
  geom_ribbon(stat = "summary", fun.data = mean_se, alpha = 0.2) +
  labs(title = "",
       x = "Trials Since Error", y = "Touch Time Residuals (Log)") +
  scale_x_continuous(breaks = seq(-15, 15, by = 1)) +
  log_theme

### PLOT: SWIPE RESIDUALS (LOG)
swipe_plot_log <- ggplot(combined_relevant_log_subset, aes(x = TrialsSinceError, y = Swipe_Residuals_Log, color = Group)) +
  geom_line(stat = "summary", fun = mean, linewidth = 1.2) +
  geom_ribbon(stat = "summary", fun.data = mean_se, alpha = 0.2) +
  labs(title = "",
       x = "Trials Since Error", y = "Swipe Time Residuals (Log)") +
  scale_x_continuous(breaks = seq(-15, 15, by = 1)) +
  log_theme

### PLOT: TOTAL RESIDUALS (LOG)
total_plot_log <- ggplot(combined_relevant_log_subset, aes(x = TrialsSinceError, y = Total_Residuals_Log, color = Group)) +
  geom_line(stat = "summary", fun = mean, linewidth = 1.2) +
  geom_ribbon(stat = "summary", fun.data = mean_se, alpha = 0.2) +
  labs(title = "",
       x = "Trials Since Error", y = "Total Time Residuals (Log)") +
  scale_x_continuous(breaks = seq(-15, 15, by = 1)) +
  log_theme

# Display plots
print(touch_plot_log)
print(swipe_plot_log)
print(total_plot_log)

##p-values##
# For Touch
summary_touch <- summary(touch_lme_model_log)
p_touch <- summary_touch$coefficients["TrialsSinceError", "Pr(>|t|)"]
print(p_touch)

# For Swipe
summary_swipe <- summary(swipe_lme_model_log)
p_swipe <- summary_swipe$coefficients["TrialsSinceError", "Pr(>|t|)"]
print(p_swipe)

# For Total
summary_total <- summary(total_lme_model_log)
p_total <- summary_total$coefficients["TrialsSinceError", "Pr(>|t|)"]
print(p_total)

