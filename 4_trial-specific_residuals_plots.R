#####RESIDUALS (TrialsSinceError == trial)#####


library(dplyr)
library(lme4)
library(ggplot2)
library(dplyr)

# Define the output directory
output_dir <- "/Users/SarahMalykke/Documents/GW/AirportScanner/Manuscript1"

# Combine the datasets
clean_combined_data <- bind_rows(
  only1error_95pct_subset_users %>%
    mutate(Group = "Error"),
  noerror_95pct_subset_users %>%
    mutate(Group = "NoError")
)

# Modify ItemId by adding 10000 when isIllegal == 1
clean_combined_data <- clean_combined_data %>%
  mutate(ItemId = ifelse(isIllegal == 1, ItemId + 10000, ItemId))

# Save to CSV in updated directory
write.csv(clean_combined_data, file.path(output_dir, "clean_combined_data.csv"), row.names = FALSE)



# Define trial ranges
trials_since_error_values <- seq(-15, 15)

# Get unique users
total_users <- unique(clean_combined_data$UserId)
cat("Total users:", length(total_users), "\n")

# Create empty lists to store data
touch_trial_data_list <- list()
swipe_trial_data_list <- list()
total_trial_data_list <- list()


# Iterate over TrialsSinceError values, computing residuals for each subset of data
for (trial in trials_since_error_values) {
  cat("Processing TrialsSinceError Trial:", trial, "\n")
  
  # Filter data to include current trial and preceding trials
  trial_data <- clean_combined_data %>% 
    filter(TrialsSinceError == trial & 
             is.finite(Touch_Time_log) & 
             is.finite(Swipe_Time_log) & 
             is.finite(Total_RT_log))
  
  cat("Number of rows in trial_data for Trial", trial, ":", nrow(trial_data), "\n")
  
  # Fit LME model for Touch_Time_log
  touch_lme_model <- tryCatch(
    {
      lmer(Touch_Time_log ~ 1 + (1 | TrialNumber) + (1 | ItemId), data = trial_data)
    },
    error = function(e) {
      cat("Error fitting Touch model for Trial:", trial, "-", e$message, "\n")
      NULL
    }
  )
  
  # Fit LME model for Swipe_Time_log
  swipe_lme_model <- tryCatch(
    {
      lmer(Swipe_Time_log ~ 1 + (1 | TrialNumber) + (1 | ItemId), data = trial_data)
    },
    error = function(e) {
      cat("Error fitting Swipe model for Trial:", trial, "-", e$message, "\n")
      NULL
    }
  )
  
  # Fit LME model for Total_RT_log
  total_lme_model <- tryCatch(
    {
      lmer(Total_RT_log ~ 1 + (1 | TrialNumber) + (1 | ItemId), data = trial_data)
    },
    error = function(e) {
      cat("Error fitting Total model for Trial:", trial, "-", e$message, "\n")
      NULL
    }
  )
  
  if (!is.null(touch_lme_model)) {
    # Extract residuals for touch time
    trial_data <- trial_data %>%
      mutate(Touch_Residuals = resid(touch_lme_model))
    
    touch_trial_data_list[[as.character(trial)]] <- trial_data
  }
  
  if (!is.null(swipe_lme_model)) {
    # Extract residuals for swipe time
    trial_data <- trial_data %>%
      mutate(Swipe_Residuals = resid(swipe_lme_model))
    
    swipe_trial_data_list[[as.character(trial)]] <- trial_data
  }
  
  if (!is.null(total_lme_model)) {
    # Extract residuals for touch time
    trial_data <- trial_data %>%
      mutate(Total_Residuals = resid(total_lme_model))
    
    total_trial_data_list[[as.character(trial)]] <- trial_data
  }
}

# Combine all residual data into one dataframe
touch_combined_residuals <- bind_rows(touch_trial_data_list, .id = "Trial") %>%
  mutate(TrialsSinceError = as.numeric(TrialsSinceError)) %>%
  filter(TrialsSinceError >= -15 & TrialsSinceError <= 15)

swipe_combined_residuals <- bind_rows(swipe_trial_data_list, .id = "Trial") %>%
  mutate(TrialsSinceError = as.numeric(TrialsSinceError)) %>%
  filter(TrialsSinceError >= -15 & TrialsSinceError <= 15)

total_combined_residuals <- bind_rows(total_trial_data_list, .id = "Trial") %>%
  mutate(TrialsSinceError = as.numeric(TrialsSinceError)) %>%
  filter(TrialsSinceError >= -15 & TrialsSinceError <= 15)


### p-values ###

library(broom)

# Calculate p-values for Touch Residuals (comparing Error vs. NoError) for each trial
pvals_touch_trial <- touch_combined_residuals %>%
  filter(!is.na(Touch_Residuals)) %>%
  group_by(TrialsSinceError) %>%
  do(tidy(t.test(Touch_Residuals ~ Group, data = .))) %>%
  ungroup() %>%
  select(TrialsSinceError, p.value)

print(pvals_touch_trial, n = Inf)

# Calculate p-values for Swipe Residuals (comparing Error vs. NoError) for each trial
pvals_swipe_trial <- swipe_combined_residuals %>%
  filter(!is.na(Swipe_Residuals)) %>%
  group_by(TrialsSinceError) %>%
  do(tidy(t.test(Swipe_Residuals ~ Group, data = .))) %>%
  ungroup() %>%
  select(TrialsSinceError, p.value)

print(pvals_swipe_trial, n = Inf)

# Calculate p-values for Total Residuals (comparing Error vs. NoError) for each trial
pvals_total_trial <- total_combined_residuals %>%
  filter(!is.na(Total_Residuals)) %>%
  group_by(TrialsSinceError) %>%
  do(tidy(t.test(Total_Residuals ~ Group, data = .))) %>%
  ungroup() %>%
  select(TrialsSinceError, p.value)

print(pvals_total_trial, n = Inf)


### save p-values as csv files ###

library(readr)

# Save the Touch p-values
write_csv(pvals_touch_trial, file.path(output_dir, "pvals_touch_trial.csv"))

# Save the Swipe p-values
write_csv(pvals_swipe_trial, file.path(output_dir, "pvals_swipe_trial.csv"))

# Save the Total p-values
write_csv(pvals_total_trial, file.path(output_dir, "pvals_total_trial.csv"))


### all test statistics ###

### Extract Full Test Statistics for Each Trial ###

library(broom)

# For Touch Residuals (trial-level)
test_stats_touch_trial <- touch_combined_residuals %>%
  filter(!is.na(Touch_Residuals)) %>%
  group_by(TrialsSinceError) %>%
  do(tidy(t.test(Touch_Residuals ~ Group, data = .))) %>%
  ungroup() %>%
  select(TrialsSinceError, estimate, statistic, parameter, p.value)

print(test_stats_touch_trial, n = Inf)

# For Swipe Residuals (trial-level)
test_stats_swipe_trial <- swipe_combined_residuals %>%
  filter(!is.na(Swipe_Residuals)) %>%
  group_by(TrialsSinceError) %>%
  do(tidy(t.test(Swipe_Residuals ~ Group, data = .))) %>%
  ungroup() %>%
  select(TrialsSinceError, estimate, statistic, parameter, p.value)

print(test_stats_swipe_trial, n = Inf)

# For Total Residuals (trial-level)
test_stats_total_trial <- total_combined_residuals %>%
  filter(!is.na(Total_Residuals)) %>%
  group_by(TrialsSinceError) %>%
  do(tidy(t.test(Total_Residuals ~ Group, data = .))) %>%
  ungroup() %>%
  select(TrialsSinceError, estimate, statistic, parameter, p.value)

print(test_stats_total_trial, n = Inf)

# Save these full test statistics as CSV files
library(readr)
write_csv(test_stats_touch_trial, file.path(output_dir, "test_stats_touch_trial.csv"))
write_csv(test_stats_swipe_trial, file.path(output_dir, "test_stats_swipe_trial.csv"))
write_csv(test_stats_total_trial, file.path(output_dir, "test_stats_total_trial.csv"))





# Define a custom theme
plot_theme <- theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    text = element_text(family = "Garamond", size = 20)  # Set font to Garamond and increase size
  )

# PLOT: TOUCH RESIDUALS
touch_plot <- ggplot(touch_combined_residuals, aes(x = TrialsSinceError, y = Touch_Residuals, color = Group)) +
  geom_line(stat = "summary", fun = mean, linewidth = 1.2) +  
  geom_ribbon(stat = "summary", fun.data = mean_se, alpha = 0.2) +
  labs(title = "Touch Residuals",
       x = "Trials Since Error", y = "Touch Time Residuals (Log)") +
  scale_x_continuous(breaks = seq(-15, 15, by = 1)) +
  plot_theme

# Display plot
print(touch_plot)

# PLOT: SWIPE RESIDUALS
swipe_plot <- ggplot(swipe_combined_residuals, aes(x = TrialsSinceError, y = Swipe_Residuals, color = Group)) +
  geom_line(stat = "summary", fun = mean, linewidth = 1.2) +
  geom_ribbon(stat = "summary", fun.data = mean_se, alpha = 0.2) +
  labs(title = "Swipe Residuals",
       x = "Trials Since Error", y = "Swipe Time Residuals (Log)") +
  scale_x_continuous(breaks = seq(-15, 15, by = 1)) +
  plot_theme

# Display plot
print(swipe_plot)


# PLOT: TOTAL RESIDUALS
total_plot <- ggplot(total_combined_residuals, aes(x = TrialsSinceError, y = Total_Residuals, color = Group)) +
  geom_line(stat = "summary", fun = mean, linewidth = 1.2) +
  geom_ribbon(stat = "summary", fun.data = mean_se, alpha = 0.2) +
  labs(title = "Total Residuals",
       x = "Trials Since Error", y = "Total Time Residuals (Log)") +
  scale_x_continuous(breaks = seq(-15, 15, by = 1)) +
  plot_theme

# Display plot
print(total_plot)



