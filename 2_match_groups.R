##### NO-ERROR GROUP SAMPLING #####

#filtering for NO error per subject
noerror_95pct <- FlashData_95pct %>%
  group_by(UserId) %>%
  filter(all(isCorrect == 1)) %>%
  ungroup()

#save the noerror_95pct output
write.csv(noerror_95pct, "~/Documents/GW/AirportScanner/Manuscript1/noerror_95pct.csv")


### no-error sampling ###

library(dplyr)

# Load datasets
only1error_95pct <- read.csv("/Users/SarahMalykke/Documents/GW/AirportScanner/Manuscript1/only1error_95pct.csv")
noerror_95pct <- read.csv("/Users/SarahMalykke/Documents/GW/AirportScanner/Manuscript1/noerror_95pct.csv")


# Compute TotalTrials for no-error group
noerror_95pct <- noerror_95pct %>%
  group_by(UserId) %>%
  mutate(TotalTrials = n()) %>%
  ungroup()

# Step 1: Find the `TotalTrials` values that exist in both datasets
matching_total_trials <- intersect(only1error_95pct$TotalTrials, noerror_95pct$TotalTrials)

# Step 2: Identify users before filtering
unique_users_only1error_before <- n_distinct(only1error_95pct$UserId)
unique_users_noerror_before <- n_distinct(noerror_95pct$UserId)

# Print how many users we started with
print(paste("Starting unique users in only1error_95pct:", unique_users_only1error_before))
print(paste("Starting unique users in noerror_95pct:", unique_users_noerror_before))

# Step 3: Filter both datasets to keep only users with these `TotalTrials`
only1error_95pct_filtered <- only1error_95pct %>%
  filter(TotalTrials %in% matching_total_trials)

noerror_95pct_filtered <- noerror_95pct %>%
  filter(TotalTrials %in% matching_total_trials)

# Step 4: Identify users after filtering
unique_users_only1error_after <- n_distinct(only1error_95pct_filtered$UserId)
unique_users_noerror_after <- n_distinct(noerror_95pct_filtered$UserId)

# Step 5: Print the number of users removed and remaining
print(paste("Unique users removed from only1error_95pct:", unique_users_only1error_before - unique_users_only1error_after))
print(paste("Unique users removed from noerror_95pct:", unique_users_noerror_before - unique_users_noerror_after))
print(paste("Remaining unique users in only1error_95pct:", unique_users_only1error_after))
print(paste("Remaining unique users in noerror_95pct:", unique_users_noerror_after))

# Step 1: Create summary tables of unique UserId counts per TotalTrials
error_totalTrial_user_counts <- only1error_95pct_filtered %>%
  group_by(TotalTrials) %>%
  summarise(ErrorUserCount = n_distinct(UserId)) %>%
  arrange(TotalTrials)

noerror_totalTrial_user_counts <- noerror_95pct_filtered %>%
  group_by(TotalTrials) %>%
  summarise(NoErrorUserCount = n_distinct(UserId)) %>%
  arrange(TotalTrials)

# Step 2: Ensure that both tables contain the exact same TotalTrials values
if (!setequal(error_totalTrial_user_counts$TotalTrials, noerror_totalTrial_user_counts$TotalTrials)) {
  stop("Mismatch in TotalTrials! Some TotalTrials values exist in one dataset but not the other.")
}

# Step 3: Merge the tables (this will fail if TotalTrials do not match, preventing data loss)
totalTrial_user_counts <- inner_join(error_totalTrial_user_counts, noerror_totalTrial_user_counts, by = "TotalTrials")

# Save
# Define the file path
total_trial_counts_file <- "/Users/SarahMalykke/Documents/GW/AirportScanner/Manuscript1/totalTrial_user_counts.csv"
write.csv(totalTrial_user_counts, total_trial_counts_file, row.names = FALSE)
print(paste("totalTrial_user_counts saved to:", total_trial_counts_file))

# Make a dataframe to keep trial of the available no error User Ids
noerror_UserIds <- noerror_95pct_filtered %>%
  group_by(UserId) %>%
  summarise(TotalTrials = n()) %>%
  ungroup()

# Create a table of Error UserIds and their TotalTrial and ErrorTrial values
error_UserIds <- only1error_95pct_filtered %>%
  distinct(UserId, TotalTrials, ErrorTrial)  # Keep one row per UserId

set.seed(42)  # Ensure reproducibility

# Initialize empty dataframes
subset_noerror_userIds <- data.frame(UserId = integer(), TotalTrials = integer())
subset_error_userIds <- data.frame(UserId = integer(), TotalTrials = integer(), ErrorTrial = integer())

# Loop through each TotalTrials value in totalTrial_user_counts
for (i in 1:nrow(totalTrial_user_counts)) {
  
  total_trials_value <- totalTrial_user_counts$TotalTrials[i]
  
  num_error_users <- totalTrial_user_counts$ErrorUserCount[i]
  num_noerror_users <- totalTrial_user_counts$NoErrorUserCount[i]
  
  # Find eligible users in both groups for this TotalTrials value
  eligible_noerror_users <- noerror_UserIds %>%
    filter(TotalTrials == total_trials_value)
  
  eligible_error_users <- error_UserIds %>%
    filter(TotalTrials == total_trials_value)
  
  # Determine the number of users to sample (taking the smaller count)
  num_to_sample <- min(nrow(eligible_noerror_users), nrow(eligible_error_users))
  
  # Sample users from both groups, ensuring they match in size
  sampled_noerror_users <- eligible_noerror_users %>%
    sample_n(num_to_sample, replace = FALSE)
  
  sampled_error_users <- eligible_error_users %>%
    sample_n(num_to_sample, replace = FALSE)
  
  # Append sampled users to the result dataframes
  subset_noerror_userIds <- bind_rows(subset_noerror_userIds, sampled_noerror_users)
  subset_error_userIds <- bind_rows(subset_error_userIds, sampled_error_users)
}

# View the final selected users
View(subset_noerror_userIds)
View(subset_error_userIds)

# Verify that row counts match
print(paste("Final row counts: subset_noerror_userIds =", nrow(subset_noerror_userIds),
            ", subset_error_userIds =", nrow(subset_error_userIds)))

# Now carry ErrorTrial assignment from subset_error_userIds over to subset_noerror_userIds
# Step 1: Check that UserId counts match per TotalTrials before merging
noerror_counts <- subset_noerror_userIds %>%
  count(TotalTrials, name = "NoError_Count")

error_counts <- subset_error_userIds %>%
  count(TotalTrials, name = "Error_Count")

# Ensure that both datasets have the same number of users per TotalTrials

# Step 1: Check that UserId counts match per TotalTrials before merging
final_noerror_counts <- subset_noerror_userIds %>%
  count(TotalTrials, name = "NoError_Count")

final_error_counts <- subset_error_userIds %>%
  count(TotalTrials, name = "Error_Count")

# Ensure that both datasets have the same number of users per TotalTrials
if (!all(final_noerror_counts$NoError_Count == final_error_counts$Error_Count)) {
  stop("Mismatch detected! Number of users per TotalTrials does not match between subset_noerror_userIds and subset_error_userIds.")
} else {
  print("User counts per TotalTrials match!")
}
if (!all(final_noerror_counts$NoError_Count == final_error_counts$Error_Count)) {
  stop("Mismatch detected! Number of users per TotalTrials does not match between subset_noerror_userIds and subset_error_userIds.")
} else {
  print("User counts per TotalTrials match!")
}

# Step 2: Ensure both dataframes are ordered the same way before copying
subset_noerror_userIds <- subset_noerror_userIds %>%
  arrange(TotalTrials, UserId)

subset_error_userIds <- subset_error_userIds %>%
  arrange(TotalTrials, UserId)

# Step 3: Copy over the `ErrorTrial` values from `subset_error_userIds` to `subset_noerror_userIds`
subset_noerror_userIds <- subset_noerror_userIds %>%
  mutate(ErrorTrial = subset_error_userIds$ErrorTrial)

# Step 4: View the updated no-error dataset
View(subset_noerror_userIds)

# Step 5: Print verification message
print("ErrorTrial values successfully copied over to subset_noerror_userIds!")

# SAVING OUT THE CORRECT USERID SUBSET LISTS FOR BOTH ERROR AND NO ERROR GROUPS

# Define file paths
noerror_file <- "/Users/SarahMalykke/Documents/GW/AirportScanner/Manuscript1/subset_noerror_userIds.csv"
error_file <- "/Users/SarahMalykke/Documents/GW/AirportScanner/Manuscript1/subset_error_userIds.csv"

# Save the CSV files
write.csv(subset_noerror_userIds, noerror_file, row.names = FALSE)
write.csv(subset_error_userIds, error_file, row.names = FALSE)

# Print confirmation messages
print(paste("subset_noerror_userIds saved to:", noerror_file))
print(paste("subset_error_userIds saved to:", error_file))

#####
# NOW PREP NO ERROR 5 PCT (TRIAL-BASED) BY
# 1. FILTERING IT BASED ON THE CORRECT SUBSET OF USER IDS 
# 2. CALCULATE TRIALS SINCE ERROR
#####

# Filter noerror_95pct to include only the selected UserIds
noerror_95pct_subset_users <- noerror_95pct %>%
  filter(UserId %in% subset_noerror_userIds$UserId)

# Verify that the filtering worked correctly
print(paste("Original noerror_95pct user count:", n_distinct(noerror_95pct$UserId)))
print(paste("Filtered noerror_95pct user count:", n_distinct(noerror_95pct_subset_users$UserId)))

# Add in ErrorTrial
noerror_95pct_subset_users <- noerror_95pct_subset_users %>%
  left_join(subset_noerror_userIds %>% select(UserId, ErrorTrial), by = "UserId")

# Calculate trials since error based on ErrorTrial
noerror_95pct_subset_users <- noerror_95pct_subset_users %>%
  mutate(TrialsSinceError = TrialNumber - ErrorTrial) 

#####
# NOW PREP ERROR 5 PCT (TRIAL-BASED) BY
# 1. FILTERING IT BASED ON THE CORRECT SUBSET OF USER IDS 
#####

# Filter noerror_95pct to include only the selected UserIds
only1error_95pct_subset_users <- only1error_95pct %>%
  filter(UserId %in% subset_error_userIds$UserId)

# Verify that the filtering worked correctly
print(paste("Original only1error_95pct user count:", n_distinct(only1error_95pct$UserId)))
print(paste("Filtered only1error_95pct_subset_users user count:", n_distinct(only1error_95pct_subset_users$UserId)))

#####
# save the final dataframes
#####

# Save the CSV files
write.csv(noerror_95pct_subset_users, "/Users/SarahMalykke/Documents/GW/AirportScanner/Manuscript1/noerror_95pct_subset_users.csv", row.names = FALSE)
write.csv(only1error_95pct_subset_users, "/Users/SarahMalykke/Documents/GW/AirportScanner/Manuscript1/only1error_95pct_subset_users.csv", row.names = FALSE)



