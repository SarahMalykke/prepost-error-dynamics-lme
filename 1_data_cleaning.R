#####DATA CLEANING#####
#load necessary libraries and packages at the start
packages <- c("dplyr", "lme4", "ggplot2", "tibble", "lmerTest")

#install and load packages
installed_packages <- packages %in% rownames(installed.packages())
if (any(!installed_packages)) {
  install.packages(packages[!installed_packages])
}

#load all required packages
lapply(packages, library, character.only = TRUE)

#import flash challenge data
Flash_95_percent <- read.csv("~/Documents/GW/AirportScanner/sandbox/95%/Flash_95%_Sarah.csv")

#making touch, swipe, and total RT
FlashData_95pct <- Flash_95_percent %>%
  mutate(
    Touch_Time = DragTime - WaitTime,
    Swipe_Time = ReleaseTime - DragTime,
    Total_RT = Swipe_Time + Touch_Time
  )

#log transforming RTs
FlashData_95pct <- FlashData_95pct %>%
  mutate(
    Touch_Time_log = log(Touch_Time),
    Swipe_Time_log = log(Swipe_Time),
    Total_RT_log = log(Total_RT)
  )

#first "level" of the game, exclude outlier RT, enough correct trials
FlashData_95pct <- FlashData_95pct %>%
  filter(Tier == 1, Day == 1, Total_RT > 250, Total_RT < 5000, NumCorrect >= 8) %>%
  group_by(UserId) %>%
  filter(ChalengeId == first(ChalengeId)) %>% #first gameplay Tier 1 Day 1
  ungroup()

#create trial number by row
FlashData_95pct <- FlashData_95pct %>%
  group_by(UserId) %>%
  mutate(TrialNumber = row_number()) %>%
  ungroup()


write.csv(FlashData_95pct, "~/Documents/GW/AirportScanner/Manuscript1/FlashData_95pct.csv")

#get N-1 accuracy
FlashData_95pct <- FlashData_95pct %>%
  group_by(UserId) %>%
  arrange(UserId, TrialNumber) %>%
  mutate(Previous_Trial_Accuracy = lag(isCorrect))

#filter for only one error per subject
only1error_95pct <- FlashData_95pct %>%
  group_by(UserId) %>%
  filter(sum(isCorrect == 0) == 1) %>%
  ungroup()

#create column to classify the trial as pre-error, error, and post-error
only1error_95pct <- only1error_95pct %>%
  group_by(UserId) %>%
  mutate(
    Error_Trial = which(isCorrect == 0), # Find the error trial number
    Error_Status = case_when(
      row_number() == Error_Trial ~ "Error",
      row_number() == (Error_Trial - 1) ~ "Pre-Error",
      row_number() == (Error_Trial + 1) ~ "Post-Error",
      TRUE ~ "Other" # Label as "Other" for trials that are not pre-error, error, or post-error
    )
  ) %>%
  ungroup()

#total trial for error group
only1error_95pct <- only1error_95pct %>%
  group_by(UserId) %>%
  mutate(TotalTrials = n()) %>%
  ungroup()

#just to get the Error_Status in the order I want
only1error_95pct <- only1error_95pct %>%
  mutate(Error_Status = factor(Error_Status, levels = c("Pre-Error", "Error", "Post-Error")))

#z-scored touch and swipe times
only1error_95pct <- only1error_95pct %>%
  group_by(UserId) %>%
  mutate(
    Touch_Time_Z = scale(Touch_Time),
    Swipe_Time_Z = scale(Swipe_Time)
  ) %>%
  ungroup()


#user count (to be used to make the no-error group equivalent)
subject_count <- only1error_95pct %>%
  summarise(Unique_Subjects = n_distinct(UserId))

#print the subject count
print(subject_count)



#identify the trial number where each user made an error
error_trials <- only1error_95pct %>%
  filter(Error_Status == "Error") %>%
  select(UserId, ErrorTrial = TrialNumber)

#display the error_trials dataset
print("Error Trials:")
print(head(error_trials))


#check if ErrorTrial column exists in the original dataset
if ("ErrorTrial" %in% colnames(only1error_95pct)) {
  only1error_95pct <- only1error_95pct %>% select(-ErrorTrial)
}

#join the ErrorTrial information back to the original data to compute trial index relative to the error
only1error_95pct <- only1error_95pct %>%
  left_join(error_trials, by = "UserId") %>%
  mutate(TrialsSinceError = TrialNumber - ErrorTrial) %>%
  filter(!is.na(TrialsSinceError))  # This removes any rows where no error was recorded

#display the updated dataset
print("Updated Data with TrialsSinceError:")
print(head(only1error_95pct))


write.csv(only1error_95pct, "~/Documents/GW/AirportScanner/Manuscript1/only1error_95pct.csv")

