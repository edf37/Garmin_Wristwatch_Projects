# Concatenate All Garmin By-Date and ID Variables into One Dataset
library(dplyr)
# Load Data
setwd("D:\\Evan\\Lab Transfer\\evand\\Garmin All Variable Epochs")

# Load Data
activity_data <- read.csv("RBS_IT3-5_Garmin_Activity_Concatenated.csv")
sleep_data <- read.csv("RBS_IT3-5_Garmin_Sleep_Concatenated.csv")
heart_data <- read.csv("RBS_IT3-5_Garmin_Heart_Rate_Concatenated.csv")
bmr_data_IT3 <- read.csv("garminActivity_merged_IT3.csv")
bmr_data_IT4_5 <- read.csv("garminActivity_merged_IT4-5.csv")
bmr_data_all <- bind_rows(bmr_data_IT3, bmr_data_IT4_5)
bmr_data_all$ActivityDate
# Preview data
head(activity_data)
head(sleep_data)
head(heart_data)

library(dplyr)
library(lubridate)

# Convert character strings like "9/27/2023" to Date objects
bmr_data_all$ActivityDate <- parse_date_time(bmr_data_all$ActivityDate, orders = "mdy HM")
bmr_data_all$ActivityDate <- as_date(bmr_data_all$ActivityDate)  # keep only date, no time

activity_data$ActivityDate <- mdy(activity_data$ActivityDate)
sleep_data$ActivityDate <- mdy(sleep_data$ActivityDate)
heart_data$ActivityDate <- mdy(heart_data$ActivityDate)

# Now join safely by ID and ActivityDate
all_data <- activity_data %>%
  left_join(sleep_data, by = c("ID", "ActivityDate")) %>%
  left_join(heart_data, by = c("ID", "ActivityDate")) %>%
  left_join(bmr_data_all, by = c("ID", "ActivityDate"))

# View combined data
head(all_data)

# Optional: Save to CSV
write.csv(all_data, "RBS_IT3-5_Garmin_AllVariables_Combined.csv", row.names = FALSE)
