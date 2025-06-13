# ====================================================
# Garmin Energy Expenditure, Activity, Sleep, and Heart Rate Data Processing and Summarization Script
# Author: Evan D. Feigel
# Purpose: Process, summarize, and combine Garmin activity, sleep, and HR data into a combined file
# Inputs: CSV files for activity logs, sleep logs, and HR data
# Outputs: Summarized files by participant and day, combined datasets
# ====================================================

library(dplyr)
library(lubridate)

# Function to load and preprocess data
load_and_preprocess <- function(filepath, date_column, tz = "UTC") {
  data <- read.csv(filepath)
  
  # Convert date column to POSIXct and add date-only column
  data[[date_column]] <- as.POSIXct(data[[date_column]], format = "%m/%d/%Y %I:%M %p", tz = tz)
  data$ActivityDate <- as.Date(data[[date_column]])
  
  return(data)
}

# Function to summarize daily metrics
summarize_daily <- function(data, group_vars, metrics, na_handling = TRUE) {
  data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(across(
      all_of(metrics), 
      list(
        total = ~ sum(.x, na.rm = na_handling),
        avg = ~ mean(.x, na.rm = na_handling)
      ),
      .names = "{.col}_{.fn}"
    ), .groups = "drop")
}

# Function to process all files in a directory matching a pattern
process_files_in_directory <- function(input_dir, pattern, date_column, output_dir, prefix, summary_vars, metrics) {
  files <- list.files(input_dir, pattern = pattern, full.names = TRUE)
  combined_data <- lapply(files, function(file) {
    data <- load_and_preprocess(file, date_column)
    summarized <- summarize_daily(data, group_vars = c("ActivityDate"), metrics = metrics)
    write.csv(summarized, file.path(output_dir, paste0(prefix, basename(file))), row.names = FALSE)
    summarized
  })
  combined_data <- bind_rows(combined_data)
  write.csv(combined_data, file.path(output_dir, paste0(prefix, "combined.csv")), row.names = FALSE)
}

# Main Workflow
main <- function() {
  # Set input/output directories
  input_dir <- "D://Evan//Lab Transfer//evand"
  output_dir <- "D://Evan//Lab Transfer//evand//BoBCAT Garmin Summaries"
  dir.create(output_dir, showWarnings = FALSE)
  
  # Activity Log Processing
  process_files_in_directory(
    input_dir = input_dir,
    pattern = "garminEpochLog.*\\.csv",
    date_column = "ActivityDateTime",
    output_dir = output_dir,
    prefix = "",
    summary_vars = c("ActivityDate"),
    metrics = c("ActiveKilocalories", "Steps", "DistanceInMeters", "ActiveTimeInSeconds", "Met")
  )
  
  # Sleep Log Processing
  process_files_in_directory(
    input_dir = input_dir,
    pattern = "garminSleep.*\\.csv",
    date_column = "ActivityDateTime",
    output_dir = output_dir,
    prefix = "",
    summary_vars = c("ActivityDate"),
    metrics = c("DeepSleepDurationInSeconds", "LightSleepDurationInSeconds", "AwakeDurationInSeconds", "RemSleepInSeconds")
  )
  
  # Combine Summaries
  combine_summaries(output_dir, "summarized_", output_dir, "combined_summaries.csv")
}

# Function to combine multiple summary files
combine_summaries <- function(input_dir, pattern, output_dir, output_file) {
  files <- list.files(input_dir, pattern = paste0("^", pattern), full.names = TRUE)
  combined_data <- bind_rows(lapply(files, read.csv))
  write.csv(combined_data, file.path(output_dir, output_file), row.names = FALSE)
}

# Run the script
main()

##########################
# Concatenate the Activity EpochLog files into One File Summarized by Date and ID
library(dplyr)
library(lubridate)
library(stringr)

# 1. Load and preprocess with ID extraction
load_and_preprocess_with_id <- function(filepath, date_column, tz = "UTC") {
  data <- read.csv(filepath)
  
  # Skip empty files
  if (nrow(data) == 0) return(NULL)
  
  # Extract ID from filename
  id <- str_extract(basename(filepath), "^RBS\\d+")
  data$ID <- id
  
  # Convert date-time and create date-only column
  data[[date_column]] <- parse_date_time(
    data[[date_column]],
    orders = c("mdY HMS p", "mdY IMS p", "ymd HMS", "mdY HM"),
    tz = tz
  )
  data$ActivityDate <- as.Date(data[[date_column]])
  
  return(data)
}


# 2. Summarize daily data by ID
summarize_by_id_and_date <- function(data, metrics) {
  data %>%
    group_by(ID, ActivityDate) %>%
    summarise(across(
      all_of(metrics),
      list(total = ~sum(.x, na.rm = TRUE), avg = ~mean(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ), .groups = "drop")
}

# 3. Main processing function
process_all_garmin_epoch <- function(input_dir, date_column, metrics, output_file) {
  files <- list.files(input_dir, pattern = "_garminEpoch.*\\.csv$", full.names = TRUE)
  
  # Load and preprocess all files
  all_data <- lapply(files, load_and_preprocess_with_id, date_column = date_column)
  all_data <- bind_rows(all_data)
  
  # Summarize
  summarized_data <- summarize_by_id_and_date(all_data, metrics)
  
  # Write to CSV
  write.csv(summarized_data, file.path(input_dir, output_file), row.names = FALSE)
}

# 4. Run
process_all_garmin_epoch(
  input_dir = "D://Evan//Lab Transfer//evand//Garmin Activity Epochs",  # adjust as needed
  date_column = "ActivityDateTime",
  metrics = c("ActiveKilocalories", "Steps", "DistanceInMeters", "ActiveTimeInSeconds", "Met"),
  output_file = "combined_garminEpoch_summarized_by_ID.csv"
)

##########################
# Concatenate the Sleep files into One File Summarized by Date and ID
library(dplyr)
library(lubridate)
library(stringr)

# 1. Load and preprocess with ID extraction
load_and_preprocess_with_id <- function(filepath, date_column, tz = "UTC") {
  data <- read.csv(filepath)
  
  # Extract ID from filename
  id <- str_extract(basename(filepath), "^RBS\\d+")
  data$ID <- id
  
  # Convert date-time and create date-only column
  data[[date_column]] <- parse_date_time(
    data[[date_column]],
    orders = c("mdY HMS p", "mdY IMS p", "ymd HMS", "mdY HM"),
    tz = tz
  )
  data$ActivityDate <- as.Date(data[[date_column]])
  
  return(data)
}

# 2. Summarize daily data by ID
summarize_by_id_and_date <- function(data, metrics) {
  data %>%
    group_by(ID, ActivityDate) %>%
    summarise(across(
      all_of(metrics),
      list(total = ~sum(.x, na.rm = TRUE), avg = ~mean(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ), .groups = "drop")
}

# 3. Main processing function
process_all_garmin_epoch <- function(input_dir, date_column, metrics, output_file) {
  files <- list.files(input_dir, pattern = "_garminSleep.*\\.csv$", full.names = TRUE)
  
  # Load and preprocess all files
  all_data <- lapply(files, load_and_preprocess_with_id, date_column = date_column)
  all_data <- bind_rows(all_data)
  
  # Summarize
  summarized_data <- summarize_by_id_and_date(all_data, metrics)
  
  # Write to CSV
  write.csv(summarized_data, file.path(input_dir, output_file), row.names = FALSE)
}

# 4. Run
process_all_garmin_epoch(
  input_dir = "D://Evan//Lab Transfer//evand//Garmin Sleep Epochs",  # adjust as needed
  date_column = "ActivityDateTime",
  metrics = c("DeepSleepDurationInSeconds", "LightSleepDurationInSeconds", "AwakeDurationInSeconds", "RemSleepInSeconds"),
  output_file = "combined_garminSleep_summarized_by_ID.csv"
)
###############################
# Concatenate Sleep files into One file by ID and Date (run if there are some participants with 0 data)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

# 1. Load and preprocess with ID extraction
load_and_preprocess_with_id <- function(filepath, date_column, tz = "UTC") {
  data <- read.csv(filepath)
  
  # Skip empty files
  if (nrow(data) == 0) return(NULL)
  
  # Extract ID from filename
  id <- str_extract(basename(filepath), "^RBS\\d+")
  data$ID <- id
  
  # Convert date-time and create date-only column
  data[[date_column]] <- parse_date_time(
    data[[date_column]],
    orders = c("mdY HMS p", "mdY IMS p", "ymd HMS", "mdY HM"),
    tz = tz
  )
  data$ActivityDate <- as.Date(data[[date_column]])
  
  return(data)
}


# 2. Summarize daily data by ID
summarize_by_id_and_date <- function(data, metrics) {
  data %>%
    group_by(ID, ActivityDate) %>%
    summarise(across(
      all_of(metrics),
      list(total = ~sum(.x, na.rm = TRUE), avg = ~mean(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ), .groups = "drop")
}

# 3. Main processing function with NA padding
process_all_garmin_sleep <- function(input_dir, date_column, metrics, output_file) {
  files <- list.files(input_dir, pattern = "_garminSleep.*\\.csv$", full.names = TRUE)
  
  # Extract participant IDs from filenames
  ids <- str_extract(basename(files), "^RBS\\d+")
  
  # Load and bind all data
  all_data_list <- lapply(files, load_and_preprocess_with_id, date_column = date_column)
  all_data <- bind_rows(all_data_list)
  
  # Get all possible ID × ActivityDate combinations
  all_ids <- unique(ids)
  all_dates <- sort(unique(as.Date(all_data$ActivityDate)))
  full_grid <- expand.grid(ID = all_ids, ActivityDate = all_dates)
  
  # Summarize real data
  summarized_data <- summarize_by_id_and_date(all_data, metrics)
  
  # Pad missing dates/IDs with NA
  padded_data <- full_grid %>%
    left_join(summarized_data, by = c("ID", "ActivityDate")) %>%
    arrange(ID, ActivityDate)
  
  # Write to CSV
  write.csv(padded_data, file.path(input_dir, output_file), row.names = FALSE)
}

# 4. Run
process_all_garmin_sleep(
  input_dir = "D://Evan//Lab Transfer//evand//Garmin Sleep Epochs",
  date_column = "ActivityDateTime",
  metrics = c("DeepSleepDurationInSeconds", "LightSleepDurationInSeconds", "AwakeDurationInSeconds", "RemSleepInSeconds"),
  output_file = "combined_garminSleep_summarized_by_ID.csv"
)
