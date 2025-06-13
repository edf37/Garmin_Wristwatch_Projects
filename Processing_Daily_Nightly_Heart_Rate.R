# Process Raw Heart Rate Data into Daily and Nightly Averages for Concatenation into Activity and Sleep Files
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

# 1. Load and preprocess with ID extraction
load_and_preprocess_with_id <- function(filepath, date_column, tz = "UTC") {
  data <- read.csv(filepath)
  
  # Skip files that are empty or missing the datetime column
  if (nrow(data) == 0 || !(date_column %in% names(data))) return(NULL)
  
  # Extract ID from filename
  id <- str_extract(basename(filepath), "^RBS\\d+")
  data$ID <- id
  
  # Parse datetime
  data[[date_column]] <- parse_date_time(
    data[[date_column]],
    orders = c("mdY HMS p", "mdY IMS p", "ymd HMS", "mdY HM"),
    tz = tz
  )
  
  # Skip if parsing fails
  if (all(is.na(data[[date_column]]))) return(NULL)
  
  data$DateTime <- data[[date_column]]
  data$Hour <- hour(data$DateTime)
  
  # Assign time period
  data$TimePeriod <- ifelse(data$Hour >= 5 & data$Hour < 22, "Day", "Night")
  
  # Adjust date for nighttime (midnight–4:59 AM)
  data$AdjustedDate <- as.Date(data$DateTime)
  data$AdjustedDate[data$TimePeriod == "Night" & data$Hour < 5] <- 
    data$AdjustedDate[data$TimePeriod == "Night" & data$Hour < 5] - 1
  
  return(data)
}

# 2. Summarize by ID, AdjustedDate, and TimePeriod, plus Total
summarize_by_id_date_period <- function(data) {
  daynight_summary <- data %>%
    group_by(ID, AdjustedDate, TimePeriod) %>%
    summarise(
      HeartRate_avg = mean(HeartRate, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = TimePeriod,
      values_from = HeartRate_avg,
      names_glue = "HeartRate_avg_{TimePeriod}"
    )
  
  total_summary <- data %>%
    group_by(ID, AdjustedDate) %>%
    summarise(
      HeartRate_avg_Total = mean(HeartRate, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Merge day/night and total summaries
  full_summary <- left_join(daynight_summary, total_summary, by = c("ID", "AdjustedDate")) %>%
    rename(ActivityDate = AdjustedDate)
  
  return(full_summary)
}

# 3. Main processing function
process_all_garmin_epoch <- function(input_dir, date_column, output_file) {
  files <- list.files(input_dir, pattern = "_garminHeartRate.*\\.csv$", full.names = TRUE)
  
  # Load and preprocess
  all_data_list <- lapply(files, load_and_preprocess_with_id, date_column = date_column)
  all_data_list <- Filter(Negate(is.null), all_data_list)  # remove NULLs
  all_data <- bind_rows(all_data_list)
  
  if (nrow(all_data) == 0) {
    stop("No data found in heart rate files.")
  }
  
  # Summarize by ID and date
  summarized_data <- summarize_by_id_date_period(all_data)
  
  # Write to CSV
  write.csv(summarized_data, file.path(input_dir, output_file), row.names = FALSE)
}

# 4. Run the pipeline (using "ActivityTime" as the datetime column)
process_all_garmin_epoch(
  input_dir = "D://Evan//Lab Transfer//evand//Garmin Heart Rate Epochs",
  date_column = "ActivityTime",
  output_file = "combined_garminHeartRate_avg_daynight_total_by_ID.csv"
)

