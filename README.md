# GarminProjects

# Wrist-worn device data - including epoch-by-epoch heart rate (12 Hz), energy expenditure, sleep, and activities (i.e., step, distance) - may be loaded, pre-processed, processed, and cleaned for subsequent analyses using this hub.
# Written on 11/26/2024

## PURPOSE

# This hub describes the steps taken from exported epoch-by-epoch data from the Garmin wrist-watch (Instinct Solar) recorded by either 
# the right or left wrist to process by-parameter data for the user's choice of analyses thereafter. This hub holds scripts written in R language 
# RStudio (R Version 4.4.1 [2024-06-14 ucrt]) for the user to process their device data. All data have been removed 
# and naming conventions for columns have been standardized to Garmin devices yet remain flexible for user preferences.

## INPUTS 

# Data 
## Individual by-parameter files exported by Fitabase, a third-party data merging and processing platform for export.
## Individual (by-participant) parameter variables into local or cloud drive to support looping functions. 
## Epoch Activity files comprise physical activity data expressed in summed totals of step count, distance, and energy expenditure and its derivatives over the monitoring period in 15 min intervals. 
## Epoch Heart Rate files comprise epoch-by-epoch heart rate data captured by the device every 15 seconds over the duration of the study period detected if device is worn.
## Variable definitions can be found in the variable repository of Fitabase here: https://www.fitabase.com/resources/knowledge-base/exporting-data/data-dictionaries/



