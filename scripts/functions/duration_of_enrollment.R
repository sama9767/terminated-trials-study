#' duration_of_enrollment
# duration_of_enrollment Function Description:
#
# This function calculates the duration of enrollment in days for each row of a data frame, based on the provided start and stop date columns.
# It handles different date formats and provides a warning if the format of the dates has changed, prompting a rewrite of the function.
#
# Parameters:
#   - df: The data frame containing the enrollment data.
#   - start_date_col: The column name or index of the start date in the data frame.
#   - stop_date_col: The column name or index of the stop date in the data frame.
#
# Returns:
#   - The input data frame with an additional column named 'patient_days' representing the duration of enrollment in days.
#   - If missing values are encountered in either the start or stop date columns, the 'patient_days' value for that row will be set to "Check missing values".
#
# Notes:
#   - This function requires the 'lubridate' package.
#   - It assumes that the date formats are either day-month-year (dmy), month-day-year (mdy), or year-month-day (ymd).
#   - If the date format has changed and the parsing is unsuccessful, a warning is issued, and the 'patient_days' value for that row is set to NA.
#
# Usage example:
#   df <- duration_of_enrollment(df, "start_date", "stop_date")
#


library(lubridate)

duration_of_enrollment <- function(df, start_date_col, stop_date_col) {
  df$patient_days <- NA
  
  for (i in 1:nrow(df)) {
    start_date <- df[[start_date_col]][i]
    stop_date <- df[[stop_date_col]][i]
    
    if (is.na(start_date) || is.na(stop_date)) {
      df$patient_days[i] <- "Check missing values"
    } else {
      # Check the format of start_date and stop_date
      if (!inherits(start_date, "Date")) {
        start_date <- parse_date_time(start_date, orders = c("dmy", "mdy", "ymd"))
        if (is.na(start_date)) {
          warning("Start date format changed")
          df$patient_days[i] <- NA
          next
        }
      }
      if (!inherits(stop_date, "Date")) {
        stop_date <- parse_date_time(stop_date, orders = c("dmy", "mdy", "ymd"))
        if (is.na(stop_date)) {
          warning("Stop date format changed")
          df$patient_days[i] <- NA
          next
        }
      }
      
      df$patient_days[i] <- as.integer(stop_date - start_date)
    }
  }
  
  return(df)
}
