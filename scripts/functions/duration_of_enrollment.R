#' duration_of_enrollment
#'
#' Description:
#'   This function calculates the duration of enrollment in days for each row in a dataframe.
#'   It takes the start date and stop date columns, handles missing or NA values,
#'   and assigns the calculated duration to the 'partient_days' column.
#'
#' Usage:
#'   duration_of_enrollment(df, start_date_col, stop_date_col)
#'
#' Arguments:
#'   df: The dataframe containing the enrollment data.
#'   start_date_col: The name of the column representing the start date of enrollment.
#'   stop_date_col: The name of the column representing the stop date of enrollment.
#'
#' Details:
#'   The function iterates through each row of the dataframe and checks if either the start date or stop date is missing or NA.
#'   If any of the dates is missing, it assigns "ERROR" to the corresponding row in the 'participant_days' column.
#'   Otherwise, it converts the start date and stop date to the Date type using the 'dmy()' function from the lubridate package.
#'   The difference between the stop date and start date is then calculated and stored as the duration of enrollment in days.
#'
#' Value:
#'   Returns the updated dataframe with the 'patient_days' column populated.
#'
#' Examples:
#'   Assuming you have a dataframe named 'df' with 'start_date' and 'stop_date' columns:
#'
#'   # Calculate the duration of enrollment in days
#'   updated_df <- duration_of_enrollment(df, "start_date", "stop_date")
#'
#'   Ensure that you provide the correct column names that correspond to the start date and stop date columns in your dataframe.
#'   Note: If any rows have missing or NA values in either the start date or stop date column, the 'partient_days' value for those rows will be assigned as "ERROR".
library(lubridate)

duration_of_enrollment <- function(df, start_date_col, stop_date_col) {
  df$patient_days <- NA
  
  for (i in 1:nrow(df)) {
    start_date <- df[[start_date_col]][i]
    stop_date <- df[[stop_date_col]][i]
    
    if (is.na(start_date) || is.na(stop_date)) {
      df$patient_days[i] <- "Check missing values"
    } else {
      start_date <- dmy(start_date)
      stop_date <- dmy(stop_date)
      df$patient_days[i] <- as.integer(stop_date - start_date)
    }
  }
  
  return(df)
}
