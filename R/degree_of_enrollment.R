#' @title degree_of_enrollment
#'
#' @description
#'
#' This function checks the enrollment status in a dataframe by
#' calculating the enrollment percentage based on the values in the
#' 'anticipated_enrollment' and 'actual_enrollment' columns.  It also computes
#' trial and dataset summaries and additional statistics related to the
#' anticipated and actual enrollments.
#'
#'
#' @param df The dataframe containing enrollment data.
#'
#' @param anticipated_column The column name for the anticipated
#'     enrollment values in the dataframe.
#'
#' @param actual_column The column name for the actual enrollment
#'     values in the dataframe.
#'
#'@param trial_column The column for grouping the data by trial (trial id)
#'
#' @param round_off (Optional) The number of decimal places to round
#'     the percentage values. Defaults to 2. Use NA to avoid rounding.
#'
#' @return A list containing:
#' updated_dataframe: The input dataframe with an added column enrollment_percentage, 
#' showing the calculated enrollment percentage for each row.
#' trial_summary: A data frame summarizing enrollment data by trial level (total anticipated, 
#' total actual, and trial-level enrollment percentages).
#' dataset_summary: A data frame summarizing dataset-wide enrollment statistics (median, minimum, maximum)
#' summary_anticipated_enrollment: A summary of anticipated enrollment statistics (median, IQR, and quantiles).
#' summary_actual_enrollment: A summary of actual enrollment statistics (median, IQR, and quantiles).
#' overall_degree_of_enrollment: A summary of the overall enrollment percentage 
#'
#'@examples
#' result <- degree_of_enrollment(df, "anticipated_enrollment", actual_enrollment", "trial_id")
#' print(result$trial_summary)

degree_of_enrollment <- function(df, anticipated_column, actual_column, trial_column, round_off = 2) {
  
  ## Some testing to ensure data integrity
  assertthat::assert_that(is.data.frame(df), msg = "Argument provided (df) is not a valid data frame")
  assertthat::assert_that(assertthat::has_name(df, anticipated_column), msg = "Anticipated enrollment column not found")
  assertthat::assert_that(assertthat::has_name(df, actual_column), msg = "Actual enrollment column not found")
  assertthat::assert_that(assertthat::is.count(round_off) || is.na(round_off), msg = "Rounding argument is not an integer")
  assertthat::assert_that(!assertthat::has_name(df, "enrollment_percentage"), msg = "The enrollment_percentage column already exists")
  
  ## Create the 'percentage' column and initialize with NA
  df$enrollment_percentage <- NA
  
  for (i in 1:nrow(df)) {
    if (is.na(df[[anticipated_column]][i]) || is.na(df[[actual_column]][i])) {
      df$enrollment_percentage[i] <- "Check missing values"
    } else {
      if (!is.na(round_off)) {
        df$enrollment_percentage[i] <- round(
          enrollment_percentage_formula(df[[actual_column]][i], df[[anticipated_column]][i]),
          round_off
        )
      } else {
        df$enrollment_percentage[i] <- enrollment_percentage_formula(df[[actual_column]][i], df[[anticipated_column]][i])
      }
    }
  }
  
  ## Flag missing values
  df$missing_flag <- ifelse(is.na(df[[anticipated_column]]) | is.na(df[[actual_column]]), "Missing Data", "Complete")
  
  ## Trial-Level Summary
  trial_summary <- df |>
    dplyr::group_by(.data[[trial_column]]) |>
    dplyr::summarize(
      anticipated_total = sum(.data[[anticipated_column]], na.rm = TRUE),
      actual_total = sum(.data[[actual_column]], na.rm = TRUE),
      trial_enrollment_percentage = enrollment_percentage_formula(actual_total,anticipated_total)
    ) 
  
  ## Dataset-Level Summary
  dataset_summary <- data.frame(
    median_enrollment = median(df$enrollment_percentage, na.rm = TRUE),
    min_enrollment = min(df$enrollment_percentage, na.rm = TRUE),
    max_enrollment = max(df$enrollment_percentage, na.rm = TRUE)
  )
  
  ## Additional Summaries
  summary_anticipated_enrollment <- df |>
    summarise(
      median_anticipated_enrollment = median(.data[[anticipated_column]], na.rm = TRUE),
      IQR_anticipated_enrollment = IQR(.data[[anticipated_column]], na.rm = TRUE),
      Q1_anticipated_enrollment = quantile(.data[[anticipated_column]], 0.25, na.rm = TRUE),
      Q3_anticipated_enrollment = quantile(.data[[anticipated_column]], 0.75, na.rm = TRUE),
      max_anticipated_enrollment = max(.data[[anticipated_column]], na.rm = TRUE),
      .groups = 'drop'
    )
  
  summary_actual_enrollment <- df |>
    summarise(
      median_actual_enrollment = median(.data[[actual_column]], na.rm = TRUE),
      IQR_actual_enrollment = IQR(.data[[actual_column]], na.rm = TRUE),
      Q1_actual_enrollment = quantile(.data[[actual_column]], 0.25, na.rm = TRUE),
      Q3_actual_enrollment = quantile(.data[[actual_column]], 0.75, na.rm = TRUE),
      max_actual_enrollment = max(.data[[actual_column]], na.rm = TRUE),
      .groups = 'drop'
    )
  
  overall_degree_of_enrollment <- df |>
    summarise(
      total_actual_enrollment = sum(.data[[actual_column]], na.rm = TRUE),
      total_anticipated_enrollment = sum(.data[[anticipated_column]], na.rm = TRUE),
      overall_percentage = enrollment_percentage_formula(total_actual_enrollment, total_anticipated_enrollment),
      .groups = 'drop'
    )
  
  ## Return as a list
  return(
    list(
      updated_dataframe = df,
      trial_summary = trial_summary,
      dataset_summary = dataset_summary,
      summary_anticipated_enrollment = summary_anticipated_enrollment,
      summary_actual_enrollment = summary_actual_enrollment,
      overall_degree_of_enrollment = overall_degree_of_enrollment
    )
  )
}
