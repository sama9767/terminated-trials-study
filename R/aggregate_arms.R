#' @title aggregate_arms
#'
#' @description This function aggregates the serious adverse event
#'     (SAE) data by trial and arm, calculating the total number of
#'     participants affected and the total number at risk for each
#'     group within a clinical trial.
#' 
#' @param df A data frame containing clinical trial information,
#'     including trial ID, arm assignment, number affected by serious
#'     adverse events, and number at risk.
#'
#' @return A tibble summarizing the total number of participants
#'     affected by serious adverse events and the total number at
#'     risk, grouped by trial ID and arm assignment.
#' @export
#'
#' @importFrom assertthat assert_that
#'
#'
#' @examples
#'
#' aggregate_arms(trial_data, "nctid", "arm_group",
#'     "serious_affected", "serious_at_risk")

aggregate_arms <- function(df, trial_id, arm_assigned, affected_col, risk_col) {
  
  # Assert that the provided column names exist in the dataframe
  assert_that(all(c(trial_id, arm_assigned, affected_col, risk_col) %in% colnames(df)), 
              msg = "One or more of the provided column names do not exist in the dataframe.")
  
  # Assert that the columns for trial and arm type are character or factor
  assert_that(is.character(df[[trial_id]]) || is.factor(df[[trial_id]]),
              msg = paste(trial_id, "should be of type character or factor."))
  assert_that(is.character(df[[arm_assigned]]) || is.factor(df[[arm_assigned]]),
              msg = paste(arm_assigned, "should be of type character or factor."))
  
  # Convert columns to numeric if they are not already
  df <- df|>
    mutate(across(all_of(affected_col), ~ as.numeric(as.character(.)), .names = "numeric_{.col}"),
           across(all_of(risk_col), ~ as.numeric(as.character(.)), .names = "numeric_{.col}"))
  
  # Assert that the converted columns are numeric
  assert_that(is.numeric(df[[paste0("numeric_", affected_col)]]),
              msg = paste(affected_col, "could not be converted to numeric."))
  assert_that(is.numeric(df[[paste0("numeric_", risk_col)]]),
              msg = paste(risk_col, "could not be converted to numeric."))
  
  # Group by trial and arm type, and summarize the serious numbers
  df_summarized <- df|>
    group_by(across(all_of(trial_id)), across(all_of(arm_assigned)))|>
    summarise(
      total_seriousnumaffected = sum(.data[[paste0("numeric_", affected_col)]], na.rm = TRUE),
      total_seriousnumatrisk = sum(.data[[paste0("numeric_", risk_col)]], na.rm = TRUE),
      .groups = 'drop'
    )
  
  return(df_summarized)
}
