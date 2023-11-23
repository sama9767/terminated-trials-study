#' @title has_summary_result
#' 
#' @description  This function checks whether a ClinicalTrials.gov trial has posted summary results. 
#'  
#'     
#' @param nctid Trial registration number for ClinicalTrials.gov trial. Can be a single value or a vector.
#'
#' @return logical vector TRUE if results were posted, otherwise FALSE
#'     
#'
#' @export
#' 
#'
#' @importFrom magrittr %>%
#'
#'
#' @examples
#'
#' has_summary_result("NCT00480077") ## TRUE
#'

has_summary_result <- function(nctid) {
  # Ensure nctid is character or a vector of characters
  assertthat::assert_that(
    is.character(nctid) | (is.vector(nctid) && is.character(nctid)),
    all(grepl("^NCT\\d{8}$", nctid))
  )
  
  # Convert single nctid to vector for consistency
  nctid <- as.character(nctid)
  
  # Retrieve historical version of nct_ids
  raw_trials <- cthist::clinicaltrials_gov_download(nctid)
  
  # Create a 'has_summary_result' variable
  has_summary_result_df <- raw_trials |> 
    dplyr::group_by(nctid) |> 
    dplyr::mutate(has_summary_result = ifelse(dplyr::last(results_posted) == "TRUE", TRUE, FALSE)) |> 
    dplyr::ungroup()
  
  # Group by nct_id and mutate has_summary_result
  has_summary_result_df <- has_summary_result_df |>
    dplyr::group_by(nctid) |>
    dplyr::mutate(
      has_summary_result = has_summary_result
    ) 
  
  # Return has_summary_result dataframe
  return(has_summary_result_df)
}
