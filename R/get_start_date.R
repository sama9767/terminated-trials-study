#' @title get_start_date
#' 
#' @description  This function creates the start date of a clinical trial, which is defined 
#' as the study start date when the overall status of the trial was first changed to ‘Terminated’ 
#' from any other overall status in the ClinicalTrials.gov registry.
#'     
#' @param nctid Trial registration number for ClinicalTrials.gov trial. Can be a single value or a vector.
#'
#' @return start date
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
#' get_start_date("NCT00480077")
#' 

get_start_date <- function(nctid) {
  # Ensure nctid is character or a vector of characters
  assertthat::assert_that(
    is.character(nctid) | (is.vector(nctid) && is.character(nctid)),
    all(grepl("^NCT\\d{8}$", nctid))
  )
  
  # Convert single nctid to vector for consistency
  nctid <- as.character(nctid)
  
  # Retrieve historical version of nct_ids
  raw_trials <- cthist::clinicaltrials_gov_download(nctid)
  
  # Create a 'start_date' variable
  start_date_df <- 
    raw_trials |>
   dplyr::group_by(nctid) |>
   dplyr::mutate(start_date = dplyr::if_else(overall_status == "TERMINATED", as.character(study_start_date), NA_character_)) |>
   dplyr::ungroup()
  
  # Group by nct_id and mutate start_date
  start_date_df <- start_date_df |>
 dplyr::group_by(nctid) |>
    dplyr::mutate(
      start_date = na.omit(start_date)[1]
    ) 
  
  # Return start_date dataframe
  return(start_date_df)
}
