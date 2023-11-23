#' @title get_stop_date
#' 
#' @description  This function creates stop_date of a clinical trial, which is defined 
#' as the actual primary completion date reported on the clinical trial record when 
#' its overall status was first changed to ‘Terminated’ from any other overall status 
#' in the ClinicalTrials.gov registry.
#'
#'     
#' @param nctid Trial registration number for ClinicalTrials.gov trial. Can be a single value or a vector.
#'
#' @return stop date 
#'     
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
#' @examples
#'
#' get_stop_date("NCT00480077")
#'

get_stop_date <- function(nctid) {
  # Ensure nctid is character or a vector of characters
  assertthat::assert_that(
    is.character(nctid) | (is.vector(nctid) && is.character(nctid)),
    all(grepl("^NCT\\d{8}$", nctid))
  )
  
  # Convert single nctid to vector for consistency
  nctid <- as.character(nctid)
  
  # Retrieve historical version of nct_ids
  raw_trials <- cthist::clinicaltrials_gov_download(nctid)
  
  # Create a 'stop_date' variable
  stop_date_df <- 
    raw_trials |>
    dplyr::group_by(nctid) |>
    dplyr::mutate(stop_date = dplyr::if_else(overall_status == "TERMINATED", as.character(primary_completion_date), NA_character_)) |>
    dplyr::ungroup()
  
  # Group by nct_id and mutate stop_date
  stop_date_df <- stop_date_df |>
   dplyr::group_by(nctid) |>
    dplyr::mutate(
      stop_date = na.omit(stop_date)[1]
    ) 
  
  # Return stop_date dataframe
  return(stop_date_df)
}
