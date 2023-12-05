#' @title get_start_date
#' 
#' @description  This function retrieves the study start date of a clinical trial from ClinicalTrials.gov. The start date is defined as the study start date when the overall status of the trial was first changed to ‘Terminated’ from any other overall status in the ClinicalTrials.gov registry.
#'     
#' @param nctid Trial registration number for ClinicalTrials.gov trial. Can be a single value or a vector.
#'
#' @param historical_version Logical. If TRUE, skips downloading historical versions using cthist and directly uses the provided data.
#'
#' @param data_frame_name Character. The name of the data frame to use when historical_version is TRUE.
#'
#' @return Data frame containing the start date for the specified clinical trial(s).
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

get_start_date <- function(nctid, historical_version = FALSE, data_frame_name = NULL) {
  # Ensure nctid is character or a vector of characters
  assertthat::assert_that(
    is.character(nctid) | (is.vector(nctid) && is.character(nctid)),
    all(grepl("^NCT\\d{8}$", nctid))
  )
  
  # Convert single nctid to vector for consistency
  nctid <- as.character(nctid)
  
  if(!historical_version){
    # Retrieve historical version of nct_ids
    raw_trials <- cthist::clinicaltrials_gov_download(nctid)
  }else{
    # If historical version is TRUE, use the provided data
    if(!is.null(data_frame_name)){
      # If data_frame_name is provided, use it
      raw_trials <- data_frame_name
    }else{
      stop("When historical_version is TRUE, data_frame_name must be provided")
    }
  }
  

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
      start_date = na.omit(start_date)[1]) |>
      dplyr::distinct(nctid, start_date)
    
  
  # Return start_date dataframe
  return(start_date_df)
}
