#' @title get_why_stopped
#' 
#' @description  This function retrieves the reason for trial termination, 
#' if given, for a specified clinical trial. 
#'
#'     
#' @param nctid Trial registration number for ClinicalTrials.gov trial
#'
#' @return Reason for termination of the clinical trial, or `no result posted` 
#' if the reason is not available.
#'     
#'
#' @export
#'
#' @importFrom magrittr %>%
#' 
#'
#' @examples 
#' get_why_stopped("NCT00480077") ## Low enrollment rates
#'

get_why_stopped <- function(nctid, historical_version = FALSE, data_frame_name = NULL) {
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
  # If If historical version is TRUE, use the provided data
    if(!is.null(data_frame_name)){
    # If data_frame_name is provided, use it  
    raw_trials <- data_frame_name
  }else{
    stop("When historical_version is TRUE, data_frame_name must be provided")
  }
  }
  
  # Create reason for termination variable (refers to the reason of termination for a clinical trial)
  get_why_stopped_df <- raw_trials |> 
    dplyr::group_by(nctid) |> 
    dplyr::mutate(reason_for_termination = ifelse(any(nzchar(whystopped)), dplyr::last(na.omit(whystopped)), NA_character_)) |> 
    dplyr::ungroup() 
  
  
  # Group by nct_id and mutate reason for trial termination
  get_why_stopped_df <- get_why_stopped_df |>
   dplyr::group_by(nctid) |>
   dplyr::mutate(
   reason_for_termination =  ifelse(is.na(reason_for_termination), "no reason posted", reason_for_termination)
    ) |>
    dplyr::distinct(nctid, reason_for_termination)
  
  # Return termination reason dataframe
  return(get_why_stopped_df)
}
