#' @title has_summary_result
#' 
#' @description  This function checks whether a ClinicalTrials.gov trial has posted summary results. It retrieves the final version of the trial data, considering the historical version if specified.
#'  
#'     
#' @param nctid Trial registration number for ClinicalTrials.gov trial. Can be a single value or a vector.
#'
#' @param historical_version Logical. If TRUE, skips downloading historical versions using cthist and directly uses the provided data.
#'
#' @param data_frame_name Character. The name of the data frame to use when historical_version is TRUE.
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

has_summary_result <- function(nctid, historical_version = FASLE,data_frame_name = NULL) {
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
    
  
  # Create a 'has_summary_result' variable
  has_summary_result_df <- raw_trials |> 
    dplyr::group_by(nctid) |> 
    dplyr::mutate(has_summary_result = ifelse(dplyr::last(results_posted) == "TRUE", TRUE, FALSE)) |> 
    dplyr::ungroup()
  
    
  
  # Group by nct_id and mutate has_summary_result
  has_summary_result_df <- has_summary_result_df |>
    dplyr::group_by(nctid) |>
    dplyr::mutate(
    has_summary_result = has_summary_result) |>
    dplyr::distinct(nctid, has_summary_result)
     
  
  # Return has_summary_result dataframe
  return(has_summary_result_df)
}
