#' @title get_actual_enrollment
#' 
#' @description This function retrieves the final recorded enrollment of a clinical trial 
#' as reported on ClinicalTrials.gov. 
#'     
#' @param nctid Trial registration number for ClinicalTrials.gov trial. Can be a single value or a vector.
#'
#' @return actual enrollment 
#'     
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#'
#' get_actual_enrollment("NCT00480077")
#' 
#' 

get_actual_enrollment <- function(nctid, historical_version = FALSE, data_frame_name = NULL) {
  # Ensure nctid is character or a vector of characters
  assertthat::assert_that(
    is.character(nctid) | (is.vector(nctid) && is.character(nctid)),
    all(grepl("^NCT\\d{8}$", nctid))
  )
  
  # Convert single nctid to vector for consistency
  nctid <- as.character(nctid)
  
  if (!historical_version) {
    # Retrieve historical version of nct_ids
    raw_trials <- cthist::clinicaltrials_gov_download(nctid)
  } else {
    # If historical version is TRUE, use provided data
    if (!is.null(data_frame_name)) {
      # If data_frame_name is provided use it
      raw_trials <- data_frame_name
    } else {
      stop("when historical_version is TRUE, data_frame_name must be provided")
    }
  }
  
  
  # Generate new variable of actual enrollment
  actual_enrollment <- raw_trials |> 
    dplyr::group_by(nctid) |> 
    dplyr::mutate(actual_enrollment = ifelse(enrolment_type == "ACTUAL", as.integer(enrolment), NA_integer_)) |>
                  dplyr::ungroup()
  
  # Group by nct_id and mutate actual_enrollment
  actual_enrollment <- actual_enrollment |>
   dplyr::group_by(nctid) |>
    dplyr::mutate(actual_enrollment = dplyr::last(na.omit(actual_enrollment))
    )   
  
  # Select the distinct row of nctid and its anticipated enrollment
  distinct_actual_enrollment <- actual_enrollment |>
    dplyr::distinct(nctid, actual_enrollment)
  
  # Return actual_enrollment 
  return(distinct_actual_enrollment)
}
