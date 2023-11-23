#' @title get_anticipated_enrollment
#' 
#' @description  This function retrieves the estimated enrollment of a clinical trial 
#' 
#' 
#' @param nctid Trial registration number for ClinicalTrials.gov trial. Can be a single value or a vector.
#'
#' @return anticipated enrollment 
#'     
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#'
#' get_anticipated_enrollment("NCT00480077")
#' 
#' 

get_anticipated_enrollment <- function(nctid) {
  # Ensure nctid is character or a vector of characters
  assertthat::assert_that(
    is.character(nctid) | (is.vector(nctid) && is.character(nctid)),
    all(grepl("^NCT\\d{8}$", nctid))
  )
  
  # Convert single nctid to vector for consistency
  nctid <- as.character(nctid)
  
  # Retrieve historical version of nct_ids
  raw_trials <- cthist::clinicaltrials_gov_download(nctid)
  
  # Generate new variable of anticipated enrollment
  anticipated_enrollment <- raw_trials |> 
    dplyr::group_by(nctid) |> 
    dplyr::mutate(anticipated_enrollment = ifelse(enrolment_type == "ESTIMATED", as.integer(enrolment), NA_integer_)) |>
    dplyr::ungroup()
  
  # Group by nct_id and mutate anticipated_enrollment
  anticipated_enrollment <- anticipated_enrollment |>
  dplyr::group_by(nctid) |>
    dplyr::mutate(
      anticipated_enrollment =  dplyr::first(na.omit(anticipated_enrollment))
    )
  
  # Return actual_enrollment 
  return(anticipated_enrollment)
}
