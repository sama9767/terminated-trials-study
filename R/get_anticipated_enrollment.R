#' @title get_anticipated_enrollment
#'
#' @description This function retrieves the estimated enrollment of a clinical trial at time of trial launch
#'
#'
#' @param nctid Trial registration number for ClinicalTrials.gov trial. Can be a single value or a vector.
#'
#' @return anticipated enrollment.
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

get_anticipated_enrollment <- function(nctid, historical_version = FALSE, data_frame_name = NULL) {
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
  
  # Generate new variable of anticipated enrollment
  anticipated_enrollment <- raw_trials |>
    dplyr::group_by(nctid) |>
    dplyr::mutate(anticipated_enrollment = ifelse(enrolment_type == "ESTIMATED", as.integer(enrolment), NA_integer_)) |>
    dplyr::ungroup()
  
  # Group by nct_id and mutate anticipated_enrollment
  anticipated_enrollment <- anticipated_enrollment |>
    dplyr::group_by(nctid) |>
    dplyr::mutate(anticipated_enrollment = dplyr::first(na.omit(anticipated_enrollment)))
  
  # Select the distinct row of nctid and its anticipated enrollment
  distinct_anticipated_enrollment <- anticipated_enrollment |>
    dplyr::distinct(nctid, anticipated_enrollment)
  
  # Return the distinct anticipated enrollment
  return(distinct_anticipated_enrollment)
}
