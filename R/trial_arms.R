#' @title how_many_trial_arms
#'
#' @description Retrieves the number of trial arms for a trial
#'     registry entry on ClinicalTrials.gov
#'
#' @param nctid A string containing a well-formed NCT number for a
#'     trial on ClinicalTrials.gov
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#'
#' how_many_trial_arms("NCT02715531")
#' ## Should return 5
#'
#' how_many_trial_arms("NCT03430843")
#' ## Should return 2
#' 
#' how_many_trial_arms("NCT03430844")
#' ## Should throw an error (NCT does not exist)
#'
#' how_many_trial_arms("Lol bad NCT")
#' ## Should throw an error (NCT not well-formed)

how_many_trial_arms <- function (nctid) {

    ## Throw an error if the NCT Number is not well-formed
    assertthat::assert_that(
                    grepl("^NCT[0-9]{8}$", nctid),
                    msg = "NCT Number is not well-formed"
                )

    ## API base URL + NCT number
    base_url <- paste0(
        "https://clinicaltrials.gov/api/v2/studies/",
        nctid
    )

    ## Get data about trial from API
    response <- httr::GET(base_url) %>%
        httr::content()

    ## Throw an error if the NCT doesn't exist
    assertthat::assert_that(
                    paste(response, collapse=" ") != paste0(
                                    "NCT number ",
                                    nctid,
                                    " not found"
                                ),
                    msg="NCT number not found"
                )

    response$protocolSection$armsInterventionsModule$armGroups %>%
        length() %>%
        return()
    
}
