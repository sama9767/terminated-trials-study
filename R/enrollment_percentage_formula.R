#' @title enrollment_percentage_formula
#'
#' @description
#'
#' This function calculates the percentage enrolled based on
#' 'anticipated_enrolment' and 'actual_enrolment' arguments
#'
#' @param actual_enrollment An integer value for the actual enrollment
#'
#' @param anticipated_enrollment An integer value for the anticipated
#'     enrollment
#'
#' @return The ratio of actual to anticipated enrollment, multiplied
#'     by 100
#'
#' @export
#'
#' @examples
#'
#' enrollment_percentage_formula(5, 500)

enrollment_percentage_formula <- function(actual_enrollment,
                                          anticipated_enrollment) {

    ## Ensure data integrity
    assertthat::assert_that(
                    assertthat::is.count(actual_enrollment),
                    msg="Actual enrollment is not an integer"
                )
    
    assertthat::assert_that(
                    assertthat::is.count(anticipated_enrollment),
                    msg="Anticipated enrollment is not an integer"
                )
    
    ## Return result
    return(100 * actual_enrollment / anticipated_enrollment)
}
