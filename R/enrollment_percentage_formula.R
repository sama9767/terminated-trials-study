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
    test_that(
        "Actual enrollment is an integer",
        {
            expect_equal(
                actual_enrollment,
                round(actual_enrollment)
            )
        }
    )
    
    test_that(
        "Anticipated enrollment is an integer",
        {
            expect_equal(
                anticipated_enrollment,
                round(anticipated_enrollment)
            )
        }
    )

    ## Return result
    return(100 * actual_enrollment / anticipated_enrollment)
}
