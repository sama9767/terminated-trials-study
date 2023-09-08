#' This function checks the enrolment status in a dataframe by
#' calculating the enrolment percentage based on the values in the
#' 'anticipated_enrolment' and 'actual_enrolment' columns.  It handles
#' missing values and provides an option to round the percentage
#' values.
#'
#' The function iterates through each row of the dataframe and checks
#' if there are missing values in the 'anticipated_enrollment' and
#' 'actual_enrollment' columns. If any of the values is missing, it
#' assigns "Check missing values" to the corresponding row in the
#' 'percentage' column. Otherwise, it calculates the enrollment
#' percentage using the formula: actual_enrollment /
#' anticipated_enrollment * 100.  The percentage values can be
#' optionally rounded to a specified number of decimal places.
#'
#' Usage: degree_of_enrollment(df, "anticipated_column",
#' "actual_column", round_off = 2)
#'
#' @param df The dataframe containing enrolment data.
#'
#' @param anticipated_column The column name for the anticipated
#'     enrollment values in the dataframe.
#'
#' @param actual_column The column name for the actual enrollment
#'     values in the dataframe.
#'
#' @param round_off (Optional) The number of decimal places to round
#'     the percentage values. Defaults to 2. Use NA to avoid rounding.
#'
#' @return The updated dataframe with the 'enrollment_percentage'
#'     column populated.
#'
#' @export
#' 
#' @examples
#' 
#'   # Assuming you have a dataframe named 'df' with
#'   # 'anticipated_enrollment' and 'actual_enrollment' columns:
#'
#'   # Round the percentage values to 1 decimal place
#'   updated_df <- duration_of_enrollment(
#'     df,
#'     "anticipated_enrollment",
#'     "actual_enrollment",
#'     round_off = 1
#'   )
#'
#'   # Do not round the percentage values
#'   updated_df <- duration_of_enrollment(
#'     df,
#'     "anticipated_enrollment",
#'     "actual_enrollment",
#'     round_off = NA
#'   )

degree_of_enrollment <- function(df, anticipated_column,
                                 actual_column, round_off = 2) {

    ## Some testing to ensure data integrity
    
    test_that(
        "The function has been passed a data frame",
        {
            expect_equal(
                is.data.frame(df),
                TRUE
            )
        }
    )

    test_that(
        "The anticipated column exists",
        {
            expect_equal(
                anticipated_column %in% colnames(df),
                TRUE
            )
        }
    )
    
    test_that(
        "The actual column exists",
        {
            expect_equal(
                actual_column %in% colnames(df),
                TRUE
            )
        }
    )

    test_that(
        "The rounding number is an integer or NA",
        {
            expect_equal(
                is.na(round_off) || round_off == round(round_off),
                TRUE
            )
        }
    )
    
    test_that(
        "The column to be created doesn't exist",
        {
            expect_equal(
                "enrollment_percentage" %in% colnames(df),
                FALSE
            )
        }
    )
    
    # Create the 'percentage' column and initialize with NA
    df$enrollment_percentage <- NA
  
    for (i in 1:nrow(df)) {
        if (is.na(df[[anticipated_column]][i])
            || is.na(df[[actual_column]][i])) {
            df$enrollment_percentage[i] <- "Check missing values"
        } else {
            if (! is.na(round_off)) {
                df$enrollment_percentage[i] <- round(
                    enrollment_percentage_formula(
                        df[[actual_column]][i],
                        df[[anticipated_column]][i]
                    ), round_off
                )
            } else {
                df$enrollment_percentage[i] <-
                    enrollment_percentage_formula(
                        df[[actual_column]][i],
                        df[[anticipated_column]][i]
                    )
            }
        }
    }
  
    return(df)
}
