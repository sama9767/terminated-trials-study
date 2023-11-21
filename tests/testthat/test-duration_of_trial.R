test_that(
    "duration_of_trial works",
    {
        df <- tibble::tribble(
                          ~actual, ~anticipated,
                          20,      21,
                          10,      NA
                      )
        result <- degree_of_enrollment(df, "actual", "anticipated")

        expect_equal(
            nrow(result),
            2
        )
    }
)
