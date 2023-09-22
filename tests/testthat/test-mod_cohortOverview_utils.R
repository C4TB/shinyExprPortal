test_that("cohortOverview works", {
    test_measures <- data.frame(
        col1 = c(1,2,3,4),
        col2 = c(2,3,4,5),
        col3 = c(3,4,5,6),
        color = c(2,2,3,3),
        estimate = c(1,1,1,1)
    )
    expect_no_error(add_cohort_overview(test_measures,
                        "test",
                        "color",
                        "numeric",
                        c("col1", "col2", "col3")))
})
