test_that("outlier function selection", {
    expect_no_error(outlier_functions("IQR"))
    expect_no_error(outlier_functions("5/95 percentiles"))
    expect_no_error(outlier_functions("No"))
})

test_that("apply outlier function", {
    of <- outlier_functions("IQR")
    expect_no_error(of(measures_data[,"var_a"]))
})

test_that("apply outlier function", {
    of <- outlier_functions("5/95 percentiles")
    expect_no_error(of(measures_data[,"var_a"]))
})


test_that("apply outlier function", {
    expect_error(outlier_functions("GG"))
})

test_that("replace false with na working example", {
    of <- outlier_functions("IQR")
    expect_no_error(replaceFalseWithNA(measures_data$var_a, of))
    expect_no_error(replaceFalseWithNA(measures_data[, "var_a", drop = FALSE], of))
})
