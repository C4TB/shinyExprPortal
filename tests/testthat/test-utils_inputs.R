test_that("test advanced settings inputs", {
    config <- list(measures_outliers = TRUE,
                   expression_outliers = TRUE,
                   correlation_method = TRUE,
                   fit_method = TRUE)
  expect_no_error(advanced_settings_inputs(config))
})

test_that("no inputs", {
    config <- NULL
    expect_no_error(advanced_settings_inputs(config))
})

test_that("outlier inputs", {
    expect_no_error(outlier_inputs("outlier_radiotbuttons"))
})

test_that("gene select input", {
    expect_no_error(geneSelectInput(c("a" ,"b")))
})

test_that("vars select input", {
    expect_no_error(varsSelectInput(c("a" ,"b")))
})

test_that("sample categories working example", {
    config <- list(list(name = "class", label = "Class", values = c("a", "b")),
                    list(name = "class_2", label = "Class 2", values = c("c", "d")))
    expect_no_error(sampleCategoryInputs(config))
})

test_that("sample categories with subset working example", {
    config <- list(list(name = "class", label = "Class", values = c("a", "b")),
                   list(name = "class_2", label = "Class 2", values = c("c", "d")))
    expect_no_error(sampleCategoryInputs(config, subset_categories =  "class_2"))
})

test_that("sample categories should be named lists of vectors", {
    expect_error(sampleCategoryInputs(c("var_a", "var_b")))
})