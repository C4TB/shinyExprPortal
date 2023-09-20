test_that("getSelectedSampleCategories working example", {
     sample_c <- list(list(name = "class", label = "Class", values = c("a", "b")),
                   list(name = "class_2", label = "Class 2", values = c("c", "d")))
    input_test <- list("class" = "a", "class_2" = "c")
  expect_no_error(getSelectedSampleCategories(sample_c, input_test))
})


test_that("getSelectedSampleCategories not working", {
    sample_c <- list(list(name = "class", label = "Class", values = c("a", "b")),
                     list(name = "class_2", label = "Class 2", values = c("c", "d")))
    input_test <- list("class" = "a")
    expect_error(getSelectedSampleCategories(sample_c, input_test))
})

test_that("getSelectedSampleCategories working with subset", {
    sample_c <- list(list(name = "class", label = "Class", values = c("a", "b")),
                     list(name = "class_2", label = "Class 2", values = c("c", "d")))
    input_test <- list("class" = "a")
    expect_no_error(getSelectedSampleCategories(sample_c, input_test, c("class")))
})


