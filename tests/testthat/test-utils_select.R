#selectMatchingValues test

test_that("basic working example", {
    vlist <- list(col_a = "treatment")
    expect_no_error(selectMatchingValues(lookup, vlist, "subject_id"))
})

test_that("basic working example 2", {
    vlist <- list(col_a = "treatment")
    expect_no_error(selectMatchingValues(lookup, vlist))
})

test_that("values_list should be named", {
    vlist <- c("treatment","wrong")
    expect_error(selectMatchingValues(lookup, vlist))
})

test_that("only work if all keys in values_list exist in lookup", {
    vlist <- list(col_a = "treatment", col_d = "wrong")
    expect_error(selectMatchingValues(lookup, vlist))
})

test_that("test if the asterisk value in lookup works", {
    vlist <- list(col_c = "good")
    expect_equal(4, nrow(selectMatchingValues(lookup, vlist)))
})

test_that("return_col should exist", {
    vlist <- list(col_c = "good")
    expect_error(selectMatchingValues(lookup, vlist, "does not work"))
})

#selectMatchingMultipleValues test
test_that("basic working example", {
    vlist <- list(col_a = c("treatment", "control"))
    expect_no_error(selectMatchingMultipleValues(lookup, vlist, "subject_id"))
})

test_that("basic working example 2", {
    vlist <- list(col_a = c("treatment", "control"))
    expect_no_error(selectMatchingMultipleValues(lookup, vlist))
})

test_that("values_list should be named", {
    vlist <- c("treatment","wrong")
    expect_error(selectMatchingMultipleValues(lookup, vlist))
})

test_that("column should exist in lookup", {
    vlist <- list(col_j = c("treatment", "control"))
    expect_error(selectMatchingMultipleValues(lookup, vlist, "subject_id"))
})

test_that("return_col should exist", {
    vlist <- list(col_c = c("treatment", "control"))
    expect_error(selectMatchingMultipleValues(lookup, vlist, "does not work"))
})

#selectFromLookup test
test_that("basic working example", {
    expect_no_error(selectFromLookup(measures_data, lookup, "subject_id"))
})

test_that("basic working example 2", {
    expect_no_error(selectFromLookup(measures_data, lookup, "subject_id", "var_a"))
})

test_that("complex_example", {
    subset_lookup <- selectMatchingValues(lookup, list(col_a = "treatment"))
    expect_no_error(selectFromLookup(measures_data, subset_lookup, "subject_id", "var_a"))
})

test_that("column should exist in input_df", {
    expect_error(selectFromLookup(measures_data, lookup, "sample_id"))
})

test_that("column should exist in lookup", {
    expect_error(selectFromLookup(measures_data, lookup, "age"))
})

test_that("return column should exist in input_df", {
    expect_error(selectFromLookup(measures_data, lookup, "subject_id", "col_c"))
})

