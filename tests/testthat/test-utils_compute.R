#fast_cor
test_that("working example pearson", {
    expect_no_error(fast_cor(measures_data[, "var_a", drop = FALSE], t(exp_matrix), method = "pearson"))
})

test_that("working example spearman", {
    expect_no_error(fast_cor(measures_data[, "var_a", drop = FALSE], t(exp_matrix), method = "spearman"))
})

test_that("working example kendall", {
    expect_no_error(fast_cor(measures_data[, "var_a", drop = FALSE], t(exp_matrix), method = "kendall"))
})

#compute_pval
test_that("working example", {
    cormat <- fast_cor(measures_data[, "var_a", drop = FALSE], t(exp_matrix))
    expect_no_error(compute_pval(cormat, nrow(measures_data)))
})

test_that("should work also if vector ", {
    cormat <- as.data.frame(fast_cor(measures_data[, "var_a", drop = FALSE], t(exp_matrix)))
    expect_no_error(compute_pval(cormat, nrow(measures_data)))
})


#correlate_matrices
test_that("correlateMatrices working example", {
    expect_no_error(correlateMatrices(measures_data[,c("age","var_a")], t(exp_matrix)))
})

test_that("correlateMatrices working example 2", {
    expect_no_error(correlateMatrices(measures_data$var_a, t(exp_matrix)))
})

test_that("correlateMatrices working example 3", {
    expect_no_error(correlateMatrices(measures_data$var_a, t(exp_matrix), adjust_method = "BH"))
})

test_that("correlateMatrices working example 4", {
    expect_no_error(correlateMatrices(measures_data$var_a, t(exp_matrix), rowname_var = "Var"))
})

test_that("incomp dimensions", {
    expect_error(correlateMatrices(measures_data$var_a, exp_matrix))
})

test_that("incomp dimensions", {
    expect_error(correlateMatrices(measures_data$var_a, t(exp_matrix)[, 1]))
})

#long_corr matrices
test_that("longCorrelationMatrix working example", {
    expect_no_error(longCorrelationMatrix("Gene","Measure",measures_data[,c("age","var_a")], t(exp_matrix)))
})

# corr to long
test_that("working example", {
    cormat <- correlateMatrices(measures_data[,c("age","var_a")], t(exp_matrix))
    expect_no_error(correlationResultsToLong(cormat, "variable", "Measure"))
})

test_that("missing pvaluesrank", {
    cormat <- correlateMatrices(measures_data[,c("age","var_a")], t(exp_matrix))
    expect_error(correlationResultsToLong(cormat, "variable", "Measure", TRUE))
})

test_that("using pvaluesrank works", {
    cormat <- correlateMatrices(measures_data[,c("age","var_a")], t(exp_matrix))
    rank_suffix <- "pvalue"
    pvaluesrank <-
        do.call(pmin, c(cormat[, endsWith(colnames(cormat), rank_suffix)],
                        na.rm = TRUE
        ))
    combined_df <- cbind(cormat, pvaluesrank)
    combined_df <- combined_df[order(combined_df$pvaluesrank), ]
    expect_no_error(correlationResultsToLong(combined_df, "variable", "Measure", TRUE))
})

#corrResultsToTable
test_that("working example", {
    cormat <- correlateMatrices(measures_data[,c("age","var_a")], t(exp_matrix))
    expect_no_error(corrResultsToTable(cormat))
})

test_that("rowname_col does not exist in df", {
    cormat <- correlateMatrices(measures_data[,c("age","var_a")], t(exp_matrix))
    expect_error(corrResultsToTable(cormat, rowname_col = "won't work"))
})

test_that("wrong input format", {
    cormat <- fast_cor(measures_data[,c("age","var_a")], t(exp_matrix))
    expect_error(corrResultsToTable(cormat))
})
