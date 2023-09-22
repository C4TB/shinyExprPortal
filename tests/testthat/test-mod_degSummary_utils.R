test_that("add_knitr_table works", {
    test_table <- data.frame(
        "a" = c("a", "b"),
        "treatment_a#P" = c(10, 20),
        "treatment_a#P_adj" = c(0,0),
        "treatment_b#P" = c(10, 20),
        "treatment_b#P_adj" = c(0,0)
    )
    model_cols <- c("a")
    header_cols <-
        c("treatment_a#P",
          "treatment_a#P_adj",
          "treatment_b#P",
          "treatment_b#P_adj")
    header_spec <- list("treatment_a" = 2, "treatment_b" = 2)
    expect_no_error(
        add_knitr_table(test_table, model_cols, header_cols, header_spec))
})
