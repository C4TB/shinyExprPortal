test_that("heatmap works", {
  expect_no_error(add_heatmap(exp_matrix, "RdBu", measures_data$var_b, c("#f00", "#0f0"),
                              c("a", "b")))
  expect_no_error(add_heatmap(exp_matrix, "RdBu", measures_data$age, c("#f00", "#0f0"),
                                c(0,40)))
  expect_no_error(add_heatmap(exp_matrix, "RdBu", measures_data$var_a, c("#f00", "#0f0","#00f","#000"),
                              c("1","2","3","4")))
  expect_no_error(add_heatmap(exp_matrix, "RdBu", measures_data[,c("age","var_b")],
                              list(age = "RdBu", var_b = "RdBu"),
                              list(age = c(0,40),var_b=c("a","b"))))
})
