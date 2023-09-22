test_that("heatmap works", {
    corr_df <- correlateMatrices(
        x = measures_data[, "age", drop = FALSE],
        y = t(exp_matrix),
        rowname_var = "Gene")
    pvaluesrank <-
        do.call(pmin, c(corr_df[, endsWith(colnames(corr_df), "pvalue"), drop = FALSE],
                        na.rm = TRUE
        ))
    combined_df <- cbind(corr_df, pvaluesrank)
    combined_df <- combined_df[order(combined_df$pvaluesrank), ]
    hm <- combined_df[seq_len(50), ] %>%
        correlationResultsToLong("Gene", "Measures", TRUE)
    ref_plot <-
        structure(
            list(
                "container",
                `$schema` = "https://vega.github.io/schema/vega-lite/v5.json",
                data = list(values = NULL),
                encoding = list(
                    x = list(
                        field = "Measures",
                        type = "nominal",
                        title = FALSE,
                        axis = list(
                            labelAngle = -15,
                            labelAlign = "left",
                            labelFontSize = 11,
                            labelFontWeight = "bold",
                            orient = "top"
                        )
                    ),
                    y = list(
                        field = "Gene",
                        type = "nominal",
                        title = "Gene ranked by lowest p-value",
                        sort = list(field = "pvaluesrank")
                    )
                ),
                layer = list(
                    list(
                        mark = list(type = "rect"),
                        encoding = list(
                            color = list(
                                field = "estimate",
                                type = "quantitative",
                                title = "Correlation",
                                scale = list(
                                    scheme = "redblue",
                                    reverse = TRUE,
                                    domain = c(-1,
                                               1)
                                )
                            )
                        )
                    ),
                    list(
                        mark = list(type = "text", tooltip = list(content = "data")),
                        transform = list(
                            list(filter = "datum.pvalue  <=  0.05 && abs(datum.estimate) >= 0.25")
                        ),
                        encoding = list(text = list(field = "estimate", format = ".2f"))
                    )
                ),
                config = list(axis = list(grid = TRUE, tickBand = "extent"))
            ),
            class = c("vegaspec_layer",
                      "vegaspec_vega_lite", "vegaspec", "list")
        )
    to_test <- vega_heatmap(
        NULL,
        "Measures",
        "Gene",
        "estimate")
    expect_equal(to_test, ref_plot)
})
