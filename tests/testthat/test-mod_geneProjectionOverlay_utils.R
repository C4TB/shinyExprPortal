test_that("scattrerplot_overlay works", {
    ref_plot <-
        structure(
            list(
                `$schema` = "https://vega.github.io/schema/vega-lite/v5.json",
                params = list(
                    list(
                        name = "group_sel",
                        select = list(
                            type = "point",
                            fields = list("c"),
                            on = "click",
                            clear = "dblclick",
                            toggle = FALSE
                        ),
                        bind = "legend"
                    ),
                    list(name = "second_title",
                         value = "Mean expression")
                ),
                data = list(name = "values"),
                hconcat = list(
                    list(
                        title = "Projected data with group membership",
                        width = 400,
                        height = 400,
                        mark = list(
                            type = "point",
                            filled = TRUE,
                            opacity = 0.75
                        ),
                        encoding = list(
                            tooltip = list(
                                list(field = "c", type = "nominal"),
                                list(field = "d",
                                     type = "nominal")
                            ),
                            x = list(field = "a", type = "quantitative"),
                            y = list(field = "b", type = "quantitative"),
                            color = list(
                                condition = list(
                                    param = "group_sel",
                                    field = "c",
                                    type = "nominal",
                                    scale = list(domain = c("a",
                                                            "b"),
                                                 range = c("#f00", "#0f0"))
                                ),
                                value = "#bbbbbb"
                            )
                        )
                    ),
                    list(
                        title = list(text = list(signal = "second_title")),
                        width = 400,
                        height = 400,
                        mark = list(type = "point",
                                    filled = TRUE),
                        encoding = list(
                            tooltip = list(
                                list(field = "c", type = "nominal"),
                                list(field = "d",
                                     type = "nominal")
                            ),
                            x = list(field = "a", type = "quantitative"),
                            y = list(field = "b", type = "quantitative"),
                            color = list(
                                condition = list(
                                    param = "group_sel",
                                    field = "d",
                                    type = "quantitative",
                                    scale = list(
                                        scheme = "redblue",
                                        reverse = TRUE,
                                        domainMid = 0
                                    ),
                                    legend = list(format = ".2f")
                                ),
                                value = "#bbbbbb"
                            )
                        )
                    )
                )
            ),
            class = c("vegaspec_hconcat",
                      "vegaspec_vega_lite", "vegaspec", "list")
        )
    to_test <- vega_scatterplot_overlay(NULL,"a","b","c","d",c("c","d"), c("#f00","#0f0"), c("a","b"))
    expect_equal(to_test, ref_plot)
})
