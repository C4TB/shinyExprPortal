test_that("vega traj scatterplot works", {
    df <- data.frame(
        pid = c("P1", "P1", "P1", "P1"),
        time = c(1,2,3,4),
        pasi = c(1,2,3,4),
        expression = c(1,2,3,4)
    )
    ref_plot <-
        list(
            `$schema` = "https://vega.github.io/schema/vega-lite/v5.json",
            data = list(values = structure(
                list(
                    pid = c("P1", "P1", "P1",
                            "P1"),
                    time = c(1, 2, 3, 4),
                    pasi = c(1, 2, 3, 4),
                    expression = c(1,
                                   2, 3, 4)
                ),
                class = "data.frame",
                row.names = c(NA, -4L)
            )),
            title = list(
                text = "Expression",
                orient = "left",
                align = "center",
                anchor = "middle"
            ),
            resolve = list(scale = list(x = "shared",
                                        y = "shared")),
            background = NULL,
            config = list(view = list(
                continuousHeight = 225, continuousWidth = 250
            )),
            usermeta = list(x = "pasi", y = "expression", facet_var = NULL),
            layer = list(list(
                mark = list(
                    type = "point",
                    filled = TRUE,
                    strokeWidth = 1
                ),
                encoding = list(
                    x = list(
                        field = "pasi",
                        type = "quantitative",
                        title = NULL,
                        scale = list(zero = FALSE)
                    ),
                    y = list(
                        field = "expression",
                        type = "quantitative",
                        title = NULL,
                        scale = list(zero = FALSE)
                    ),
                    color = list(field = "time", type = "nominal")
                )
            ))
        )
    to_test <- vega_traj_scatterplot(df, "pasi", color_var = "time")
    expect_equal(to_test, ref_plot)
})

test_that("vega traj scatterplot works with facet and color palette", {
    df <- data.frame(
        pid = c("P1", "P1", "P1", "P1"),
        group = c("1", "1", "2", "2"),
        time = c(1,2,3,4),
        pasi = c(1,2,3,4),
        expression = c(1,2,3,4)
    )
    ref_plot <-
        list(
            `$schema` = "https://vega.github.io/schema/vega-lite/v5.json",
            data = list(values = structure(
                list(
                    pid = c("P1", "P1", "P1",
                            "P1"),
                    group = c("1", "1", "2", "2"),
                    time = c(1, 2, 3, 4),
                    pasi = c(1, 2, 3, 4),
                    expression = c(1, 2, 3, 4)
                ),
                class = "data.frame",
                row.names = c(NA,
                              -4L)
            )),
            title = list(
                text = "Expression",
                orient = "left",
                align = "center",
                anchor = "middle"
            ),
            resolve = list(scale = list(x = "shared", y = "shared")),
            background = NULL,
            config = list(view = list(
                continuousHeight = 225, continuousWidth = 250
            )),
            usermeta = list(x = "pasi", y = "expression", facet_var = "group"),
            spec = list(layer = list(list(
                mark = list(
                    type = "point",
                    filled = TRUE,
                    strokeWidth = 1
                ),
                encoding = list(
                    x = list(
                        field = "pasi",
                        type = "quantitative",
                        title = NULL,
                        scale = list(zero = FALSE)
                    ),
                    y = list(
                        field = "expression",
                        type = "quantitative",
                        title = NULL,
                        scale = list(zero = FALSE)
                    ),
                    color = list(
                        field = "time",
                        type = "nominal",
                        scale = list(
                            range = list(`1` = "#f00", `2` = "#0f0"),
                            domain = c("1",
                                       "2")
                        )
                    )
                )
            )), resolve = list(axis = list(x = "shared"))),
            facet = list(
                field = "group",
                header = list(
                    title = "pasi",
                    titleAlign = "center",
                    titleAnchor = "middle",
                    titleOrient = "bottom",
                    labelFontWeight = 600,
                    labelFontSize = 12
                )
            )
        )
    to_test <- vega_traj_scatterplot(df, "pasi", color_var = "time", facet_var = "group", color_palette = list("1" = "#f00", "2" = "#0f0"))
    expect_equal(to_test, ref_plot)
})

