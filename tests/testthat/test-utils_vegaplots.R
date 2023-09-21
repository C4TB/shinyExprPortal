test_that("plot is as expected", {
    ref_plot <-
        list(
            `$schema` = "https://vega.github.io/schema/vega-lite/v5.json",
            title = list(
                text = "Expression versusmeasure",
                orient = "left",
                align = "center",
                anchor = "middle"
            ),
            data = list(values = structure(
                list(
                    subject_id = c("P1", "P2", "P3", "P4"),
                    age = c(30, 35,
                            40, 45),
                    sex = c("Male", "Male", "Male", "Female"),
                    var_a = c(1,
                              2, 3, NA),
                    var_b = c("a", "a", "b", "b")
                ),
                class = "data.frame",
                row.names = c(NA,
                              -4L)
            )),
            spec = list(layer = list(list(
                mark = list(
                    type = "point",
                    color = "black",
                    filled = TRUE,
                    size = 25,
                    stroke = "black",
                    strokeWidth = 0.5
                ),
                encoding = list(
                    x = list(
                        field = "age",
                        type = "quantitative",
                        title = NULL,
                        scale = list(padding = 0.80000000000000004)
                    ),
                    y = list(
                        field = "var_a",
                        type = "quantitative",
                        title = NULL
                    ),
                    tooltip = list(
                        list(
                            field = "age",
                            type = "quantitative",
                            title = "Measure"
                        ),
                        list(field = "var_a", type = "quantitative")
                    )
                )
            )),
            resolve = list(axis = list(x = "shared"))),
            facet = list(
                field = "sex",
                header = list(
                    title = NULL,
                    labelOrient = "top",
                    labelAnchor = "start",
                    labelFontSize = 12,
                    labelFontFamily = "sans-serif"
                )
            ),
            columns = 4,
            resolve = list(scale = list(x = "independent",
                                        y = "independent")),
            background = NULL,
            config = list(
                axis = list(grid = FALSE),
                style = list(cell = list(stroke = "transparent")),
                view = list(continuousHeight = 170, continuousWidth = 220)
            ),
            usermeta = list(x = "age", y = "var_a", color_var = NULL)
        )
    to_test <- vega_layer_scatterplot(measures_data, "age", "var_a", "sex")
    expect_equal(to_test, ref_plot)
})

test_that("plot is as expected with color", {
    ref_plot <-
        list(
            `$schema` = "https://vega.github.io/schema/vega-lite/v5.json",
            title = list(
                text = "Expression versusmeasure",
                orient = "left",
                align = "center",
                anchor = "middle"
            ),
            data = list(values = structure(
                list(
                    subject_id = c("P1", "P2", "P3", "P4"),
                    age = c(30, 35,
                            40, 45),
                    sex = c("Male", "Male", "Male", "Female"),
                    var_a = c(1,
                              2, 3, NA),
                    var_b = c("a", "a", "b", "b")
                ),
                class = "data.frame",
                row.names = c(NA,
                              -4L)
            )),
            spec = list(layer = list(list(
                mark = list(
                    type = "point",
                    color = "black",
                    filled = TRUE,
                    size = 25,
                    stroke = "black",
                    strokeWidth = 0.5
                ),
                encoding = list(
                    x = list(
                        field = "age",
                        type = "quantitative",
                        title = NULL,
                        scale = list(padding = 0.80000000000000004)
                    ),
                    y = list(
                        field = "var_a",
                        type = "quantitative",
                        title = NULL
                    ),
                    tooltip = list(
                        list(
                            field = "age",
                            type = "quantitative",
                            title = "Measure"
                        ),
                        list(field = "var_a", type = "quantitative"),
                        list(field = "var_b", type = "nominal")
                    ),
                    color = list(field = "var_b", type = "nominal")
                )
            )), resolve = list(axis = list(x = "shared"))),
            facet = list(
                field = "sex",
                header = list(
                    title = NULL,
                    labelOrient = "top",
                    labelAnchor = "start",
                    labelFontSize = 12,
                    labelFontFamily = "sans-serif"
                )
            ),
            columns = 4,
            resolve = list(scale = list(x = "independent",
                                        y = "independent")),
            background = NULL,
            config = list(
                axis = list(grid = FALSE),
                style = list(cell = list(stroke = "transparent")),
                view = list(continuousHeight = 170, continuousWidth = 220)
            ),
            usermeta = list(x = "age", y = "var_a", color_var = "var_b")
        )
    to_test <- vega_layer_scatterplot(measures_data, "age", "var_a", "sex", color_var = "var_b")
    expect_equal(to_test, ref_plot)
})

test_that("plot is as expected with quant color", {
    ref_plot <- list(
        `$schema` = "https://vega.github.io/schema/vega-lite/v5.json",
        title = list(
            text = "Expression versusmeasure",
            orient = "left",
            align = "center",
            anchor = "middle"
        ),
        data = list(values = structure(
            list(
                subject_id = c("P1", "P2", "P3", "P4"),
                age = c(30, 35,
                        40, 45),
                sex = c("Male", "Male", "Male", "Female"),
                var_a = c(1,
                          2, 3, NA),
                var_b = c("a", "a", "b", "b")
            ),
            class = "data.frame",
            row.names = c(NA,
                          -4L)
        )),
        spec = list(layer = list(list(
            mark = list(
                type = "point",
                color = "black",
                filled = TRUE,
                size = 25,
                stroke = "black",
                strokeWidth = 0.5
            ),
            encoding = list(
                x = list(
                    field = "age",
                    type = "quantitative",
                    title = NULL,
                    scale = list(padding = 0.80000000000000004)
                ),
                y = list(
                    field = "var_a",
                    type = "quantitative",
                    title = NULL
                ),
                tooltip = list(
                    list(
                        field = "age",
                        type = "quantitative",
                        title = "Measure"
                    ),
                    list(field = "var_a", type = "quantitative"),
                    list(field = "age", type = "quantitative")
                ),
                color = list(field = "age", type = "quantitative")
            )
        )), resolve = list(axis = list(x = "shared"))),
        facet = list(
            field = "sex",
            header = list(
                title = NULL,
                labelOrient = "top",
                labelAnchor = "start",
                labelFontSize = 12,
                labelFontFamily = "sans-serif"
            )
        ),
        columns = 4,
        resolve = list(scale = list(x = "independent",
                                    y = "independent")),
        background = NULL,
        config = list(
            axis = list(grid = FALSE),
            style = list(cell = list(stroke = "transparent")),
            view = list(continuousHeight = 170, continuousWidth = 220)
        ),
        usermeta = list(x = "age", y = "var_a", color_var = "age")
    )
    to_test <- vega_layer_scatterplot(measures_data, "age", "var_a", "sex", color_var = "age")
    expect_equal(to_test, ref_plot)
})

test_that("plot is as expected with custom colors", {
    ref_plot <-
        list(
            `$schema` = "https://vega.github.io/schema/vega-lite/v5.json",
            title = list(
                text = "Expression versusmeasure",
                orient = "left",
                align = "center",
                anchor = "middle"
            ),
            data = list(values = structure(
                list(
                    subject_id = c("P1", "P2", "P3", "P4"),
                    age = c(30, 35,
                            40, 45),
                    sex = c("Male", "Male", "Male", "Female"),
                    var_a = c(1,
                              2, 3, NA),
                    var_b = c("a", "a", "b", "b")
                ),
                class = "data.frame",
                row.names = c(NA,
                              -4L)
            )),
            spec = list(layer = list(list(
                mark = list(
                    type = "point",
                    color = "black",
                    filled = TRUE,
                    size = 25,
                    stroke = "black",
                    strokeWidth = 0.5
                ),
                encoding = list(
                    x = list(
                        field = "age",
                        type = "quantitative",
                        title = NULL,
                        scale = list(padding = 0.80000000000000004)
                    ),
                    y = list(
                        field = "var_a",
                        type = "quantitative",
                        title = NULL
                    ),
                    tooltip = list(
                        list(
                            field = "age",
                            type = "quantitative",
                            title = "Measure"
                        ),
                        list(field = "var_a", type = "quantitative"),
                        list(field = "age", type = "quantitative")
                    ),
                    color = list(
                        field = "age",
                        type = "quantitative",
                        scale = list(scheme = "Reds")
                    )
                )
            )), resolve = list(axis = list(x = "shared"))),
            facet = list(
                field = "sex",
                header = list(
                    title = NULL,
                    labelOrient = "top",
                    labelAnchor = "start",
                    labelFontSize = 12,
                    labelFontFamily = "sans-serif"
                )
            ),
            columns = 4,
            resolve = list(scale = list(x = "independent",
                                        y = "independent")),
            background = NULL,
            config = list(
                axis = list(grid = FALSE),
                style = list(cell = list(stroke = "transparent")),
                view = list(continuousHeight = 170, continuousWidth = 220)
            ),
            usermeta = list(x = "age", y = "var_a", color_var = "age")
        )
    to_test <- vega_layer_scatterplot(measures_data, "age", "var_a", "sex", color_var = "age", custom_colors = "Reds")
    expect_equal(to_test, ref_plot)
})

test_that("plot is as expected with custom named colors", {
    ref_plot <-
        list(
            `$schema` = "https://vega.github.io/schema/vega-lite/v5.json",
            title = list(
                text = "Expression versusmeasure",
                orient = "left",
                align = "center",
                anchor = "middle"
            ),
            data = list(values = structure(
                list(
                    subject_id = c("P1", "P2", "P3", "P4"),
                    age = c(30, 35,
                            40, 45),
                    sex = c("Male", "Male", "Male", "Female"),
                    var_a = c(1,
                              2, 3, NA),
                    var_b = c("a", "a", "b", "b")
                ),
                class = "data.frame",
                row.names = c(NA,
                              -4L)
            )),
            spec = list(layer = list(list(
                mark = list(
                    type = "point",
                    color = "black",
                    filled = TRUE,
                    size = 25,
                    stroke = "black",
                    strokeWidth = 0.5
                ),
                encoding = list(
                    x = list(
                        field = "age",
                        type = "quantitative",
                        title = NULL,
                        scale = list(padding = 0.80000000000000004)
                    ),
                    y = list(
                        field = "var_a",
                        type = "quantitative",
                        title = NULL
                    ),
                    tooltip = list(
                        list(
                            field = "age",
                            type = "quantitative",
                            title = "Measure"
                        ),
                        list(field = "var_a", type = "quantitative"),
                        list(field = "var_b", type = "nominal")
                    ),
                    color = list(
                        field = "var_b",
                        type = "nominal",
                        scale = list(
                            range = c("#ff0000",
                                      "#bcf"),
                            domain = c("b", "undefined")
                        )
                    )
                )
            )), resolve = list(axis = list(x = "shared"))),
            facet = list(
                field = "sex",
                header = list(
                    title = NULL,
                    labelOrient = "top",
                    labelAnchor = "start",
                    labelFontSize = 12,
                    labelFontFamily = "sans-serif"
                )
            ),
            columns = 4,
            resolve = list(scale = list(x = "independent",
                                        y = "independent")),
            background = NULL,
            config = list(
                axis = list(grid = FALSE),
                style = list(cell = list(stroke = "transparent")),
                view = list(continuousHeight = 170, continuousWidth = 220)
            ),
            usermeta = list(x = "age", y = "var_a", color_var = "var_b")
        )
    to_test <- vega_layer_scatterplot(measures_data, "age", "var_a", "sex", color_var = "var_b",custom_colors = list("b" = "#ff0000", "undefined" = "#bcf"))
    expect_equal(to_test, ref_plot)
})

test_that("plot is as expected with fit line", {
    ref_plot <-
        list(
            `$schema` = "https://vega.github.io/schema/vega-lite/v5.json",
            title = list(
                text = "Expression versusmeasure",
                orient = "left",
                align = "center",
                anchor = "middle"
            ),
            data = list(values = structure(
                list(
                    subject_id = c("P1", "P2", "P3", "P4"),
                    age = c(30, 35,
                            40, 45),
                    sex = c("Male", "Male", "Male", "Female"),
                    var_a = c(1,
                              2, 3, NA),
                    var_b = c("a", "a", "b", "b"),
                    fit = c(1,
                            2, 3, NA),
                    upper_ci = c(NaN, NaN, NaN, NA),
                    lower_ci = c(NaN,
                                 NaN, NaN, NA)
                ),
                class = "data.frame",
                row.names = c(NA,
                              -4L)
            )),
            spec = list(
                layer = list(
                    list(
                        mark = list(type = "errorband",
                                    color = "#999"),
                        encoding = list(
                            tooltip = NULL,
                            x = list(
                                field = "age",
                                type = "quantitative",
                                axis = list(domainColor = "black")
                            ),
                            y = list(
                                field = "upper_ci",
                                type = "quantitative",
                                scale = list(zero = FALSE),
                                axis = list(domainColor = "black")
                            ),
                            y2 = list(field = "lower_ci")
                        )
                    ),
                    list(
                        mark = list(type = "line",
                                    color = "#55f"),
                        encoding = list(
                            x = list(field = "age",
                                     type = "quantitative"),
                            y = list(field = "fit", type = "quantitative")
                        )
                    ),
                    list(
                        mark = list(
                            type = "point",
                            color = "black",
                            filled = TRUE,
                            size = 25,
                            stroke = "black",
                            strokeWidth = 0.5
                        ),
                        encoding = list(
                            x = list(
                                field = "age",
                                type = "quantitative",
                                title = NULL,
                                scale = list(padding = 0.80000000000000004)
                            ),
                            y = list(
                                field = "var_a",
                                type = "quantitative",
                                title = NULL
                            ),
                            tooltip = list(
                                list(
                                    field = "age",
                                    type = "quantitative",
                                    title = "Measure"
                                ),
                                list(field = "var_a", type = "quantitative")
                            )
                        )
                    )
                ),
                resolve = list(axis = list(x = "shared"))
            ),
            facet = list(
                field = "var_b",
                header = list(
                    title = NULL,
                    labelOrient = "top",
                    labelAnchor = "start",
                    labelFontSize = 12,
                    labelFontFamily = "sans-serif"
                )
            ),
            columns = 4,
            resolve = list(scale = list(x = "independent",
                                        y = "independent")),
            background = NULL,
            config = list(
                axis = list(grid = FALSE),
                style = list(cell = list(stroke = "transparent")),
                view = list(continuousHeight = 170, continuousWidth = 220)
            ),
            usermeta = list(x = "age", y = "var_a", color_var = NULL)
        )
    to_test <- vega_layer_scatterplot(measures_data, "age", "var_a",facet_var = "var_b") %>%
        vega_add_fitline(fit_method = "linear")
    expect_equal(to_test, ref_plot)
})

test_that("plot is as expected with empty fit line", {
    ref_plot <-
        list(
            `$schema` = "https://vega.github.io/schema/vega-lite/v5.json",
            title = list(
                text = "Expression versusmeasure",
                orient = "left",
                align = "center",
                anchor = "middle"
            ),
            data = list(values = structure(
                list(
                    subject_id = c("P1", "P2", "P3", "P4"),
                    age = c(30, 35,
                            40, 45),
                    sex = c("Male", "Male", "Male", "Female"),
                    var_a = c(1,
                              2, 3, NA),
                    var_b = c("a", "a", "b", "b")
                ),
                class = "data.frame",
                row.names = c(NA,
                              -4L)
            )),
            spec = list(
                layer = list(
                    list(
                        mark = list(
                            type = "point",
                            color = "black",
                            filled = TRUE,
                            size = 25,
                            stroke = "black",
                            strokeWidth = 0.5
                        ),
                        encoding = list(
                            x = list(
                                field = "age",
                                type = "quantitative",
                                title = NULL,
                                scale = list(padding = 0.80000000000000004)
                            ),
                            y = list(
                                field = "var_a",
                                type = "quantitative",
                                title = NULL
                            ),
                            tooltip = list(
                                list(
                                    field = "age",
                                    type = "quantitative",
                                    title = "Measure"
                                ),
                                list(field = "var_a", type = "quantitative")
                            )
                        )
                    )
                ),
                resolve = list(axis = list(x = "shared"))
            ),
            facet = list(
                field = "var_b",
                header = list(
                    title = NULL,
                    labelOrient = "top",
                    labelAnchor = "start",
                    labelFontSize = 12,
                    labelFontFamily = "sans-serif"
                )
            ),
            columns = 4,
            resolve = list(scale = list(x = "independent",
                                        y = "independent")),
            background = NULL,
            config = list(
                axis = list(grid = FALSE),
                style = list(cell = list(stroke = "transparent")),
                view = list(continuousHeight = 170, continuousWidth = 220)
            ),
            usermeta = list(x = "age", y = "var_a", color_var = NULL)
        )
    to_test <- vega_layer_scatterplot(measures_data, "age", "var_a",facet_var = "var_b") %>%
        vega_add_fitline()
    expect_equal(to_test, ref_plot)
})