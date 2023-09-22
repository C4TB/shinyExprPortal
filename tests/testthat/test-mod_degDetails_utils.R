test_that("vega_volcanoplot works", {
  de_results <- data.frame(
      Gene = c("ABC", "DEF"),
      P.value = c(0.05, 0.05),
      logFC = c(1,1),
      signif = c(3,3)
  )
  ref_plot <-
      list(
          `$schema` = "https://vega.github.io/schema/vega/v5.json",
          data = list(
              list(
                  name = "source_0",
                  values = structure(
                      list(
                          Gene = c("ABC", "DEF"),
                          P.value = c(0.050000000000000003,
                                      0.050000000000000003),
                          logFC = c(1, 1),
                          signif = c(3,
                                     3),
                          log10 = c(1.3010299956639813, 1.3010299956639813),
                          displayGene = c("ABC", "DEF")
                      ),
                      row.names = c(NA, -2L),
                      class = "data.frame"
                  )
              ),
              list(
                  name = "source_1",
                  values = structure(
                      list(logFC = c(-1, 1)),
                      class = "data.frame",
                      row.names = c(NA,
                                    -2L)
                  )
              ),
              list(
                  name = "source_2",
                  values = structure(
                      list(log10 = 1.3010299956639813),
                      class = "data.frame",
                      row.names = c(NA,
                                    -1L)
                  )
              ),
              list(
                  name = "data_0",
                  source = "source_0",
                  transform = list(
                      list(type = "filter", expr = "isValid(datum[\"logFC\"]) &&\n                        isFinite(+datum[\"logFC\"])  &&\n                        isValid(datum[\"log10\"]) &&\n                        isFinite(+datum[\"log10\"])")
                  )
              ),
              list(
                  name = "data_1",
                  source = "source_1",
                  transform = list(
                      list(type = "filter", expr = "isValid(datum[\"logFC\"]) &&\n                                isFinite(+datum[\"logFC\"])")
                  )
              ),
              list(
                  name = "data_2",
                  source = "source_2",
                  transform = list(
                      list(type = "filter", expr = "isValid(datum[\"log10\"]) &&\n                                isFinite(+datum[\"log10\"])")
                  )
              )
          ),
          background = NULL,
          padding = 5,
          width = 400,
          height = 400,
          style = "cell",
          config = list(axis = list(grid = FALSE)),
          marks = list(
              list(
                  name = "points",
                  type = "symbol",
                  style = list("point"),
                  from = list(data = "data_0"),
                  encode = list(
                      update = list(
                          opacity = list(value = 0.5),
                          fill = list(scale = "color", field = "signif_label"),
                          tooltip = list(signal = "{\"Gene\": isValid(datum[\"Gene\"]) ? datum[\"Gene\"] : \"\"+datum[\"Gene\"], \"logFC\": isValid(datum[\"logFC\"]) ?\n                    datum[\"logFC\"] : \"\"+datum[\"logFC\"],\n                    \"-log10 p\": isValid(datum[\"log10\"]) ? datum[\"log10\"] :\n                    \"\"+datum[\"log10\"]}"),
                          x = list(scale = "x", field = "logFC"),
                          y = list(scale = "y", field = "log10")
                      )
                  )
              ),
              list(
                  type = "text",
                  from = list(data = "points"),
                  encode = list(enter = list(
                      text = list(field = "datum.displayGene"),
                      fontSize = list(value = 10)
                  )),
                  transform = list(list(
                      type = "label",
                      avoidBaseMark = TRUE,
                      size = list(signal = "[width+60,height]")
                  ))
              ),
              list(
                  name = "vrule",
                  type = "rule",
                  style = list("rule"),
                  from = list(data = "data_1"),
                  encode = list(
                      update = list(
                          strokeDash = list(value = c(4, 4)),
                          stroke = list(value = "black"),
                          x = list(scale = "x", field = "logFC"),
                          y = list(value = 0),
                          y2 = list(field = list(group = "height"))
                      )
                  )
              ),
              list(
                  name = "hrule",
                  type = "rule",
                  style = list("rule"),
                  from = list(data = "data_2"),
                  encode = list(
                      update = list(
                          strokeDash = list(value = c(4, 4)),
                          stroke = list(value = "black"),
                          y = list(scale = "y", field = "log10"),
                          x2 = list(value = 0),
                          x = list(field = list(group = "width"))
                      )
                  )
              )
          ),
          scales = list(
              list(
                  name = "x",
                  type = "linear",
                  domain = list(-1.5, 1.5),
                  range = list(0, list(signal = "width")),
                  nice = TRUE,
                  zero = TRUE
              ),
              list(
                  name = "y",
                  type = "linear",
                  domain = list(fields = list(
                      list(data = "data_0", field = "log10"),
                      list(data = "data_2", field = "log10")
                  )),
                  range = list(list(signal = "height"), 0),
                  nice = TRUE,
                  zero = TRUE
              ),
              list(
                  name = "color",
                  type = "ordinal",
                  domain = list("not significant", "log FC", "p-value", "log FC and p-value"),
                  range = c("black", "green", "blue", "red")
              )
          ),
          axes = list(
              list(
                  scale = "x",
                  orient = "bottom",
                  grid = FALSE,
                  title = "logFC",
                  labelFlush = TRUE,
                  labelOverlap = TRUE,
                  tickCount = list(signal = "ceil(width/40)"),
                  zindex = 0
              ),
              list(
                  scale = "y",
                  orient = "left",
                  grid = FALSE,
                  title = "-log10 p",
                  labelOverlap = TRUE,
                  tickCount = list(signal = "ceil(height/40)"),
                  zindex = 0
              )
          ),
          legends = list(
              list(
                  title = "Significance",
                  fill = "color",
                  symbolType = "circle",
                  encode = list(symbols = list(update = list(
                      opacity = list(value = 0.5)
                  )))
              )
          )
      )
  to_test <- vega_volcanoplot(de_results)
  expect_equal(to_test, ref_plot)
})

test_that("vega_volcanoplot works", {
    de_results <- data.frame(
        Gene = c("ABC", "DEF"),
        P.value = c(0.05, 0.05),
        logFC = c(1,1),
        signif = c(3,3)
    )
    ref_plot <-
        list(
            `$schema` = "https://vega.github.io/schema/vega/v5.json",
            data = list(
                list(
                    name = "source_0",
                    values = structure(
                        list(
                            Gene = c("ABC", "DEF"),
                            P.value = c(0.050000000000000003,
                                        0.050000000000000003),
                            logFC = c(1, 1),
                            signif = c(3,
                                       3),
                            log10 = c(1.3010299956639813, 1.3010299956639813),
                            displayGene = c("ABC", "")
                        ),
                        row.names = c(NA, -2L),
                        class = "data.frame"
                    )
                ),
                list(
                    name = "source_1",
                    values = structure(
                        list(logFC = c(-1,
                                       1)),
                        class = "data.frame",
                        row.names = c(NA, -2L)
                    )
                ),
                list(
                    name = "source_2",
                    values = structure(
                        list(log10 = 1.3010299956639813),
                        class = "data.frame",
                        row.names = c(NA,
                                      -1L)
                    )
                ),
                list(
                    name = "data_0",
                    source = "source_0",
                    transform = list(
                        list(type = "filter", expr = "isValid(datum[\"logFC\"]) &&\n                        isFinite(+datum[\"logFC\"])  &&\n                        isValid(datum[\"log10\"]) &&\n                        isFinite(+datum[\"log10\"])")
                    )
                ),
                list(
                    name = "data_1",
                    source = "source_1",
                    transform = list(
                        list(type = "filter", expr = "isValid(datum[\"logFC\"]) &&\n                                isFinite(+datum[\"logFC\"])")
                    )
                ),
                list(
                    name = "data_2",
                    source = "source_2",
                    transform = list(
                        list(type = "filter", expr = "isValid(datum[\"log10\"]) &&\n                                isFinite(+datum[\"log10\"])")
                    )
                )
            ),
            background = NULL,
            padding = 5,
            width = 400,
            height = 400,
            style = "cell",
            config = list(axis = list(grid = FALSE)),
            marks = list(
                list(
                    name = "points",
                    type = "symbol",
                    style = list("point"),
                    from = list(data = "data_0"),
                    encode = list(
                        update = list(
                            opacity = list(value = 0.5),
                            fill = list(scale = "color", field = "signif_label"),
                            tooltip = list(signal = "{\"Gene\": isValid(datum[\"Gene\"]) ? datum[\"Gene\"] : \"\"+datum[\"Gene\"], \"logFC\": isValid(datum[\"logFC\"]) ?\n                    datum[\"logFC\"] : \"\"+datum[\"logFC\"],\n                    \"-log10 p\": isValid(datum[\"log10\"]) ? datum[\"log10\"] :\n                    \"\"+datum[\"log10\"]}"),
                            x = list(scale = "x", field = "logFC"),
                            y = list(scale = "y", field = "log10")
                        )
                    )
                ),
                list(
                    type = "text",
                    from = list(data = "points"),
                    encode = list(enter = list(
                        text = list(field = "datum.displayGene"),
                        fontSize = list(value = 10)
                    )),
                    transform = list(list(
                        type = "label",
                        avoidBaseMark = FALSE,
                        size = list(signal = "[width+60,height]")
                    ))
                ),
                list(
                    name = "vrule",
                    type = "rule",
                    style = list("rule"),
                    from = list(data = "data_1"),
                    encode = list(
                        update = list(
                            strokeDash = list(value = c(4, 4)),
                            stroke = list(value = "black"),
                            x = list(scale = "x", field = "logFC"),
                            y = list(value = 0),
                            y2 = list(field = list(group = "height"))
                        )
                    )
                ),
                list(
                    name = "hrule",
                    type = "rule",
                    style = list("rule"),
                    from = list(data = "data_2"),
                    encode = list(
                        update = list(
                            strokeDash = list(value = c(4, 4)),
                            stroke = list(value = "black"),
                            y = list(scale = "y", field = "log10"),
                            x2 = list(value = 0),
                            x = list(field = list(group = "width"))
                        )
                    )
                )
            ),
            scales = list(
                list(
                    name = "x",
                    type = "linear",
                    domain = list(-1.5, 1.5),
                    range = list(0, list(signal = "width")),
                    nice = TRUE,
                    zero = TRUE
                ),
                list(
                    name = "y",
                    type = "linear",
                    domain = list(fields = list(
                        list(data = "data_0", field = "log10"),
                        list(data = "data_2", field = "log10")
                    )),
                    range = list(list(signal = "height"), 0),
                    nice = TRUE,
                    zero = TRUE
                ),
                list(
                    name = "color",
                    type = "ordinal",
                    domain = list("not significant", "log FC", "p-value", "log FC and p-value"),
                    range = c("black", "green", "blue", "red")
                )
            ),
            axes = list(
                list(
                    scale = "x",
                    orient = "bottom",
                    grid = FALSE,
                    title = "logFC",
                    labelFlush = TRUE,
                    labelOverlap = TRUE,
                    tickCount = list(signal = "ceil(width/40)"),
                    zindex = 0
                ),
                list(
                    scale = "y",
                    orient = "left",
                    grid = FALSE,
                    title = "-log10 p",
                    labelOverlap = TRUE,
                    tickCount = list(signal = "ceil(height/40)"),
                    zindex = 0
                )
            ),
            legends = list(
                list(
                    title = "Significance",
                    fill = "color",
                    symbolType = "circle",
                    encode = list(symbols = list(update = list(
                        opacity = list(value = 0.5)
                    )))
                )
            )
        )
    to_test <- vega_volcanoplot(de_results, gene_list = c("ABC"))
    expect_equal(to_test, ref_plot)
})



test_that("prepareModelResults works", {
    de_results <- data.frame(
        Gene = c("ABC", "DEF"),
        P.value = c(0.05, 0.05),
        logFC = c(1,1),
        signif = c(3,3)
    )
    expect_no_error(prepareModelResultsTable(de_results))
})

test_that("prepareModelResults works without logFC", {
    de_results <- data.frame(
        Gene = c("ABC", "DEF"),
        P.value = c(0.05, 0.05),
        signif = c(3,3)
    )
    expect_no_error(prepareModelResultsTable(de_results))
})