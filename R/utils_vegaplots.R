vega_layer_scatterplot <-
  function(data,
           x,
           y,
           facet_var,
           facet_sort = NULL,
           label_lookup = NULL,
           scales = c("independent", "shared"),
           color_var = NULL,
           custom_colors = NULL,
           gene_name = NULL,
           opts = list()) {
    scales <- match.arg(scales)

    default_opts <- list(ncolumns = 4, width = 220, height = 170)
    opts <- modifyList(default_opts, opts)

    if (is.null(gene_name)) {
      ctitle <- "Expression versus clinical measure"
    } else {
      ctitle <- paste("Expression level of", gene_name)
    }

    # Points layer
    point_layer <- list(
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
          field = x,
          type = "quantitative",
          title = NULL,
          scale = list(padding = 0.8)
        ),
        y = list(
          field = y,
          type = "quantitative",
          title = NULL
        ),
        tooltip = list(
          list(
            field = x,
            type = "quantitative",
            title = "Measure"
          ),
          list(field = y, type = "quantitative")
        )
      )
    )

    if (!is.null(color_var)) {
      point_layer$encoding$color <- list(field = color_var, type = "nominal")
      point_layer$encoding$tooltip <-
        c(
          point_layer$encoding$tooltip,
          list(list(field = color_var, type = "nominal"))
        )
      if (!is.null(custom_colors)) {
        point_layer$encoding$color$scale <-
          list(range = unlist(custom_colors, use.names = F))
        if (!is.null(names(custom_colors))) {
          point_layer$encoding$color$scale$domain <- names(custom_colors)
        }
      }
    }

    facet_spec <- list(
      field = facet_var,
      header = list(
        title = NULL,
        labelOrient = "top",
        labelAnchor = "start",
        labelFontSize = 12,
        labelFontFamily = "sans-serif"
      )
    )

    if (!is.null(label_lookup)) {
      facet_spec$header$labelExpr <- paste0(label_lookup, "[datum.value]")
    }

    if (!is.null(facet_sort)) {
      facet_spec$sort <- facet_sort
    }

    list(
      `$schema` = "https://vega.github.io/schema/vega-lite/v5.json",
      title = list(
        text = ctitle,
        orient = "left",
        align = "center",
        anchor = "middle"
      ),
      data = list(values = data),
      spec = list(
        layer = list(point_layer),
        resolve = list(axis = list(x = "shared"))
      ),
      facet = facet_spec,
      columns = opts$ncolumns,
      resolve = list(scale = list(x = scales, y = "independent")),
      background = NULL,
      config = list(
        axis = list(grid = FALSE),
        style = list(cell = list(stroke = "transparent")),
        view = list(
          continuousHeight = opts$height,
          continuousWidth = opts$width
        )
      ),
      usermeta = list(
        x = x,
        y = y,
        color_var = color_var
      )
    )
  }

vega_add_fitline <-
  function(chartspec, fit_method = c("none", "linear", "quadratic", "cubic")) {
    fit_method <- match.arg(fit_method)

    if (fit_method == "none") {
      return(chartspec)
    }

    x <- chartspec$usermeta$x
    y <- chartspec$usermeta$y
    facet_var <- chartspec$facet$field
    color_var <- chartspec$usermeta$color_var

    data <- chartspec$data$values

    predict_m <- function(x, lhs, rhs) {
      fit_formula <- switch(fit_method,
        linear = paste(lhs, "~", rhs),
        quadratic = paste(lhs, "~ splines::ns(", rhs, ", df = 2)"),
        cubic = paste(lhs, "~ splines::ns(", rhs, ", df = 3)")
      )
      # Fit regression model
      model <-
        stats::lm(stats::as.formula(fit_formula),
          data = x,
          na.action = na.exclude
        )
      # Predict values and upper and lower CI
      predictions <- stats::predict(model, se = T, na.action = na.exclude)
      data.frame(
        fit = predictions$fit,
        upper_ci = predictions$fit + 2 * predictions$se.fit,
        lower_ci = predictions$fit - 2 * predictions$se.fit
      )
    }

    # Compute models per facet
    if (!is.null(facet_var)) {
      model <- data %>%
        group_by(.data[[facet_var]])
    } else {
      model <- data
    }
    model <- model %>%
      select(all_of(c(x, y, facet_var))) %>%
      tidyr::nest(data = c(x, y)) %>%
      mutate(fit_model = lapply(data, predict_m, lhs = y, rhs = x)) %>%
      tidyr::unnest(c(data, fit_model))

    # Combine with rest of the data
    data <-
      data %>% left_join(model, by = c(x, y, facet_var))

    # Update data
    chartspec$data$values <- data

    errorband_layer <- list(
      mark = list(
        type = "errorband",
        color = "#999"
      ),
      encoding = list(
        tooltip = NULL,
        x = list(
          field = x, type = "quantitative",
          axis = list(domainColor = "black")
        ),
        y = list(
          field = "upper_ci",
          type = "quantitative",
          scale = list(zero = F),
          axis = list(domainColor = "black")
        ),
        y2 = list(field = "lower_ci")
      )
    )

    line_layer <- list(
      mark = list(
        type = "line",
        color = "#55f"
      ),
      encoding = list(
        x = list(field = x, type = "quantitative"),
        y = list(field = "fit", type = "quantitative")
      )
    )

    chartspec$spec$layer <-
      list(errorband_layer, line_layer, chartspec$spec$layer[[1]])

    chartspec
  }

vega_traj_scatterplot <-
  function(data,
           x,
           facet_var,
           color_var,
           group_var,
           color_palette = NULL) {
    point_layer <- list(
      mark = list(
        type = "point",
        filled = TRUE,
        strokeWidth = 1
      ),
      encoding = list(
        x = list(
          field = x,
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
        color = list(field = color_var, type = "nominal")
      )
    )

    if (!is.null(color_palette)) {
      point_layer$encoding$color$scale <- list(range = color_palette)
      if (!is.null(names(color_palette))) {
        point_layer$encoding$color$scale$domain <- names(color_palette)
      }
    }

    list(
      `$schema` = vega_schema(),
      data = list(values = data),
      spec = list(
        layer = list(point_layer),
        resolve = list(axis = list(x = "shared"))
      ),
      facet = list(
        field = facet_var,
        header = list(
          title = x,
          titleAlign = "center",
          titleAnchor = "middle",
          titleOrient = "bottom",
          labelFontWeight = 600,
          labelFontSize = 12
        )
      ),
      title = list(
        text = "Expression",
        orient = "left",
        align = "center",
        anchor = "middle"
      ),
      resolve = list(scale = list(x = "shared", y = "shared")),
      background = NULL,
      config = list(
        view = list(continuousHeight = 225, continuousWidth = 250)
      ),
      usermeta = list(
        x = x,
        y = "expression",
        facet_var = facet_var,
        other_vars = c(group_var, color_var)
      )
    )
  }


vega_heatmap <-
  function(data,
           x,
           y,
           fill,
           max_pvalue = 0.05,
           min_corr = 0.25,
           padj = FALSE) {
    p_cond <- if (padj) "datum.padj" else "datum.pvalue"

    text_filter <-
      paste(p_cond, " <= ", max_pvalue, "&& abs(datum.estimate) >=", min_corr)

    chart <- list(
      width <- "container",
      `$schema` = vega_schema(),
      data = list(values = data),
      encoding = list(
        x = list(
          field = x,
          type = "nominal",
          title = FALSE,
          axis = list(
            labelAngle = 0,
            labelBaseline = "middle",
            labelFontSize = 11,
            labelFontWeight = "bold",
            orient = "top"
          )
        ),
        y = list(
          field = y,
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
              field = fill,
              type = "quantitative",
              title = "Correlation",
              scale = list(
                scheme = "redblue",
                reverse = TRUE,
                domain = c(-1, 1)
              )
            )
          )
        ),
        list(
          mark = list(type = "text", tooltip = list(content = "data")),
          transform = list(
            list(filter = text_filter)
          ),
          encoding = list(
            text = list(field = "estimate", format = ".2f")
          )
        )
      ),
      config = list(axis = list(grid = TRUE, tickBand = "extent"))
    )
  }
