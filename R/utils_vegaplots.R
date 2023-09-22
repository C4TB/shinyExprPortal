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
    opts <- utils::modifyList(default_opts, opts)

    if (is.null(gene_name)) {
      ctitle <- "Expression versusmeasure"
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
      if (is.numeric(data[[color_var]])) {
        ctype <- "quantitative"
      } else {
        ctype <- "nominal"
      }
      point_layer$encoding$color <- list(field = color_var, type = ctype)
      point_layer$encoding$tooltip <-
        c(
          point_layer$encoding$tooltip,
          list(list(field = color_var, type = ctype))
        )
      if (!is.null(custom_colors)) {
        if (length(custom_colors) > 1)
          point_layer$encoding$color$scale <-
            list(range = unlist(custom_colors, use.names = FALSE))
        else {
          point_layer$encoding$color$scale <-
            list(scheme = custom_colors)
        }
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
          na.action = stats::na.exclude
        )
      # Predict values and upper and lower CI
      predictions <-
        stats::predict(model, se = TRUE, na.action = stats::na.exclude)
      data.frame(
        fit = predictions$fit,
        upper_ci = predictions$fit + 2 * predictions$se.fit,
        lower_ci = predictions$fit - 2 * predictions$se.fit
      )
    }

    # Compute models per facet
    if (!is.null(facet_var)) {
      model <- data %>%
        group_by(across(all_of(facet_var)))
    } else {
      model <- data
    }
    model <- model[, c(x, y, facet_var)] %>%
      tidyr::nest(data = all_of(c(x, y))) %>%
      mutate(fit_model = lapply(data, predict_m, lhs = y, rhs = x)) %>%
      tidyr::unnest(all_of(c("data", "fit_model")))

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
          scale = list(zero = FALSE),
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
    if (!is.null(facet_var))
    chartspec$spec$layer <-
      list(errorband_layer, line_layer, unlist(chartspec$spec$layer, FALSE))
    else
      chartspec$layer <-
      list(errorband_layer, line_layer, unlist(chartspec$layer, FALSE))

    chartspec
  }
