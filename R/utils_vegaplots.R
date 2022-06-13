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
      `$schema` = vegawidget::vega_schema(),
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
      `$schema` = vegawidget::vega_schema(),
      data = list(values = data),
      encoding = list(
        x = list(
          field = x,
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

    vegawidget::as_vegaspec(chart)
  }


#' Vega-lite volcanoplot
#'
#' The table must be enriched with a column called color that contains
#'
#' @param data DE table enriched with color column
#' @param fc_min fold change significance line. Default is 1 (and -1)
#' @param pvalue_min p-value significance line. Default is 0.05
#' @param pvalue_col column to use for p-value position. Default is P.value
#' @param colors optional list of custom colors for not significant, logFC,
#'  p-value and logFC and p-value significant. Defaults are black, blue, green
#'  and red
#' @param strokeDash vector with strokeDash configuration. Default is 4,4
#' @param opacity float value for points opacity. Default is 0.5
#'
#' @return vega-lite R spec
vegalite_volcanoplot <- function(data,
                             fc_min = 1,
                             pvalue_min = 0.05,
                             pvalue_col = "P.value",
                             gene_col = "Gene",
                             colors = c("black", "green", "blue", "red"),
                             strokeDash = c(4, 4),
                             opacity = 0.5) {

  data$log10 <- -log10(data[[pvalue_col]])

  logfc_limit <- max(max(abs(data$logFC)) * 1.5, 1.2, fc_min)

  logfc_line <- data.frame(logFC = c(-fc_min, fc_min))
  signif_line <- data.frame(log10 = c(-log10(pvalue_min)))


  signif_labels <- list(
    "not significant", "log FC",
    "%s", "log FC and %s"
  )
  pvalue_labels <- list(
    "P.value" = "p-value",
    "q.value" = "adj. p-value"
  )
  pvalue_label <-
    pvalue_labels[pvalue_col]
  color_domain <-
    lapply(signif_labels, sprintf, pvalue_label)

  chart <- list(
    `$schema` = "https://vega.github.io/schema/vega-lite/v5.json",
    data = list(values = data),
    width = 400,
    height = 400,
    config = list(
      axis = list(grid = FALSE)
    ),
    layer = list(
      list(
        # params = list(
        #   list(name = "grid",
        #        select = "interval",
        #        bind = "scales")
        # ),
        mark = list(type = "point", filled = TRUE, opacity = opacity),
        encoding = list(
          x = list(field = "logFC", type = "quantitative",
                   scale = list(domain = list(-logfc_limit, logfc_limit))),
          y = list(field = "log10", type = "quantitative", title = "-log10 p"),
          color = list(field = "signif_label",
                       type = "nominal",
                       scale = list(
                         domain = color_domain,
                         range = colors
                       ),
                       title = "Significance"),
          tooltip = list(
            list(field = gene_col),
            list(field = "logFC"),
            list(field = "log10", title = "-log10 p")
          )
        )
      ),
      list(
        data = list(values = logfc_line),
        mark = list(type = "rule", strokeDash = strokeDash),
        encoding = list(
          x = list(field = "logFC", type = "quantitative")
        )
      ),
      list(
        data = list(values = signif_line),
        mark = list(type = "rule", strokeDash = strokeDash),
        encoding = list(
          y = list(field = "log10", type = "quantitative")
        )
      )
    )
  )
  chart
}

vega_volcanoplot <- function(data,
                             fc_min = 1,
                             pvalue_min = 0.05,
                             pvalue_col = "P.value",
                             gene_col = "Gene",
                             colors = c("black", "green", "blue", "red"),
                             strokeDash = c(4, 4),
                             opacity = 0.5) {
  
  data$log10 <- -log10(data[[pvalue_col]])
  
  data$displayGene <- ifelse(data$signif == 3, data$Gene, "")
  
  logfc_limit <- max(max(abs(data$logFC)) * 1.5, 1.2, fc_min)
  
  logfc_line <- data.frame(logFC = c(-fc_min, fc_min))
  signif_line <- data.frame(log10 = c(-log10(pvalue_min)))
  
  
  signif_labels <- list(
    "not significant", "log FC",
    "%s", "log FC and %s"
  )
  pvalue_labels <- list(
    "P.value" = "p-value",
    "q.value" = "adj. p-value"
  )
  pvalue_label <-
    pvalue_labels[pvalue_col]
  color_domain <-
    lapply(signif_labels, sprintf, pvalue_label)
  
  chart <- list(
    `$schema` = vegawidget::vega_schema("vega"),
    data = list(
      list(name = "source_0", values = data),
      list(name = "source_1", values = logfc_line),
      list(name = "source_2", values = signif_line),
      list(name = "data_0",
           source = "source_0",
           transform = list(
             list(
               type = "filter",
               expr = "isValid(datum[\"logFC\"]) && isFinite(+datum[\"logFC\"]) && isValid(datum[\"log10\"]) && isFinite(+datum[\"log10\"])"
             )
           )),
      list(name = "data_1",
           source = "source_1",
           transform = list(
             list(
               type = "filter",
               expr = "isValid(datum[\"logFC\"]) && isFinite(+datum[\"logFC\"])"
             )
           )),
      list(name = "data_2",
           source = "source_2",
           transform = list(
             list(
               type = "filter",
               expr = "isValid(datum[\"log10\"]) && isFinite(+datum[\"log10\"])"
             )
           ))
    ),
    background = NULL,
    padding = 5,
    width = 400,
    height = 400,
    style = "cell",
    config = list(
      axis = list(grid = FALSE)
    ),
    marks = list(
      list(
        name = "points",
        type = "symbol",
        style = list("point"),
        from = list(data = "data_0"),
        encode = list(
          update = list(
            opacity = list(value = opacity),
            fill = list(scale = "color", field = "signif_label"),
            tooltip = list(
              signal = "{\"Gene\": isValid(datum[\"Gene\"]) ? datum[\"Gene\"] :
              \"\"+datum[\"Gene\"], \"logFC\": isValid(datum[\"logFC\"]) ?
              datum[\"logFC\"] : \"\"+datum[\"logFC\"], 
              \"-log10 p\": isValid(datum[\"log10\"]) ? datum[\"log10\"] :
              \"\"+datum[\"log10\"]}"
            ),
            x = list(scale = "x", field = "logFC"),
            y = list(scale = "y", field = "log10")
          )
        )
      ),
      list(
        type = "text",
        from = list(data = "points"),
        encode = list(
          enter = list(
            text = list(field = "datum.displayGene"),
            fontSize = list(value = 8)
          )
        ),
        transform = list(
          list(type = "label",
               anchor = list("top", "bottom", "right", "left"),
               offset = list(1),
               size = list(signal = "[width+60,height]"))
        )
      ),
      list(
        name = "vrule",
        type = "rule",
        style = list("rule"),
        from = list(data = "data_1"),
        encode = list(
          update = list(
            strokeDash = list(value = strokeDash),
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
            strokeDash = list(value = strokeDash),
            stroke = list(value = "black"),
            y = list(scale = "y", field = "log10"),
            x2 = list(value = 0),
            x = list(field = list(group = "width"))
          )
        )
      )
    ),
    scales = list(
      list(name = "x",
           type = "linear",
           domain = list(-logfc_limit, logfc_limit),
           range = list(0, list(signal = "width")),
           nice = TRUE,
           zero = TRUE),
      list(name = "y",
           type = "linear",
           domain = list(
             fields = list(
               list(data = "data_0", "field" = "log10"),
               list(data = "data_2", "field" = "log10")
             )
           ),
           range = list(list(signal = "height"), 0),
           nice = TRUE,
           zero = TRUE),
      list(name = "color",
           type = "ordinal",
           domain = color_domain,
           range = colors)
    ),
    axes = list(
      list(scale = "x",
           orient = "bottom",
           grid = FALSE,
           title = "logFC",
           labelFlush = TRUE,
           labelOverlap = TRUE,
           tickCount = list(signal = "ceil(width/40)"),
           zindex = 0),
      list(scale = "y",
           orient = "left",
           grid = FALSE,
           title = "-log10 p",
           labelOverlap = TRUE,
           tickCount = list(signal = "ceil(height/40)"),
           zindex = 0)
    ),
    legends = list(
      list(title = "Significance",
           fill = "color",
           symbolType = "circle",
           encode = list(
             symbols = list(
               update = list(
                 opacity = list(value = opacity)
               )
             )
           ))
    )
  )
  chart
}