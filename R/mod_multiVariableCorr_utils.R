vega_heatmap <-
  function(data,
           x,
           y,
           fill,
           max_pvalue = 0.05,
           min_corr = 0.25,
           padj = FALSE,
           custom_heatmap_scheme = "redblue") {
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
                scheme = custom_heatmap_scheme,
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