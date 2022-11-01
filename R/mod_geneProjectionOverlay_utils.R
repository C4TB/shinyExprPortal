encoding_list <-
  function(x,
           y,
           color_field,
           field_type,
           tooltip_vars,
           custom_group_colors = NULL,
           colors_domain = NULL) {
    
  if (field_type == "quantitative") {
    color_list <- list(
      condition = list(
        param = "group_sel",
        field = color_field,
        type = field_type,
        scale = list(
          scheme = "redblue",
          reverse = TRUE,
          domainMid = 0
        ),
        legend = list(format = ".2f")
      ),
      value = "#bbbbbb"
    )
  } else {
    color_list <- list(
      condition = list(
        param = "group_sel",
        field = color_field,
        type = field_type
      ),
      value = "#bbbbbb"
    )
    if (!is.null(custom_group_colors)) {
      color_list$condition$scale <- list(
        domain = colors_domain,
        range = custom_group_colors
      )
    }
  }
  list(
    tooltip = lapply(
      tooltip_vars,
      function(x) list(field = x, type = "nominal")
    ),
    x = list(field = x, type = "quantitative"),
    y = list(field = y, type = "quantitative"),
    color = color_list
  )
}

vega_scatterplot_overlay <-
  function(data,
           x,
           y,
           color_var,
           overlay_var,
           tooltip_vars,
           custom_group_colors = NULL,
           colors_domain = NULL,
           title = NULL,
           width = 800) {
    side <- width / 2

    data_value <-
      if (!is.null(data)) list(values = data) else list(name = "values")

    chart <- list(
      `$schema` = "https://vega.github.io/schema/vega-lite/v5.json",
      params = list(
        list(
          name = "group_sel",
          select = list(
            type = "point",
            fields = list(color_var),
            on = "click",
            clear = "dblclick",
            toggle = FALSE
          ),
          bind = "legend"
        ),
        if (is.null(title)) {
          list(
            name = "second_title",
            value = "Mean expression"
          )
        }
      ),
      data = data_value,
      hconcat = list(
        list(
          title = "Projected data with group membership",
          width = side,
          height = side,
          mark = list(type = "point", filled = TRUE, opacity = 0.75),
          encoding = encoding_list(x = x,
                                   y = y,
                                   color_field = color_var,
                                   field_type = "nominal",
                                   tooltip_vars = tooltip_vars,
                                   custom_group_colors = custom_group_colors,
                                   colors_domain = colors_domain)
        ),
        list(
          title = title %||% list(text = list(signal = "second_title")),
          width = side,
          height = side,
          mark = list(type = "point", filled = TRUE),
          encoding =
            encoding_list(x, y, overlay_var, "quantitative", tooltip_vars)
        )
      )
    )
    vegawidget::as_vegaspec(chart)
  }
