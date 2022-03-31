encoding_list <- function(x, y, color_field, field_type) {
  if (field_type == "quantitative") {
    color_list <- list(
      condition = list(
        param = "cluster_sel",
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
        param = "cluster_sel",
        field = color_field,
        type = field_type
      ),
      value = "#bbbbbb"
    )
  }
  list(
    x = list(field = x, type = "quantitative"),
    y = list(field = y, type = "quantitative"),
    color = color_list
  )
}

vega_scatterplot_overlay <- 
  function(data, x, y, color_var, overlay_var, title, width = 800) {
    side <- width/2
  chart <- list(
    `$schema` = "https://vega.github.io/schema/vega-lite/v5.json",
    params = list(
      list(name = "cluster_sel",
           select = list(type = "point",
                         fields = list(color_var),
                         on = "click",
                         clear = "dblclick",
                         toggle = FALSE),
           bind = "legend")
    ),
    data = list(values = data),
    hconcat = list(
      list(
        title = "Projected data with cluster membership",
        width = side,
        height = side,
        mark = list(type = "point", filled = "true", opacity = 0.75),
        encoding = encoding_list(x, y, color_var, "nominal")
      ),
      list(
        title = title,
        width = side,
        height = side,
        mark = list(type = "point", filled = "true"),
        encoding = encoding_list(x, y, overlay_var, "quantitative")
      )
    )
  )
  vegawidget::as_vegaspec(chart)
}