#' Create tabPanel-plotOutput from a configuration
#' @noRd
plotsTabPanels <- function(tab_config_list, ns) {
  tl <- lapply(tab_config_list, function(tab_config) {
    tab_id <- tab_config$name
    output_vars <- tab_config$variables
    plotWidth <- {
      if (length(output_vars) < 4) {
        length(output_vars) * 200
      } else {
        800
      }
    }
    plotHeight <- ((length(output_vars) %/% 4) + 1) * 200
    tabPanel(
      tab_id,
      uiOutput(ns(tab_id)),
      style = "height: 100%"
    )
  })
  tl
}
