multiVariableCorr_config <- function(config, data_folder = "") {
  message("Checking multiVariableCorr configuration")

  required_packages <- c("RColorBrewer", "plotly", "DT")
  stopIfNotInstalled(required_packages, "degDetails")

  if (!is.null(config$advanced)) {
    validateAdvancedSettings(config$advanced, "multiVariableCorr")
  }

  if (is.null(config$heatmap_variables)) {
    stop_nice(paste(
      "multiVariableCorr:",
      "named 'scatterplot_variables' list is missing"
    ))
  }

  config
}
