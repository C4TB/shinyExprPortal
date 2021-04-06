#' @import shiny
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_longer
#' @importFrom gridExtra grid.arrange
NULL

#' Run the Shiny Application
#'
#' @param config_file The name of the yaml configuration file
#' @param data_folder Optional directory prefix for data files. 
#'  Enables using the same configuration file under different file structures.
#' @param ... Further optional arguments.
#'
#' @export
run_app <- function(
  config_file,
  data_folder = "",
  ...
) {
  golem::with_golem_options(
    app = shinyApp(
      ui = app_ui, 
      server = app_server
    ), 
    golem_opts = list(appdata = parseConfig(config_file, data_folder), ...)
  )
}

#' Run the app loading only a specified module configuration
#' 
#' This function will set and load all global variables and data, but will 
#' only parse and set up the UI and server functions of the specified module.
#' This enables a very lightweight module loading and testing. It still requires
#' a configuration file, but the configuration of other non-specified modules 
#' will not be parsed.
#'
#' @param module_name a module that has been listed under available_modules 
#'  in golem-config.yml
#' @param config_file The name of the yaml configuration file
#' @param data_folder Optional directory prefix for data files
#' @param ... 
#'
#' @noRd
#'
run_module <- function(
  module_name,
  config_file,
  data_folder = "",
  ...
) {
  app <- golem::with_golem_options(
    app = shiny::shinyApp(
      ui = dev_module_ui,
      server = dev_module_server,
    ), 
    golem_opts = list(module_name = module_name,
                      appdata = parseConfig(config_file,
                                            data_folder,
                                            module_name),
                      ...)
  )
  print(app)
}
