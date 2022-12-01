#' @import shiny
#' @import dplyr
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom DT DTOutput renderDT
NULL

#' Run the Shiny Application
#'
#' This function should be run only after you have created the configuration
#' file and placed all required files in the app folder. See
#' `vignette("quickstart", package = "shinyExprPortal")` for help with setup or
#' `vignette("fullguide", package = "shinyExprPortal")` for a complete
#' configuration guide.
#'
#' @param config_file The name of the yaml configuration file
#' @param data_folder Optional directory prefix for data files. Use this
#' argument if you want to version your files across different folders
#' @param custom_modules Optional list of available custom modules. See the 
#' 'Details' section.
#' @param nthreads Optional number of threads/cores to speed up loading files
#' and computing correlations on UNIX-based systems. Default is 1
#' @param ... Further optional arguments.
#' 
#' @return Runs the app
#'
#' @details `custom_modules` should contain a list of names for user-defined
#' modules that are loaded in the environment before calling run_app. Each
#' module should be accompanied by the corresponding mod_moduleName_ui,
#' mod_moduleName_server moduleName_config functions. These functions could be
#' placed in a custom_modules.R file, for example, and loaded using `source`.
#' The package will then parse the configuration file, and if it contains one of
#' the custom module names, it will call the module configuration parsing
#' function and add it to the interface. See `vignette("customization")` for a
#' complete example.
#' 
#' Please note that if running on Windows, nthreads will be always set to 1
#' due to limitations on the current implementation. 
#' 
#' @seealso [create_config_wizard()] to create a configuration using a wizard,
#' [create_config_template()] to create a configuration file template.
#'
#' @examples
#' if (interactive()) {
#' run_app("config.yaml", nthreads = 4)
#' }
#' @export
run_app <- function(config_file,
                    data_folder = "",
                    custom_modules = NULL,
                    nthreads = 1L,
                    ...) {
  
  if ((nthreads > 1) && (.Platform$OS.type == "windows")) {
    nthreads <- 1
    message("Multiple threads are not currently supported on Windows. Running ",
            "portal in single-threaded mode.")
  }
  
  app <- shinyApp(
    ui = app_ui,
    server = app_server
  )

  app$appOptions$loaded_opts <-
    list(
      config = parseConfig(config_file,
        data_folder,
        custom_modules = custom_modules,
        nthreads = nthreads
      ),
      nthreads = nthreads,
      ...
    )

  app
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
#'
#'
#' @noRd
#'
run_module <- function(module_name,
                       config_file,
                       data_folder = "",
                       custom_modules = NULL,
                       ...) {
  app <- shiny::shinyApp(
    ui = dev_module_ui,
    server = dev_module_server,
  )
  app$appOptions$loaded_opts <-
    list(
      module_name = module_name,
      config = parseConfig(config_file,
        data_folder,
        test_module = module_name,
        custom_modules = custom_modules
      ),
      ...
    )
  print(app)
}

#' Print list of currently supported modules
#'
#' See \code{vignette("config", package = "shinyExprPortal")} for details on how
#' to configure each module.
#'
#' @return list of available modules
#' @export
#'
#' @examples
#' show_available_modules()
show_available_modules <- function() {
  print(get_golem_config("available_modules"))
}
