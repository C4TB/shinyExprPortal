#' degModules UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_degModules_ui <- function(id, appdata){
  ns <- NS(id)
  available_submodules <- get_golem_config("degModules")
  loaded_submodules <- appdata$modules$degModules$modules
  module_tabs <- lapply(names(loaded_submodules), function(submodule_name) {
    if (!submodule_name %in% available_submodules)
      stop(submodule_name, " module is not supported. Please verify spelling.")
    do.call(paste("mod", submodule_name, "ui", sep = "_"),
            list(id = submodule_name, appdata = appdata))
  })
  do.call(navbarMenu, c("Differential Expression Analysis",
                        module_tabs))
}
    
#' degModules Server Function
#'
#' @noRd 
mod_degModules_server <- function(module_name, appdata) {
  # loaded_submodules <- Filter(Negate(is.null),
  loaded_submodules <- appdata$modules$degModules$modules
  for (submodule_name in names(loaded_submodules)) {
    do.call(
      paste("mod", submodule_name, "server", sep = "_"),
      list(
        module_name = submodule_name,
        appdata = appdata
      )
    )
  }
}
