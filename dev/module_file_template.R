mod_MODULENAME_ui <- function(id, appdata, global, module_config) {
  MODULENAME_tab(ARGUMENTS,
                     id)
}

MODULENAME_tab <- function(ARGUMENTS,
                           title = NULL,
                           id = NULL) {
  ns <- NS(id)
  tabPanel(
    title = "MODULE TITLE",
    value = "MODULENAME",
    tags$h5(title %||% "Module description"),
    splitLayout(
      verticalLayout(
        wellPanel(
          ## INPUTS
          )
        ),
        verticalLayout(
          ## OUTPUTS ,
          cellWidths = c("20%", "80%"),
          cellArgs = list(style = "white-space: normal;")
        )
    )
  )
}

mod_MODULENAME_server <- function(module_name, appdata, global, module_config) {
  moduleServer(module_name, function(input, output, session) {
    ns <- session$ns
    
    clinical <- appdata$clinical
    expression_matrix <- appdata$expression_matrix
    sample_lookup <- appdata$sample_lookup

    # REST OF CODE HERE
    
  })
}
    
# Config template

#' @noRd
MODULENAME_config <- function(config, data_folder = "") { 
  message("Checking MODULENAME configuration")
  
  requiredPackages <- c("")
  stopIfNotInstalled(requiredPackages, "MODULENAME")
  
  if (is.null(config$required_variable)) {
    stop("MODULENAME: 
         'required_variable' to ******* is missing")
  }
  
  config
}