mod_MODULENAME_ui <- function(module_name,
                              config,
                              module_config) {
  MODULENAME_tab(ARGUMENTS,
                module_name)
}

MODULENAME_tab <- function(ARGUMENTS,
                           title = NULL,
                           id = NULL) {
  ns <- NS(id)
  tabPanel(
    title = title %||% "MODULE TITLE",
    value = "MODULENAME",
    tags$h5(description %||% "Module description"),
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

mod_MODULENAME_server <- function(module_name, config, module_config) {
  moduleServer(module_name, function(input, output, session) {
    ns <- session$ns
    
    clinical <- config$data$clinical
    expression_matrix <- config$data$expression_matrix
    sample_lookup <- config$data$sample_lookup
    subject_var <- config$subject_variable
    sample_var <- config$sample_variable

    # REST OF CODE HERE
    
  })
}
    
# Config template

#' @noRd
MODULENAME_config <- function(config, ...) { 
  message("Checking MODULENAME configuration")
  
  requiredPackages <- c("")
  stopIfNotInstalled(requiredPackages, "MODULENAME")
  
  if (is.null(config$required_variable)) {
    stop("MODULENAME: 
         'required_variable' to ******* is missing")
  }
  
  config
}