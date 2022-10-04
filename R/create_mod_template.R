#' Creates a module code template in current working directory
#'
#' @param module_name module name in camelCase 
#'
#' @export
create_module_template <- function(module_name) {
  MODULENAME <- module_name
  template <-
    'mod_{{MODULENAME}}_ui <- function(module_name,
                              config,
                              module_config) {
  {{MODULENAME}}_tab(ARGUMENTS,
                module_name)
}

{{MODULENAME}}_tab <- function(ARGUMENTS,
                           title = NULL,
                           id = NULL) {
  ns <- NS(id)
  tabPanel(
    title = title %||% "MODULE TITLE",
    value = "{{MODULENAME}}",
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

mod_{{MODULENAME}}_server <- function(module_name, config, module_config) {
  moduleServer(module_name, function(input, output, session) {
    ns <- session$ns
    
    clinical <- config$data$clinical
    expression_matrix <- config$data$expression_matrix
    sample_lookup <- config$data$sample_lookup
    subject_var <- config$subject_variable
    sample_var <- config$sample_variable
    ## REST OF CODE HERE
    
  })
}

{{MODULENAME}}_config <- function(config, ...) { 
  message("Checking {{MODULENAME}} configuration")
  
  requiredPackages <- c("")
  stopIfNotInstalled(requiredPackages, "{{MODULENAME}}")
  
  if (is.null(config$required_variable)) {
    stop("{{MODULENAME}}: 
         \'required_variable\' to ******* is missing")
  }
  
  config
}'
  
  f <- file(paste0("mod_", MODULENAME, ".R"))
  writeLines(whisker::whisker.render(template), f)
  close(f)
}
