#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @noRd
app_ui <- function(request) {
  config <- golem::get_golem_options("config")
  modules_to_include <- Filter(Negate(is.null), config$modules)
  
  about_tab <- tabPanel("About",
                        value = "about",
                        fluidPage(uiOutput("icon_menu"),
                                  htmlOutput("about_info")))
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    fluidPage(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
        )
      ),
    # Need to use do.call to pass list of tabPanels to navbarPage
    do.call(navbarPage,
            c(title = list(config$logo),
              id = "tabSelect",
              append(
                list(about_tab),
            # Cycle through the modules that were identified in the configuration file
            # And call the corresponding UI function
                lapply(names(modules_to_include), function(module_name) {
                  call_module(module_name,
                              "ui",
                              config$data,
                              config$global,
                              modules_to_include[[module_name]])
                })
              )))
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @importFrom shinyjs useShinyjs
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'clinvisx'
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
    shinyjs::useShinyjs()
  )
}

dev_module_ui <- function(request) {
  module_name <- golem::get_golem_options("module_name")
  config <- golem::get_golem_options("config")
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    fluidPage(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css"))
    ),
    # Need to use do.call to pass list of tabPanels to navbarPage
    navbarPage(title = config$name,
               call_module(module_name,
                           "ui",
                           config$data,
                           config$global,
                           config$modules[[module_name]])
    )
  )
}

