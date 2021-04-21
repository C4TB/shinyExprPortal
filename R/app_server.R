#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @noRd
app_server <- function(input, output, session) {
  # List the first level callModules here
  config <- golem::get_golem_options("config")
  modules_to_include <- Filter(Negate(is.null), config$modules)

  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (not_null(query[["tab"]])) {
      tab <- query[["tab"]]
      updateNavbarPage(session, inputId = "tabSelect", selected = tab)
      session$userData[[tab]] <- query
    }
  })
  
  for (module_name in names(modules_to_include)) {
    call_module(module_name,
                "server",
                config$data,
                config$global,
                modules_to_include[[module_name]])
  }
  
  output$about_info <- renderUI({ 
    if (is.null(config$about)) {
      p("clinvisx exploration tool")
   } else {
      ext <- tools::file_ext(config$about)
      if (!file.exists(config$about)) 
       stop("about file not found")
      switch(ext,
             txt = includeText(config$about),
             html = includeHTML(config$about),
             md = includeMarkdown(config$about))
    }
  })
}

dev_module_server <- function(input, output, session) {
  module_name <- golem::get_golem_options("module_name")
  config <- golem::get_golem_options("config")
  modules_to_include <- Filter(Negate(is.null), config$modules)
  call_module(module_name,
              "server",
              config$data,
              config$global,
              modules_to_include[[module_name]])
}
