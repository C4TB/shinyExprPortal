#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @noRd
app_server <- function(input, output, session) {
  # List the first level callModules here
  config <- golem::get_golem_options("config")
  modules_to_include <- Filter(Negate(is.null), config$modules)
  for (module_name in names(modules_to_include)) {
    call_module(module_name,
                "server",
                config$data,
                config$global,
                modules_to_include[[module_name]])
    # do.call(
    #   paste("mod", module_name, "server", sep = "_"),
    #   list(
    #     module_name = module_name,
    #     appdata = appdata$data,
    #     global = appdata$global,
    #     module_config = modules_to_include[[module_name]]
    #   )
    # )
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

call_module <- function(module_name, type = c("server", "ui"), appdata, global, 
                        module_config) {
  type <- match.arg(type)
  do.call(
    paste("mod", module_name, type, sep = "_"),
    list(
      module_name = module_name,
      appdata = appdata,
      global = global,
      module_config = module_config
    )
  )  
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
  # do.call(
  #   paste("mod", module_name, "server", sep = "_"),
  #   list(
  #     module_name = module_name,
  #     appdata = config$data,
  #     global = config$global,
  #     module_config = modules_to_include[[module_name]]
  #   )
  # )
}
