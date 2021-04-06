#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @noRd
app_server <- function(input, output, session) {
  # List the first level callModules here
  appdata <- golem::get_golem_options("appdata")
  modules_to_include <- Filter(Negate(is.null), appdata$modules)
  for (module_num in seq_along(modules_to_include)) {
    module_name <- names(modules_to_include)[module_num]
    do.call(
      paste("mod", module_name, "server", sep = "_"),
      list(
        module_name = module_name,
        appdata = appdata
        # input = input,
        # output = output,
        # session = session
      )
    )
  }
  output$about_info <- renderUI({ 
    if (is.null(appdata$about)) {
      p("clinvisx exploration tool")
   } else {
      ext <- tools::file_ext(appdata$about)
      if (!file.exists(appdata$about)) 
       stop("about file not found")
      switch(ext,
             txt = includeText(appdata$about),
             html = includeHTML(appdata$about),
             md = includeMarkdown(appdata$about))
    }
  })
}

dev_module_server <- function(input, output, session) {
  module_name <- golem::get_golem_options("module_name")
  appdata <- golem::get_golem_options("appdata")
  do.call(
    paste("mod", module_name, "server", sep = "_"),
    list(
      module_name = module_name,
      appdata = appdata)
  )
}
