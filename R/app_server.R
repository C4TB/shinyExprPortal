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
  
  output$icon_menu <- renderUI({
    req(config$menu)
    actionButtonList <- lapply(config$menu, function(module_name) {
      local_image <- file_path(config$data_folder, "www", paste0(module_name, ".png"))
      if (file.exists(local_image)) {
        image_name <- file_path("local", paste0(module_name, ".png"))
      } else {
        image_name <- file_path("www", paste0(module_name, ".png"))
      }
      # button_style <- paste0("width: 250px;
      #                 height: 250px;
      #                 background: url('", image_name ,"');
      #                 background-size: cover;
      #                 background-position: center;")
      img_style <- "cursor: pointer;
                    height: 250px;
                    width: 250px"
      img(id = module_name, class = "iconclick", src = image_name, style = img_style)
      # actionButton(paste0("goto_", module_name),
      #              label = NULL,
      #              style = button_style)
    })
    actionButtonList$width <- "1000px"
    actionButtonList$cellArgs <- list(style = "padding-right: unset; width: unset")
    do.call(flowLayout, actionButtonList)
  })
  
  observeEvent(input$iconclick, {
    updateNavbarPage(session, inputId = "tabSelect", selected = input$iconclick)
  })
  
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
