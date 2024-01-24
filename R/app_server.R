#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd
#' @importFrom htmltools includeMarkdown
app_server <- function(input, output, session) {
    # List the first level callModules here
    config <- get_opts("config")
    modules_to_include <- Filter(Negate(is.null), config$modules)

    shinyhelper::observe_helpers(help_dir = system.file("helpfiles",
                                            package = utils::packageName()))

    observe({
        query <- parseQueryString(session$clientData$url_search)
        if (!is.null(query[["tab"]])) {
            tab <- query[["tab"]]
            updateNavbarPage(session, inputId = "tabSelect", selected = tab)
            session$userData[[tab]] <- query
        }
    })

    for (module_name in names(modules_to_include)) {
        call_module(
            module_name,
            "server",
            config,
            modules_to_include[[module_name]]
        )
    }

    output$icon_menu <- renderUI({
        req(config$iconMenu)
        img_icon_list <- lapply(config$iconMenu, function(module_name) {
            local_image <- file_path(
                config$data_folder, "www",
                paste0(module_name, ".png")
            )
            if (file.exists(local_image)) {
                image_name <- file_path("local", paste0(module_name, ".png"))
            } else {
                image_name <- file_path("www", paste0(module_name, ".png"))
            }
            img(id = module_name, class = "iconclick", src = image_name)
        })
        img_icon_list$width <- "1000px"
        img_icon_list$cellArgs <-
            list(style = "padding-right: unset; width: unset")
        do.call(flowLayout, img_icon_list)
    })

    observeEvent(input$iconclick, {
        updateNavbarPage(session,
            inputId = "tabSelect",
            selected = input$iconclick
        )
    })

    output$about_info <- renderUI({
        if (is.null(config$about)) {
            p("Welcome to shinExprPortal. This is a placeholder
    introduction when the 'about' file has not been defined. The tool supports
    text, HTML and markdown files. Create one in your application folder and
    point to it in your configuration file using 'about: file_name.ext'.")
        } else {
            ext <- file_ext(config$about)
            switch(ext,
                txt = includeText(config$about),
                html = includeHTML(config$about),
                md = .includeMarkdown(config$about)
            )
        }
    })
}

dev_module_server <- function(input, output, session) {
    module_name <- get_opts("module_name")
    config <- get_opts("config")
    modules_to_include <- Filter(Negate(is.null), config$modules)
    call_module(
        module_name,
        "server",
        config,
        modules_to_include[[module_name]]
    )
}
