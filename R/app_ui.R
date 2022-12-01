#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @noRd
app_ui <- function(request) {
    config <- get_opts("config")
    modules_to_include <- Filter(Negate(is.null), config$modules)

    about_tab <-
        tabPanel("About",
            value = "about",
            fluidPage(
                uiOutput("icon_menu"),
                tags$script(
                    HTML("$(document).on('click', '.iconclick', function() {
                        Shiny.setInputValue('iconclick', $(this).attr('id'));
                        });")
                ),
                htmlOutput("about_info")
            )
        )

    modules_ui_list <- lapply(names(modules_to_include), function(module_name) {
        call_module(
            module_name,
            "ui",
            config,
            modules_to_include[[module_name]]
        )
    })
    args <- append(list(about_tab), modules_ui_list)
    args$title <- list(config$logo %||% config$name)
    args$windowTitle <- config$windowtitle %||% "Expression analysis portal"
    if (!is.null(config$bootstrap)) {
        args$theme <- do.call(bslib::bs_theme, config$bootstrap)
    }
    args$id <- "tabSelect"
    tagList(
        # Leave this function for adding external resources
        golem_add_external_resources(args$windowTitle),
        # List the first level UI elements here
        # Need to use do.call to pass list of tabPanels to navbarPage
        do.call(navbarPage, args)
    )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @noRd
golem_add_external_resources <- function(name) {
    # importFrom shinyjs useShinyjs
    add_resource_path(
        "www", app_sys("app/www")
    )

    tags$head(
        favicon(),
        bundle_resources(
            path = app_sys("app/www"),
            app_title = name
        ),
        # Add here other external resources
        # for example, you can add shinyalert::useShinyalert()
        # shinyjs::useShinyjs()
    )
}

dev_module_ui <- function(request) {
    module_name <- get_opts("module_name")
    config <- get_opts("config")
    tagList(
        # Leave this function for adding external resources
        golem_add_external_resources(paste(config$name, "analysis portal")),
        # List the first level UI elements here
        fluidPage(
            tags$head(
                tags$link(
                    rel = "stylesheet",
                    type = "text/css",
                    href = "style.css"
                )
            )
        ),
        # Need to use do.call to pass list of tabPanels to navbarPage
        navbarPage(
            title = config$name,
            call_module(
                module_name,
                "ui",
                config,
                config$modules[[module_name]]
            )
        )
    )
}
