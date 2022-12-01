#' @noRd
corrModules_config <- function(config, data_folder = "") {
    message("Checking corrModules configuration")
    modules <- config$modules
    submodules_config <-
        lapply(names(modules), function(module_name) {
            if (!is.null(modules[[module_name]])) {
                do.call(
                    paste0(module_name, "_config"),
                    list(
                        config = modules[[module_name]],
                        data_folder = data_folder
                    )
                )
            }
        })
    list(modules = stats::setNames(submodules_config, names(modules)))
}
