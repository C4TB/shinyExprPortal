# corrModules  UI Function
mod_corrModules_ui <- function(module_name, config, module_config) {
    ns <- NS(module_name)
    available_submodules <- get_golem_config("corrModules")
    loaded_submodules <- module_config$modules
    module_tabs <- lapply(names(loaded_submodules), function(submodule_name) {
        if (!submodule_name %in% available_submodules) {
            stop(submodule_name, " module is not supported. Please verify
                spelling.")
        }
        do.call(
            paste("mod", submodule_name, "ui", sep = "_"),
            list(
                module_name = submodule_name,
                config = config,
                module_config = loaded_submodules[[submodule_name]]
            )
        )
    })
    do.call(navbarMenu, c(
        "Correlations",
        module_tabs
    ))
}

mod_corrModules_server <- function(module_name, config, module_config) {
    loaded_submodules <- module_config$modules
    for (submodule_name in names(loaded_submodules)) {
        do.call(
            paste("mod", submodule_name, "server", sep = "_"),
            list(
                module_name = submodule_name,
                config = config,
                module_config = loaded_submodules[[submodule_name]]
            )
        )
    }
}
