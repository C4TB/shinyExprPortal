#' Access files in the current app
#'
#' @param ... Character vector specifying directory and or file to
#'     point to inside the current package.
#'
#' @noRd
app_sys <- function(...) {
    base::system.file(..., package = "shinyExprPortal")
}


#' Read App Config
#'
#' @param value Value to retrieve from the config file.
#' @param config R_CONFIG_ACTIVE value.
#' @param use_parent Logical, scan the parent directory for config file.
#'
#' @importFrom config get
#'
#' @noRd
get_golem_config <- function(value, config = Sys.getenv("GOLEM_CONFIG_ACTIVE",
                Sys.getenv("R_CONFIG_ACTIVE", "default")), use_parent = TRUE) {
    config::get(
        value = value,
        config = config,
        # Modify this if your config file is somewhere else:
        file = app_sys("app-config.yml"),
        use_parent = use_parent
    )
}

call_module <- function(module_name, type = c("server", "ui"), config,
                        module_config) {
    type <- match.arg(type)
    do.call(
        paste("mod", module_name, type, sep = "_"),
        list(
            module_name = module_name,
            config = config,
            module_config = module_config
        )
    )
}
