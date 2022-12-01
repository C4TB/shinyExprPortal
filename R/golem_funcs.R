get_opts <- function(key) {
    getShinyOption("loaded_opts")[[key]]
}

add_resource_path <- function(prefix, directoryPath, warn_empty = FALSE) {
    list_f <- length(list.files(path = directoryPath)) == 0
    if (list_f) {
        if (warn_empty) {
            warning("No resources to add from resource path (directory empty).")
        }
    } else {
        addResourcePath(prefix, directoryPath)
    }
}

bundle_resources <-
    function(path,
    app_title,
    name = "golem_resources",
    version = "0.0.1",
    meta = NULL,
    head = NULL,
    attachment = NULL,
    package = NULL,
    all_files = TRUE,
    app_builder = "golem") {
        if (length(list.files(path)) > 0) {
            res <- list()
            res[[length(res) + 1]] <- htmltools::htmlDependency(
                name,
                version,
                src = path,
                script = list.files(path,
                    pattern = "\\.js$",
                    recursive = TRUE
                ),
                meta = c(
                    `app-builder` = app_builder,
                    meta
                ),
                head = c(
                    as.character(tags$title(app_title)),
                    as.character(
                        includeScript(system.file("utils/golem-js.js",
                            package = "golem"))
                    ),
                    head
                ),
                attachment = attachment,
                package = package,
                all_files = all_files
            )
            css_nms <- paste0(
                basename(path),
                "/",
                list.files(path,
                    pattern = "\\.css$", recursive = TRUE
                )
            )
            for (i in css_nms) {
                res[[length(res) + 1]] <- tags$link(href = i,
                    rel = "stylesheet")
            }
            res
        }
    }

favicon <-
    function(ico = "favicon", rel = "shortcut icon", resources_path = "www",
    ext = "ico") {
        ico <- file.path(resources_path, paste(ico, ext, sep = "."))
        tags$head(tags$link(rel = rel, href = ico))
    }
