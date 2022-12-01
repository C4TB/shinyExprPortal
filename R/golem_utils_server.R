#' If x is `NULL`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NULL`
#'
#' @noRd
#'
#' @examples
#' NULL %||% 1
"%||%" <- function(x, y) {
    if (is.null(x)) {
        y
    } else {
        x
    }
}

#' If x is `NA`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NA`
#'
#' @noRd
#'
#' @examples
#' NA %||% 1
"%|NA|%" <- function(x, y) {
    if (is.na(x)) {
        y
    } else {
        x
    }
}

file_path <- function(path = "", ...) {
    if (path != "") {
        file.path(path, ...)
    } else {
        file.path(...)
    }
}

buildURL <- function(key_values, prefix = NULL) {
    key_values <- key_values[key_values != "-"]
    keys <- names(key_values)
    values <- key_values
    params <- paste0(keys, "=", values)
    url_params <- paste(params, collapse = "&")
    if (!is.null(prefix)) {
        utils::URLencode(paste0(prefix, "&", url_params))
    } else {
        utils::URLencode(url_params)
    }
}

appendToURLv <- function(url, key, values) {
    list_of_urls <- paste0(url, "&", key, "=", values)
    vencode <- Vectorize(utils::URLencode)
    vencode(list_of_urls)
}

appendToURL <- function(url, key, value) {
    utils::URLencode(paste0(url, "&", key, "=", value))
}

urlVector <- function(values, name, baseURL) {
    vapply(values,
        FUN = function(x) {
            paste0(
                '<a href="',
                appendToURL(baseURL, name, x),
                '">', x, "</a>"
            )
        },
        FUN.VALUE = character(1)
    )
}

urlAsTag <- function(values) {
    curlv <- Vectorize(characterURLsub, SIMPLIFY = FALSE)
    curlv(values)
}

valuesToURL <- function(url, key, values) {
    curlv <- Vectorize(characterURLsub, SIMPLIFY = FALSE)
    curlv(appendToURLv(url, key, values))
}

stopIfNotInstalled <- function(packages, mod_name) {
    lv <- vapply(
        packages,
        function(pkg) !requireNamespace(pkg, quietly = TRUE),
        logical(1)
    )
    not_inst <- packages[lv]
    if (length(not_inst) > 0) {
        stop_nice(
            "Package(s) ",
            paste(not_inst, collapse = ", "), " required for '", mod_name,
            "' not found."
        )
    }
}

stop_nice <- function(...) {
    opt <- options(error = NULL)
    on.exit(options(opt))
    stop(..., call. = FALSE)
}
