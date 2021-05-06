#' Inverted versions of in, is.null and is.na
#' 
#' @noRd
#' 
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

#' Removes the null from a vector
#' 
#' @noRd
#' 
#' @example 
#' drop_nulls(list(1, NULL, 2))
drop_nulls <- function(x){
  x[!sapply(x, is.null)]
}

#' If x is `NULL`, return y, otherwise return x
#' 
#' @param x,y Two elements to test, one potentially `NULL`
#' 
#' @noRd
#' 
#' @examples
#' NULL %||% 1
"%||%" <- function(x, y){
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
"%|NA|%" <- function(x, y){
  if (is.na(x)) {
    y
  } else {
    x
  }
}

#' Typing reactiveValues is too long
#' 
#' @inheritParams reactiveValues
#' @inheritParams reactiveValuesToList
#' 
#' @noRd
rv <- shiny::reactiveValues
rvtl <- shiny::reactiveValuesToList


file_path <- function(path = "", ...) {
  if (path != "") {
    file.path(path, ...)
  }
  else {
    file.path(...)
  }
}

flattenlist <- function(x){  
  morelists <- sapply(x, function(xprime) class(xprime)[1]=="list")
  out <- c(x[!morelists], unlist(x[morelists], recursive=FALSE))
  if(sum(morelists)){ 
    Recall(out)
  }else{
    return(out)
  }
}

buildURL <- function(key_values, prefix = NULL) {
  key_values <- key_values[key_values != "-"]
  keys <- names(key_values)
  values <- key_values
  params <- paste0(keys, "=", values)
  url_params <- stringr::str_c(params, collapse = "&") 
  if (!is.null(prefix)) {
    URLencode(paste0(prefix, "&", url_params))
  } else {
    URLencode(url_params)
  }
}

appendToURLv <- function(url, key, values) {
  list_of_urls <- paste0(url, "&", key, "=", values)
  vencode <- Vectorize(URLencode)
  vencode(list_of_urls)
}

appendToURL <- function(url, key, value) {
  URLencode(paste0(url, "&", key, "=", value))
}

urlAsTag <- function(values) {
  curlv <- Vectorize(characterURLsub, SIMPLIFY = FALSE)
  curlv(values)
}

valuesToURL <- function(url, key, values) {
  curlv <- Vectorize(characterURLsub, SIMPLIFY = FALSE)
  curlv(appendToURLv(url, key, values))
}