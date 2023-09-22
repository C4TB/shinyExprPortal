outlier_functions <- function(key = c("5/95 percentiles", "IQR", "No")) {
  key = match.arg(key)
  switch(key,
  "5/95 percentiles" = valuesInsideQuantileRange,
  "IQR" = valuesInsideTukeyFences,
  "No" = function(x) TRUE
  )
}

#' Replace values with NA conditionally
#'
#' This function sweeps across columns, replacing the values of rows
#' for which `fun` returns FALSE with NA.
#'
#' @param x a data frame
#' @param fun a function that returns a logical vector
#' @noRd
#' @return transformed data frame
replaceFalseWithNA <- function(x, fun) {
  if (is.data.frame(x)) {
    for (col in names(x)) {
      x[!do.call(fun, list(x[[col]])), col] <- NA
    }
  } else {
    x[!do.call(fun, list(x))] <- NA
  }
  x
}

#' Flags if values in vector are within or outside Tukey fences
#'
#' @param x a vector
#' @noRd
#' @return a logical vector
#'
#' @importFrom stats quantile
valuesInsideTukeyFences <- function(x) {
  qt <- quantile(x, probs = c(.25, .75), na.rm = TRUE)
  iqr <- qt[2] - qt[1]
  upper_fence <- qt[2] + 1.5 * iqr
  lower_fence <- qt[1] - 1.5 * iqr
  x >= lower_fence & x <= upper_fence & !is.na(x)
}

#' Flag if values
#'
#' @param x a vector
#' @param probs numeric vector of probabilities with values in 0,1
#' (passed to [stats::quantile()])
#'
#' @return a logical vector
#' @noRd
#' @importFrom stats quantile
valuesInsideQuantileRange <- function(x, probs = c(.05, .95)) {
  qt <- quantile(x, probs = probs, na.rm = TRUE)
  x >= qt[1] & x <= qt[2] & !is.na(x)
}
