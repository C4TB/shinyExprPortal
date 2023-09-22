# All functions here should return actual values (data frames, vectors)

#' Filter a lookup table based on a list of key-value pairs. Optionally,
#' if there is a * in the lookup table, that row will be returned for that
#' condition.
#'
#'
#' @param lookup a data frame with the keys from `values_list` and `return_col`
#'  (if supplied)
#' @param values_list a named vector with key-value pairs from `lookup`
#' @param return_col optional name of column in `lookup`.
#'
#' @return a data frame or vector (if `return_col` is provided)
#' @noRd
selectMatchingValues <- function(lookup, values_list, return_col = NULL) {
  if (!is.vector(values_list) | is.null(names(values_list))) {
    stop("Argument values_list must be a named vector or list")
  }
  if (!all(names(values_list) %in% colnames(lookup))) {
    stop("All keys in argument values_list should be found in lookup table")
  }
  if (!is.null(return_col)) {
    if (!return_col %in% colnames(lookup)) {
      stop("Column ", return_col, " does not exist in lookup data frame")
    }
  }
  for (key in names(values_list)) {
    lookup <- lookup[lookup[, key] == values_list[key] |
      lookup[, key] == "*", ]
  }
  if (!is.null(return_col)) {
    lookup[[return_col]]
  } else {
    lookup
  }
}
#' @importFrom rlang parse_expr
selectMatchingMultipleValues <-
  function(lookup, values_list, return_col = NULL) {
  # Error checking
  if (!is.vector(values_list) | is.null(names(values_list))) {
    stop("Argument values_list must be a named vector or list")
  }
  if (!all(names(values_list) %in% colnames(lookup))) {
      stop("All keys in argument values_list should be found in lookup table")
  }
  if (!is.null(return_col)) {
    if (!return_col %in% colnames(lookup)) {
      stop("Column ", return_col, " does not exist in lookup data frame")
    }
  }
  # Create a list of conditional expressions
  cond <- vapply(seq_along(values_list), function(x) {
    key <- names(values_list)[[x]]
    value <- paste0('"', values_list[[x]], '"')
    op <- " == "
    paste0("(", paste(key, value, sep = op, collapse = " | "), ")")
  }, character(1))
  # Parse list of expression and apply filter
  parsed_cond <- rlang::parse_expr(paste(cond, collapse = " & "))
  subset <- lookup %>% dplyr::filter(!!parsed_cond)
  if (!is.null(return_col)) {
    subset[[return_col]]
  } else {
    subset
  }
}

#' Extract a vector matching a lookup vector
#'
#' This functions matches the rows of an input data frame with the rows of
#' a lookup data frame based on a shared column and returns a vector from
#' the input data frame.
#'
#' @param input_df a data frame containing `matching_col` and `return_col`
#' @param lookup_df a lookup data frame to find `matching_col`
#' @param matching_col a character or factor column that exists in both data
#'  frames
#' @param return_col a column to return from `input_df`
#'
#' @return a vector of `var_a`
#' @noRd
selectFromLookup <- function(input_df, lookup_df, matching_col,
                             return_col = NULL) {
  if (!matching_col %in% colnames(input_df)) {
    stop("Column ", matching_col, " does not exist in input data frame")
  }
  if (!matching_col %in% colnames(lookup_df)) {
    stop("Column ", matching_col, " does not exist in lookup data frame")
  }
  if (!is.null(return_col)) {
    if (!return_col %in% colnames(input_df)) {
      stop("Column ", return_col, " does not exist in data frame")
    }
  }
  return_df <-
    input_df[input_df[[matching_col]] %in% lookup_df[[matching_col]], ]
  if (!is.null(return_col)) {
    return_df[[return_col]]
  } else {
    return_df
  }
}
