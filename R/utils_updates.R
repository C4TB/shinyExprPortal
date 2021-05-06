#' Return sample classes selected in UI inputs
#'
#' The list returned by this function is particularly useful for the functions
#' that retrieve rows in a lookup table with columns matching the sample
#'  classes.
#'
#' @param sample_classes list of sample classes lists
#' @param inputs inputs from a Shiny app
#'
#' @return list of key-value pairs
#' @noRd
getSelectedSampleClasses <- function(sample_classes, inputs) {
  values_list <- c()
  input_names <- c()
  for (sample_class in sample_classes) {
    sc_name <- sample_class[["name"]]
    input_names <- c(input_names, sc_name)
    # if (!sc_name %in% names(inputs)) {
    if (is.null(inputs[[sc_name]]))
      stop("Sample class not found in input list.
           Check if UI has been created correctly")
    #}
    values_list <- c(values_list, inputs[[sc_name]])
  }
  names(values_list) <- input_names
  values_list[values_list != "NA"]
}

#' Return subset of sample classes in UI inputs
#'
#' @param subset_classes list of sample class names to filter
#' @param sample_classes list of sample classes lists
#' @param inputs inputs from a Shiny app
#'
#' @return list of key-value pairs
#' @noRd
getSubsetSampleClasses <- function(subset_classes, sample_classes, inputs) {
  sc_logic <- as.logical(unlist(
    lapply(sample_classes,
           function(sc)
             sc$name %in% subset_classes)
  ))
  sample_classes <- sample_classes[which(sc_logic)]
  subset_values <- sapply(sample_classes, function(x) inputs[[x$name]])
  names(subset_values) <- subset_classes
  subset_values
}