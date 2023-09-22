#' Return sample categories selected in UI inputs
#'
#' The list returned by this function is particularly useful for the functions
#' that retrieve rows in a lookup table with columns matching the sample
#'  categories
#'
#' @param sample_categories list of sample categories lists
#' @param inputs inputs from a Shiny app
#'
#' @return list of key-value pairs
#' @noRd
getSelectedSampleCategories <-
  function(sample_categories,
           inputs,
           subset_categories = NULL) {
    values_list <- c()
    input_names <- c()

    for (sc_item in sample_categories) {
      sc_name <- sc_item[["name"]]
      if (is.null(subset_categories) |
          (!is.null(subset_categories) &
           (sc_name %in% subset_categories))) {
        input_names <- c(input_names, sc_name)
        if (is.null(inputs[[sc_name]])) {
          stop("Sample class not found in input list.
               Check if UI has been created correctly")
        }
        values_list <- c(values_list, inputs[[sc_name]])
      }
    }
    names(values_list) <- input_names
    values_list[values_list != "NA"]
  }