#' Compute median expression for gene modules
#'
#' @param module_df data frame containing `genes` and `modules` columns
#' @param expression_matrix matrix to calculate the medians for samples
#'
#' @return matrix with module names as rownames and original samples from input
#' @noRd
#'
mediansPerModule <- function(module_df, expression_matrix) {
  list_of_modules <- levels(module_df$modules)
  gene_lists <-
    lapply(list_of_modules, function(x)
      module_df[module_df$modules == x, "genes"])
  medians_per_module <-
    lapply(gene_lists, function(x)
      colMediansSubset(expression_matrix, x))
  names(medians_per_module) <- list_of_modules
  do.call(rbind, medians_per_module)
  
}

#' Compute a summary data frame of modules medians
#'
#' @param module_medians matrix of modules medians for all samples
#' @param lookup lookup data frame to match samples to `across_class`
#' @param across_class sample class to compare module medians
#' @param jitter_col if `TRUE`, add jitter to modules for plotting
#'
#' @return a data frame with median modules across sample classes
#' @noRd
#' 
computeModulesSummary <- function(module_medians, lookup, 
                                  across_class, jitter_col = TRUE) {
  
  # Compute summary of medians across a selected class
  agg_by <- list()
  agg_by[[across_class]] <- lookup[[across_class]]
  agg_result <- 
    stats::aggregate(t(module_medians), by = agg_by,
              FUN = stats::median)
  median_across <- pivot_longer(agg_result,
                                -all_of(across_class),
                                names_to = "Modules",
                                values_to = "Median_Expression")
  if (jitter_col)
  median_across$jittered <- 
    jitter(as.numeric(as.factor(median_across[[across_class]])))
  
  median_across
}

#' Compute a profile for a module across a sample class
#'
#' @param module_medians matrix of modules medians for all samples
#' @param selected_module module to compute profile for
#' @param lookup a lookup data frame to match samples with classes
#' @param sample_col sample ID column in `lookup`
#' @param across_class sample class to compare module medians
#'
#' @return data frame including median expression, class and id for each sample
#' @noRd
#'
computeModuleProfile <- function(module_medians, selected_module, lookup,
                                 sample_col, across_class) {
  
  module_median_vector <- module_medians[selected_module, ]
  # Assemble data frame of profile:
  # - median of vectors 
  # - class to compare
  # - sample ID column
  module_profile <- data.frame(as.numeric(module_median_vector),
                               lookup[[across_class]],
                               lookup[[sample_col]])
  colnames(module_profile) <- c("Expression", across_class, sample_col)
  
  # Order by the comparison class then convert sample_col to factor
  # To preserve the order when plotting
  module_profile <-
    module_profile[order(module_profile[[across_class]]), ]
  module_profile[, sample_col] <- 
    factor(module_profile[, sample_col],
           levels = module_profile[, sample_col])
  
  module_profile
}