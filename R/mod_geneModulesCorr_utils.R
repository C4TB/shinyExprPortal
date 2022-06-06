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
    lapply(list_of_modules, function(x) {
      module_df[module_df$modules == x, "genes"]
    })
  medians_per_module <-
    lapply(gene_lists, function(x) {
      colMediansSubset(expression_matrix, x)
    })
  names(medians_per_module) <- list_of_modules
  do.call(rbind, medians_per_module)
}

#' Compute a summary data frame of modules medians
#'
#' @param module_medians matrix of modules medians for all samples
#' @param lookup lookup data frame to match samples to `across_category`
#' @param across_category sample class to compare module medians
#' @param jitter_col if `TRUE`, add jitter to modules for plotting
#'
#' @return a data frame with median modules across sample categories
#' @noRd
#'
computeModulesSummary <- function(module_medians, lookup,
                                  across_category, jitter_col = TRUE) {

  # Compute summary of medians across a selected class
  agg_by <- list()
  agg_by[[across_category]] <- lookup[[across_category]]
  agg_result <-
    stats::aggregate(t(module_medians),
      by = agg_by,
      FUN = stats::median
    )
  median_across <- pivot_longer(agg_result,
    -all_of(across_category),
    names_to = "Modules",
    values_to = "Median_Expression"
  )
  if (jitter_col) {
    median_across$jittered <-
      jitter(as.numeric(as.factor(median_across[[across_category]])))
  }

  median_across
}

#' Compute a profile for a module across a sample category
#'
#' @param module_medians matrix of modules medians for all samples
#' @param selected_module module to compute profile for
#' @param lookup a lookup data frame to match samples with classes
#' @param sample_col sample ID column in `lookup`
#' @param across_category sample category to compare module medians
#'
#' @return data frame including median expression, category and id for each
#'  sample
#' @noRd
#'
computeModuleProfile <- function(module_medians, selected_module, lookup,
                                 sample_col, across_category) {
  module_median_vector <- module_medians[selected_module, ]
  # Assemble data frame of profile:
  # - median of vectors
  # - class to compare
  # - sample ID column
  module_profile <- data.frame(
    as.numeric(module_median_vector),
    lookup[[across_category]],
    lookup[[sample_col]]
  )
  colnames(module_profile) <- c("Expression", across_category, sample_col)

  # Order by the comparison class then convert sample_col to factor
  # To preserve the order when plotting
  module_profile <-
    module_profile[order(module_profile[[across_category]]), ]
  module_profile[, sample_col] <-
    factor(module_profile[, sample_col],
      levels = module_profile[, sample_col]
    )

  module_profile
}

plotModulesOverview <- function(overview_data, across_category) {
  across_formula <- stats::as.formula(paste("~", across_category))

  overview_data %>%
    plotly::plot_ly(source = "overview", key = ~Modules) %>%
    plotly::add_markers(
      x = ~jittered,
      y = ~Median_Expression,
      color = across_formula,
      marker = list(size = 6, opacity = 0.5),
      colors = "Set1",
      hoverinfo = "text",
      text = ~ paste0(
        "Module: ", Modules,
        "<br>Median Expression: ", signif(Median_Expression, 4)
      )
    ) %>%
    plotly::layout(
      yaxis = list(title = "Median Expression"),
      xaxis = list(
        title = across_category,
        showticklabels = FALSE
      )
    ) %>%
    htmlwidgets::onRender(
      "
        function(el, x) {
          el.on('plotly_hover', function(d) {
          Plotly.d3.select('.cursor-crosshair').style('cursor', 'pointer');
          });
          el.on('plotly_unhover', function(d) {
          Plotly.d3.select('.cursor-crosshair').style('cursor', 'crosshair');
          });
        }
    "
    )
}

plotModuleProfile <- function(module_profile, expression_col, sample_col,
                              across_category, plot_title = "") {
  module_exp <- module_profile[, expression_col]
  y_pos <- sum(module_exp) / length(module_exp)
  ggplot(module_profile, aes(
    x = .data[[sample_col]],
    y = .data[[expression_col]]
  )) +
    geom_tile(
      height = Inf,
      aes(
        x = .data[[sample_col]],
        y = y_pos,
        fill = as.factor(.data[[across_category]])
      )
    ) +
    geom_line(aes(group = 1)) +
    scale_fill_brewer(palette = "Set1") +
    theme(
      plot.title = element_text(
        lineheight = 0.8,
        face = "bold",
        colour = "black",
        size = 15
      ),
      axis.title = element_text(
        face = "bold",
        colour = "black",
        size = 15
      ),
      axis.text = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none"
    ) +
    ggtitle(paste(strwrap(plot_title, 50), collapse = "\n"))
}
