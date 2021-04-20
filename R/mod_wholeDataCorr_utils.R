#' Creates a heatmap from correlations between genes and clinical variables
#'
#' @param df wide data frame with correlations
#' @param min_pvalue threshold to display labels
#' @param min_corr absolute threshold to display labels
#'
#' @return a ggplot object
#' 
#' @noRd
plotCorrelationHeatmap <- function(df, min_pvalue = 0,
                                   min_corr = 0, filter = "pvalue") { 
  heatmap_df <- df %>%
    pivot_longer(c(-.data$Gene, -.data$pvalues_rank),
                 names_pattern = "(.*)_(.*)", 
                 names_to = c("Variable", ".value"))
  
  p <- ggplot(heatmap_df,
                       aes(
                         x = .data$Variable,
                         y = .data$Gene,
                         fill = .data$estimate
                       )) +
    scale_x_discrete(expand = c(0, 0), position = "top") +
    scale_y_discrete(limits = rev(unique(heatmap_df$Gene))) +
    scale_fill_distiller(palette = "RdBu", limits = c(-1, 1)) +
    geom_tile() +
    ylab("Gene ranked by highest significance")
    theme_classic() +
    theme(
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5
        )
    )
  pvalue_condition <- max(-log10(na.omit(heatmap_df[[filter]]))) > min_pvalue
  corr_condition <- max(abs(na.omit(heatmap_df$estimate))) > min_corr
  
  if (pvalue_condition & corr_condition) {
    subset_condition <-
      (-log10(heatmap_df[[filter]]) > min_pvalue) &
      (abs(heatmap_df$estimate) > min_corr)
    subset_hm <- heatmap_df[subset_condition,]
    p <-
      p + geom_text(aes(label = signif(.data$estimate, 2)),
                             size = 3,
                             data = subset_hm)
  }
  p
  
}