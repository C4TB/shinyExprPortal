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
                         fill = .data$estimate,
                         text = signif(.data[[filter]], 2)
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
      p + geom_text(aes(
          label = signif(.data$estimate, 2)),
                             size = 3,
                             data = subset_hm)
  }
  p
  
}

plotly_corr_heatmap <- function(df, max_pvalue = 0.05,
                                min_corr = 0, filter = "pvalue") {
  
  selected_df <- df %>% dplyr::select(contains("estimate"))
  colnames(selected_df) <- gsub("(.*_)*(_estimate)", "\\1", 
                                colnames(selected_df))
  rownames(selected_df) <- df$Gene
  cormat <- as.matrix(selected_df)
  cormat <- cormat[nrow(cormat):1, ]
  
  pval_df <- df %>% dplyr::select(contains("_pvalue")) 
  colnames(pval_df) <- gsub("(.*_)*(_pvalue)", "\\1", 
                                colnames(pval_df))
  rownames(pval_df) <- df$Gene
  pmat <- as.matrix(pval_df)
  pmat <- pmat[nrow(pmat):1, ]
  labels <- cormat
  labels <- signif(labels, 2)
  lv <- !(abs(cormat) > min_corr & pmat < max_pvalue)
  labels[lv] <- ""
  
  hm_colors <- grDevices::colorRamp(rev(RColorBrewer::brewer.pal(5, "RdBu")))
  p <- plotly::plot_ly(x = colnames(cormat),
                       y = rownames(cormat),
                       z = cormat,
                       type = "heatmap",
                       colors = hm_colors,
                       zmin = -1, zmax = 1, zmid = 0,
                       text = labels
                       ) %>% 
    plotly::add_annotations(text = labels,
                            showarrow = FALSE,
                            font = list(size = 8)
    ) %>%
    plotly::layout(
      yaxis = list(type ="category", dtick = 1, tickfont = list(size = 9)),
      xaxis = list(type ="category", side = "top", tickfont = list(size = 9))
     )
}

plotInteractiveCorrHeatmap <- function(df, min_pvalue = 0,
                                       min_corr = 0, filter = "pvalue") { 
  selected_df <- df %>% dplyr::select(contains("estimate"))
  colnames(selected_df) <- gsub("(.*_)*(_estimate)", "\\1", 
                               colnames(selected_df))
  rownames(selected_df) <- df$Gene
  cormat <- as.matrix(selected_df)
  main_heatmap(cormat[nrow(cormat):1, ],
           colors = rev(RColorBrewer::brewer.pal(3,"RdBu")),
           layout = list(font = list(size = 9))) %>%
    add_row_labels() %>%
    add_col_labels(side = "top", textangle = 0)
  

}
