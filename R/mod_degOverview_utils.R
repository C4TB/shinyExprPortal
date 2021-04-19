gg_volcano_plot <- function(table,
                            fc_threshold,
                            pvalue_threshold,
                            adjusted = "q.value",
                            gene_column = "Gene") {
  max_x_data <- max(abs(min(table$logFC)), max(table$logFC))
  data_fc_lim <- max(max_x_data, fc_threshold)
  signif_labels_colors <-
    c(
      "not significant" = "gray",
      "log FC" = "darkgreen",
      "p-value" = "blue",
      "log FC and p-value" = "red"
    )
  if (adjusted == "q.value") {
    ylab_text <- "-log10 q"
  } else {
    ylab_text <- "-log10 p"
  }
  p <-
    ggplot(table,
           aes(
             .data$logFC,
             .data[[adjusted]],
             color = stringr::str_wrap(.data$color, width = 20)
           )) +
    geom_point() +
    ylab(ylab_text) + 
    xlab("Log fold change") +
    geom_text(aes(label = .data[[gene_column]]),
              data = table[table$color == "log FC and p-value",],
              vjust = "top",
              hjust = "right") +
    scale_y_continuous(limits = c(0, NA)) +
    xlim(-data_fc_lim, data_fc_lim) +
    scale_color_manual(values = signif_labels_colors) +
    geom_vline(xintercept = -fc_threshold) + 
    geom_vline(xintercept = fc_threshold) +
    geom_hline(yintercept = pvalue_threshold) +
    theme_classic() + 
    theme(legend.title = element_blank())
  p  
}