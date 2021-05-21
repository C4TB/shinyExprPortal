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
      "q-value" = "blue",
      "log FC and p-value" = "red",
      "log FC and q-value" = "red"
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
             color = .data$color,
           )) +
    geom_point() +
    ylab(ylab_text) + 
    xlab("Log fold change") +
    geom_text(aes(label = .data[[gene_column]]),
              data = table[table$signif == 3,],
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

gg_avgexpr_plot <- function(table,
                            pvalue_threshold,
                            adjusted = "q.value",
                            gene_column = "Gene") {
  max_x_data <- max(abs(min(table$AvgExpr)), max(table$AvgExpr))
  signif_labels_colors <-
    c(
      "not significant" = "gray",
      "log FC" = "darkgreen",
      "p-value" = "blue",
      "q-value" = "blue",
      "log FC and p-value" = "red",
      "log FC and q-value" = "red"
    )
  if (adjusted == "q.value") {
    ylab_text <- "-log10 q"
  } else {
    ylab_text <- "-log10 p"
  }
  ggplot(table, aes(y = .data[[adjusted]],
                    x = .data$AvgExpr,
                    color = stringr::str_wrap(.data$color, width = 20))) +
    geom_point() + 
    geom_text(aes(label = .data[[gene_column]]),
              data = table[table$signif == 3,],
              vjust = "top",
              hjust = "right") +
    scale_y_continuous(limits = c(0, NA)) +
    scale_color_manual(values = signif_labels_colors) +
    ylab(ylab_text) + 
    geom_hline(yintercept = pvalue_threshold) +
    xlim(-max_x_data, max_x_data) +
    theme_classic() +
    theme(legend.title = element_blank())
}

prepareResultsTable <-
  function(table,
           fc_threshold = 1,
           pvalue_threshold = -log10(0.05),
           pvalue_adjusted_flag = "p.value"
           ) {
    
  signif_labels <- list("not significant", "log FC",
                        "%s", "log FC and %s")
  pvalue_labels <- list("p.value" = "p-value",
                        "q.value" = "q-value")
  
  pvalue_label <- pvalue_labels[pvalue_adjusted_flag]
  table$pvalue_signif <-
    as.numeric(
      table[[pvalue_adjusted_flag]] < pvalue_threshold
    )
  if ("logFC" %in% colnames(table)) {
    table$fc_signif <-
      as.numeric(abs(table$logFC) > abs(fc_threshold))
    table$signif <- table$fc_signif + 2 * table$pvalue_signif  
  } else {
    table$signif <- 2 * table$pvalue_signif  
  }
  table$color <- sprintf(as.character(signif_labels[table$signif+1]), pvalue_label)
  # Apply log transformation to p and q value
  table$p.value <- -log10(table$p.value)
  table$q.value <- -log10(table$q.value)
  table
}
