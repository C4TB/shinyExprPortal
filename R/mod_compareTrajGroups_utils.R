plotTrajGroups <- function(df,
                     selected_variable,
                     selected_gene,
                     traj_var,
                     facet_var, pal = NULL) {
  ggplot(df,
         aes(x = .data[[selected_variable]], y = .data$expression)) +
    geom_point(aes(color = .data[[traj_var]]),
               size = 2) + #pch=21,colour="black",
    { 
      if (!is.null(pal)) {
        if (length(pal) > 1)
          scale_color_manual(values = pal)
        else 
          scale_color_brewer(palette = pal)
      }
      else scale_fill_viridis_d() 
    } +
    facet_wrap(stats::as.formula(paste("~", facet_var)),
               scales = "fixed") +
    ylab(paste("Expression level of", selected_gene, sep = " ")) +
    theme_bw() +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 12, face = "bold"),
      aspect.ratio = 1,
    )
}
