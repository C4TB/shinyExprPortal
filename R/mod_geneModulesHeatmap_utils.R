custom_add_col_annotations <- function(p,
         annotation,
         colors = NULL,
         side = c("top","bottom"),
         size = 0.05,
         buffer = 0.015,
         inner_buffer = buffer / 2,
         layout = list(),
         show_colorbar = TRUE,
         range = NULL){
  
  pick_discrete_colors <- utils::getFromNamespace("pick_discrete_colors", "iheatmapr")
  pick_continuous_colors <- utils::getFromNamespace("pick_continuous_colors", "iheatmapr")
  
  side <- match.arg(side)
  # Convert to data.frame
  x <- as.data.frame(annotation)
  
  for (i in seq_len(ncol(x))){
    if (is.character(x[,i]) || is.factor(x[,i]) || is.logical(x[,i])){
      if (!is.null(colors) && colnames(x)[i] %in% names(colors)){
        tmp_colors <- colors[[colnames(x)[i]]]
      } else{
        tmp_colors <- pick_discrete_colors(as.factor(x[,i]), p)
      }
      p <- add_col_groups(p, 
                          x[,i],
                          name = colnames(x)[i],
                          title = colnames(x)[i],
                          colors = tmp_colors,
                          show_colorbar = show_colorbar,
                          side = side,
                          size = size,
                          buffer = if (i == 1)
                            buffer else inner_buffer,
                          layout = layout,
                          show_title = TRUE)
    } else if (is.numeric(x[,i])){
      if (!is.null(colors) && colnames(x)[i] %in% names(colors)){
        tmp_colors <- colors[[colnames(x)[i]]]
      } else{
        tmp_colors <- pick_continuous_colors(zmid = 0, 
                                             zmin = min(x[,i], na.rm = TRUE),
                                             zmax = max(x[,i], na.rm = TRUE), p)
      }
      if (!is.null(range) && colnames(x)[i] %in% names(range)) {
        custom_range <- range[[colnames(x)[i]]]
        zmin <- custom_range[[1]]
        zmax <- custom_range[[2]]
      } else {
        zmin = min(x[,i], na.rm = TRUE)
        zmax = max(x[,i], na.rm = TRUE)
      }

      p <- add_col_signal(p, 
                          x[,i],
                          name = colnames(x)[i],
                          colors = tmp_colors,
                          side = side,
                          size = size,
                          buffer = if (i == 1)
                            buffer else inner_buffer,
                          zmin = zmin,
                          zmax = zmax,
                          layout = layout,
                          show_title = TRUE,
                          show_colorbar = show_colorbar)
    } else{
      stop("Input should be character, factor, logical, or numeric")
    }
  }
  return(p)
}