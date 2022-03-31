# From allGenesScatterplot

mid_rescaler <- function(mid = 0) {
  function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
    scales::rescale_mid(x, to, from, mid)
  }
}

ggplot(df, aes(.data[[x]], .data[[y]], colour = fc)) +
  geom_point() +
  scale_color_distiller(palette = "RdBu", rescaler = mid_rescaler(), 
                        label = function(x) sprintf("%.2f", x)) +
  #scale_color_gradient2(low="#2166ac",mid = "white", high = "#b2182b", midpoint = 0,limits = c(-1, 1)) +
  theme_transp_border() +
  labs(title = "Expression difference between mean of subgroup and mean of all samples")