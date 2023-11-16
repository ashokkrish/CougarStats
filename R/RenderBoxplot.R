RenderBoxplot <- function(dat, df_boxplot, df_outliers, plotColour, plotTitle, plotXLab, plotYLab) {
  
  bp <- ggplot(df_boxplot, aes(x = x, y = 0)) +
    stat_boxplot(geom ='errorbar', width = 0.15) +
    geom_boxplot(fill = plotColour,
                 alpha = 1,
                 outlier.shape = NA) +
    geom_point(data = filter(df_boxplot, x %in% df_outliers),
               size = 5) +
    geom_text(data = filter(df_boxplot, x %in% df_outliers),
              aes(x = x, y = 0, label = x),
              size = 15 / .pt,
              vjust = -1.25) +
    labs(title = plotTitle,
         x = plotXLab,
         y = plotYLab) +
    theme_minimal() +
    theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
          axis.title.x = element_text(size = 16, face = "bold", vjust = -1.5),
          axis.title.y = element_text(size = 16, face = "bold"),
          axis.text.x.bottom = element_text(size = 16),
          axis.text.y.left = element_blank(),
          plot.margin = unit(c(1, 1, 1, 1),"cm")) +
    ylim(-1, 1) +
    coord_cartesian(clip="off")
  
  if(length(unique(dat)) == 1) {
    bp <- bp + scale_x_continuous(breaks = dat, limits = c(dat[1] - 1, dat[1] + 1))
  } else {
    bp <- bp + scale_x_continuous(n.breaks = 8)
  }
  
  return(bp) 
}