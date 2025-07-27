library(ggplot2)

RenderBoxplot <- function(dat, df_boxplot, df_outliers, plotColour, plotTitle, plotXlab, plotYlab, boxWidth, gridlines, flip) {
  
  # group outliers, if there are duplicates, a (count) appears in the label
  outlier_labels <- df_boxplot %>%
    filter(x %in% df_outliers) %>%
    group_by(x) %>%
    summarise(count = n(), .groups = 'drop') %>%
    mutate(label = ifelse(count > 1, paste0(x, " (", count, ")"), as.character(x)))
  
  outlier_data <- df_boxplot %>%
    filter(x %in% df_outliers) %>%
    left_join(outlier_labels, by = "x")
  
  bp <- ggplot(df_boxplot, aes(x = x, y = 0)) +
    stat_boxplot(geom ='errorbar', width = 0.15) +
    geom_boxplot(width = boxWidth,
                 fill = plotColour,
                 alpha = 1,
                 outlier.shape = NA) +
    geom_point(data = filter(df_boxplot, x %in% df_outliers),
               size = 2) +
    ## Uncomment the below to display the outlier values about the indicator
    geom_text(data = outlier_data,
              aes(x = x, y = 0, label = label),
              size = 15 / .pt,
              vjust = -1) +
    labs(title = plotTitle,
         x = plotXlab,
         y = plotYlab) +
    theme_void() +
    theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5, margin = margin(0,0,5,0)),
          axis.title.x = element_text(size = 16, face = "bold", vjust = -1.5, margin = margin(5,0,0,0)),
          axis.title.y = element_text(size = 16, face = "bold", margin = margin(0,5,0,0)),
          axis.text.x.bottom = element_text(size = 16, margin = margin(5,0,0,0)),
          axis.text.y.left = element_blank(),
          plot.margin = unit(c(1, 1, 1, 1),"cm"),
          panel.border = element_rect(fill = NA)) +
    ylim(-1, 1)
  
  if(length(unique(dat)) == 1) {
    bp <- bp + scale_x_continuous(breaks = dat, limits = c(dat[1] - 1, dat[1] + 1))
  } else {
    bp <- bp + scale_x_continuous(breaks = waiver(), n.breaks = 8, expand = expansion(mult = 0.15))
  }
  
  if("Major" %in% gridlines) {
    bp <- bp + theme(panel.grid.major = element_line(colour = "#D9D9D9"))
  }
  
  if("Minor" %in% gridlines) {
    bp <- bp + theme(panel.grid.minor = element_line(colour = "#D9D9D9"))
  }
  
  if(flip == 1){
    bp <- bp + coord_flip(clip = "off") +
      theme(axis.text.x.bottom = element_blank(),
            axis.text.y.left = element_text(size = 16)) +
      labs(x = plotYlab,
           y = plotXlab)
  }
  
  return(bp) 
}