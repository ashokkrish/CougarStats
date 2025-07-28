library(ggplot2)

RenderSideBySideBoxplot <- function(dat, df_boxplot, df_outliers, plotColour, plotTitle, plotXlab, plotYlab, boxWidth, gridlines, flip) {
  
  # outlier label - formatting: outlier (count)
  df_outliers <- df_outliers %>%
    group_by(sample, data) %>%
    summarise(count = n(), .groups = 'drop') %>%
    mutate(label = ifelse(count > 1, paste0(data, " (", count, ")"), as.character(data)))
  
  bp <- ggplot(df_boxplot, aes(x = data, y = sample)) +
    stat_boxplot(geom = 'errorbar', width = 0.15) +
    geom_boxplot(width = boxWidth,
                 fill = plotColour,
                 alpha = 1,
                 outlier.shape = NA) + # this turns automatic outlier calculations off
    geom_point(data = df_outliers,
               aes(x = data, y = sample),
               size = 2) +
    geom_text(data = df_outliers,
              aes(x = data, y = sample, label = label),
              size = 15 / .pt,
              vjust = -1.25) +
    labs(title = plotTitle,
         x = plotXlab,
         y = plotYlab) +
    theme_void() +
    theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5, margin = margin(0,0,5,0)),
          axis.title.x = element_text(size = 16, face = "bold", vjust = -1.5),
          axis.title.y = element_text(size = 16, face = "bold"),
          axis.text.x.bottom = element_text(size = 16, margin = margin(5,0,0,0)),
          axis.text.y.left = element_text(size = 16, margin = margin(0,5,0,0)),
          plot.margin = unit(c(1, 1, 1, 1),"cm"),
          panel.border = element_rect(fill = NA)) 
    
  
  if(length(unique(dat)) == 1) {
    bp <- bp + scale_x_continuous(breaks = dat, limits = c(dat[1] - 1, dat[1] + 1))
  } else {
    bp <- bp + scale_x_continuous(n.breaks = 8, limits = c(min(dat) - 1, max(dat) + 1))
  }
    
  if("Major" %in% gridlines) {
    bp <- bp + theme(panel.grid.major = element_line(colour = "#D9D9D9"))
  }
    
  if("Minor" %in% gridlines) {
    bp <- bp + theme(panel.grid.minor = element_line(colour = "#D9D9D9"))
  }
    
    if(flip == 1){
      bp <- bp + coord_flip(clip="off") +
        labs(x = plotYlab,
             y = plotXlab)
    }

  return(bp) 
}