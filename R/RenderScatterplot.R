RenderScatterplot <- function(df, plotTitle, plotXLab, plotYLab, regLineColour, pointColour){
  
  sPlot <- ggplot(df, aes(x = x, y = y)) + 
    geom_point(shape = 19,
               size = 3,
               colour = pointColour) + 
    geom_smooth(formula = y ~ x, method = lm,  
                se = FALSE,    
                fullrange = TRUE,
                linewidth = 1,
                colour = regLineColour) +
    labs(title = plotTitle,
         x = plotXLab,
         y = plotYLab) +
    theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
          axis.title.x = element_text(size = 16, face = "bold", vjust = -1.5),
          axis.title.y = element_text(size = 16, face = "bold"),
          axis.text.x.bottom = element_text(size = 12),
          axis.text.y.left = element_text(size = 12),
          panel.background = element_rect(fill = "white", colour = "black"),
          panel.grid.major = element_line(colour = "grey90"),
          panel.grid.minor = element_line(colour = "grey90"))
  
  return(sPlot)
}