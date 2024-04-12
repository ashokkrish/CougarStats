RenderScatterplot <- function(df, plotTitle, plotXLab, plotYLab, regLineColour, pointColour, lineWidth = 1, pointSize = 3, gridlines){
  
  sPlot <- ggplot(df, aes(x = x, y = y)) + 
    geom_point(shape = 19,
               size = pointSize,
               colour = pointColour) + 
    geom_smooth(formula = y ~ x, method = lm,  
                se = FALSE,    
                fullrange = TRUE,
                linewidth = lineWidth,
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
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  if("Major" %in% gridlines) {
    sPlot <- sPlot + theme(panel.grid.major = element_line(colour = "#D9D9D9"))
  }
  
  if("Minor" %in% gridlines) {
    sPlot <- sPlot + theme(panel.grid.minor = element_line(colour = "#D9D9D9"))
  }
  
  return(sPlot)
}