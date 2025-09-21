RenderScatterplot <- function(df, model, plotTitle, plotXLab, plotYLab, regLineColour, pointColour, lineWidth = 1, pointSize = 3, gridlines, showConfidenceInterval, showPredictionInterval){
  
  sPlot <- ggplot(df, aes(x = x, y = y)) + 
    geom_point(shape = 19,
               size = pointSize,
               colour = pointColour) + 
    geom_smooth(aes(color = "Regression Line", linetype = "Regression Line"), formula = y ~ x, method = lm,  
                se = FALSE,    
                fullrange = TRUE,
                linewidth = lineWidth,
                show.legend = TRUE) +
    labs(title = plotTitle,
         x = plotXLab,
         y = plotYLab) +
    theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
          axis.title.x = element_text(size = 16, face = "bold", vjust = -1.5),
          axis.title.y = element_text(size = 16, face = "bold"),
          axis.text.x.bottom = element_text(size = 12, face = "bold"),
          axis.text.y.left = element_text(size = 12, face = "bold"),
          panel.background = element_rect(fill = "white", colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 14),
          legend.key.size = unit(1.5, "lines"))
  
  if(showConfidenceInterval) {
    conf_int <- suppressWarnings(predict(model, interval = "confidence"))
    df_conf <- cbind(df, conf_int)
    sPlot <- sPlot + 
      geom_line(data = df_conf, aes(y = lwr, color = "Confidence Interval", linetype = "Confidence Interval")) +
      geom_line(data = df_conf, aes(y = upr, color = "Confidence Interval", linetype = "Confidence Interval"), show.legend = FALSE)
  }
  
  if(showPredictionInterval) {
    pred_int <- suppressWarnings(predict(model, interval = "prediction"))
    df_pred <- cbind(df, pred_int)
    sPlot <- sPlot + 
      geom_line(data = df_pred, aes(y = lwr, color = "Prediction Interval", linetype = "Prediction Interval")) +
      geom_line(data = df_pred, aes(y = upr, color = "Prediction Interval", linetype = "Prediction Interval"), show.legend = FALSE)
  }
  
  sPlot <- sPlot + 
    scale_color_manual(name = NULL,
                       values = c("Regression Line" = regLineColour, "Confidence Interval" = "green", "Prediction Interval" = "red")) +
    scale_linetype_manual(name = NULL,
                          values = c("Regression Line" = "solid", "Confidence Interval" = "dotted", "Prediction Interval" = "dotted"))
  
  if("Major" %in% gridlines) {
    sPlot <- sPlot + theme(panel.grid.major = element_line(colour = "#D9D9D9"))
  }
  
  if("Minor" %in% gridlines) {
    sPlot <- sPlot + theme(panel.grid.minor = element_line(colour = "#D9D9D9"))
  }
  
  return(sPlot)
}