RenderScatterplot <- function(
    df,
    model,
    plotTitle,
    plotXLab,
    plotYLab,
    regLineColour,
    pointColour,
    lineWidth = 1,
    pointSize = 3,
    gridlines,
    showConfidenceInterval,
    showPredictionInterval,
    showRegressionLine
) {
  
  # -------------------------------------------------------------------------
  # Base scatter plot
  # -------------------------------------------------------------------------
  
  p <- plot_ly() %>%
    add_trace(
      data   = df,
      x      = ~x,
      y      = ~y,
      type   = "scatter",
      mode   = "markers",
      name   = "Data Points",
      marker = list(
        color  = pointColour,
        size   = pointSize * 3,
        symbol = "circle"
      ),
      hovertemplate = paste0(
        "<b>", plotXLab, ":</b> %{x}<br>",
        "<b>", plotYLab, ":</b> %{y}<br>",
        "<extra></extra>"
      )
    ) %>%
    layout(
      xaxis = list(
        title = plotXLab,
        showline = TRUE,
        linewidth = 2,
        linecolor = "black",
        mirror = FALSE,
        zeroline = TRUE,
        zerolinecolor = "black"
      ),
      yaxis = list(
        title = plotYLab,
        showline = TRUE,
        linewidth = 2,
        linecolor = "black",
        mirror = FALSE,
        zeroline = TRUE,
        zerolinecolor = "black"
      )
    )
  
  # -------------------------------------------------------------------------
  # Regression Line
  # -------------------------------------------------------------------------
  
  if(showRegressionLine) {
    
    x_seq    <- seq(min(df$x), max(df$x), length.out = 200)
    new_data <- data.frame(datx = x_seq)
    y_pred   <- predict(model, newdata = new_data)
    
    p <- p %>%
      add_trace(
        x          = x_seq,
        y          = y_pred,
        type       = "scatter",
        mode       = "lines",
        name       = "Regression Line",
        line       = list(
          color = regLineColour,
          width = lineWidth * 2
        ),
        hovertemplate = paste0(
          "<b>Fitted:</b> %{y:.4f}<br>",
          "<extra></extra>"
        )
      )
  }
  
  # -------------------------------------------------------------------------
  # Confidence Interval
  # -------------------------------------------------------------------------
  
  if(showConfidenceInterval) {

    x_seq_ci <- seq(min(df$x), max(df$x), length.out = 200)
    conf_int <- suppressWarnings(
      predict(model, newdata = data.frame(datx = x_seq_ci), interval = "confidence")
    )

    df_conf <- data.frame(x = x_seq_ci, conf_int)
    
    # Upper bound
    p <- p %>%
      add_trace(
        data       = df_conf,
        x          = ~x,
        y          = ~upr,
        type       = "scatter",
        mode       = "lines",
        name       = "Confidence Interval",
        line       = list(
          color = "darkblue",
          width = lineWidth * 2,
          dash  = "dash"
        ),
        hovertemplate = paste0(
          "<b>CI Upper:</b> %{y:.4f}<br>",
          "<extra></extra>"
        )
      ) %>%
      # Lower bound - same legend group so only one legend entry
      add_trace(
        data       = df_conf,
        x          = ~x,
        y          = ~lwr,
        type       = "scatter",
        mode       = "lines",
        name       = "Confidence Interval",
        showlegend = FALSE,
        line       = list(
          color = "darkblue",
          width = lineWidth * 2,
          dash  = "dash"
        ),
        hovertemplate = paste0(
          "<b>CI Lower:</b> %{y:.4f}<br>",
          "<extra></extra>"
        )
      )
  }
  
  # -------------------------------------------------------------------------
  # Prediction Interval
  # -------------------------------------------------------------------------
  
  if(showPredictionInterval) {

    x_seq_pi <- seq(min(df$x), max(df$x), length.out = 200)
    pred_int <- suppressWarnings(
      predict(model, newdata = data.frame(datx = x_seq_pi), interval = "prediction")
    )

    df_pred <- data.frame(x = x_seq_pi, pred_int)
    
    # Upper bound
    p <- p %>%
      add_trace(
        data       = df_pred,
        x          = ~x,
        y          = ~upr,
        type       = "scatter",
        mode       = "lines",
        name       = "Prediction Interval",
        line       = list(
          color = "red",
          width = lineWidth * 2,
          dash  = "dash"
        ),
        hovertemplate = paste0(
          "<b>PI Upper:</b> %{y:.4f}<br>",
          "<extra></extra>"
        )
      ) %>%
      # Lower bound
      add_trace(
        data       = df_pred,
        x          = ~x,
        y          = ~lwr,
        type       = "scatter",
        mode       = "lines",
        name       = "Prediction Interval",
        showlegend = FALSE,
        line       = list(
          color = "red",
          width = lineWidth * 2,
          dash  = "dash"
        ),
        hovertemplate = paste0(
          "<b>PI Lower:</b> %{y:.4f}<br>",
          "<extra></extra>"
        )
      )
  }
  
  # -------------------------------------------------------------------------
  # Layout
  # -------------------------------------------------------------------------
  
  # Gridlines
  xgrid <- if("Major" %in% gridlines) TRUE else FALSE
  ygrid <- if("Major" %in% gridlines) TRUE else FALSE
  
  p <- p %>%
    layout(
      title = list(
        text    = plotTitle,
        font    = list(size = 20, family = "Arial Black"),
        y       = 0.95,
        x       = 0.5,
        xanchor = "center",
        yanchor = "top"
      ),
      margin = list(t = 80, r = 20, b = 80, l = 60),
      xaxis = list(
        title      = plotXLab,
        showgrid   = xgrid,
        gridcolor  = "#D9D9D9",
        zeroline   = FALSE,
        titlefont  = list(size = 14, family = "Arial")
      ),
      yaxis = list(
        title      = plotYLab,
        showgrid   = ygrid,
        gridcolor  = "#D9D9D9",
        zeroline   = FALSE,
        titlefont  = list(size = 14, family = "Arial")
      ),
      legend = list(
        orientation = "h",
        x           = 0.5,
        xanchor     = "center",
        y           = -0.2
      ),
      plot_bgcolor  = "white",
      paper_bgcolor = "white",
      hovermode     = "closest"
    )
  
  return(p)
}


# OLD CODE 

# RenderScatterplot <- function(
#     df,
#     model,
#     plotTitle,
#     plotXLab,
#     plotYLab,
#     regLineColour,
#     pointColour,
#     lineWidth = 1,
#     pointSize = 3,
#     gridlines,
#     showConfidenceInterval,
#     showPredictionInterval,
#     showRegressionLine
# ) {
#   
#   sPlot <- ggplot(df, aes(x = x, y = y)) + 
#     geom_point(
#       shape   = 19,
#       size    = pointSize,
#       colour  = pointColour
#     ) +
#     labs(
#       title = plotTitle,
#       x     = plotXLab,
#       y     = plotYLab
#     ) +
#     theme(
#       plot.title        = element_text(size = 24, face = "bold", hjust = 0.5),
#       axis.title.x      = element_text(size = 16, face = "bold", vjust = -1.5),
#       axis.title.y      = element_text(size = 16, face = "bold"),
#       axis.text.x.bottom = element_text(size = 12, face = "bold"),
#       axis.text.y.left   = element_text(size = 12, face = "bold"),
#       panel.background   = element_rect(fill = "white", colour = "black"),
#       panel.grid.major   = element_blank(),
#       panel.grid.minor   = element_blank(),
#       legend.position    = "bottom",
#       legend.text        = element_text(size = 14),
#       legend.key.size    = unit(1.5, "lines")
#     )
#   
#   # -------------------------------------------------------------------------
#   # Regression Line
#   # -------------------------------------------------------------------------
#   
#   if(showRegressionLine) {
#     sPlot <- sPlot +
#       geom_smooth(
#         aes(
#           color    = "Regression Line",
#           linetype = "Regression Line"
#         ),
#         formula     = y ~ x,
#         method      = lm,
#         se          = FALSE,
#         fullrange   = TRUE,
#         linewidth   = lineWidth,
#         show.legend = TRUE
#       )
#   }
#   
#   # -------------------------------------------------------------------------
#   # Confidence Interval
#   # -------------------------------------------------------------------------
#   
#   if(showConfidenceInterval) {
#     
#     conf_int <- suppressWarnings(
#       predict(model, interval = "confidence")
#     )
#     
#     df_conf <- cbind(df, conf_int)
#     
#     sPlot <- sPlot +
#       geom_line(
#         data = df_conf,
#         aes(
#           y         = lwr,
#           color     = "Confidence Interval",
#           linetype  = "Confidence Interval"
#         ),
#         linewidth = 1.2
#       ) +
#       geom_line(
#         data = df_conf,
#         aes(
#           y         = upr,
#           color     = "Confidence Interval",
#           linetype  = "Confidence Interval"
#         ),
#         linewidth   = 1.2,
#         show.legend = FALSE
#       )
#   }
#   
#   # -------------------------------------------------------------------------
#   # Prediction Interval
#   # -------------------------------------------------------------------------
#   
#   if(showPredictionInterval) {
#     
#     pred_int <- suppressWarnings(
#       predict(model, interval = "prediction")
#     )
#     
#     df_pred <- cbind(df, pred_int)
#     
#     sPlot <- sPlot +
#       geom_line(
#         data = df_pred,
#         aes(
#           y         = lwr,
#           color     = "Prediction Interval",
#           linetype  = "Prediction Interval"
#         ),
#         linewidth = 1.2
#       ) +
#       geom_line(
#         data = df_pred,
#         aes(
#           y         = upr,
#           color     = "Prediction Interval",
#           linetype  = "Prediction Interval"
#         ),
#         linewidth   = 1.2,
#         show.legend = FALSE
#       )
#   }
#   
#   # -------------------------------------------------------------------------
#   # Dynamic Legend
#   # -------------------------------------------------------------------------
#   
#   colorValues <- c()
#   linetypeValues <- c()
#   
#   if(showRegressionLine) {
#     colorValues["Regression Line"] <- regLineColour
#     linetypeValues["Regression Line"] <- "solid"
#   }
#   
#   if(showConfidenceInterval) {
#     colorValues["Confidence Interval"] <- "darkblue"
#     linetypeValues["Confidence Interval"] <- "dashed"
#   }
#   
#   if(showPredictionInterval) {
#     colorValues["Prediction Interval"] <- "red"
#     linetypeValues["Prediction Interval"] <- "dashed"
#   }
#   
#   if(length(colorValues) > 0) {
#     sPlot <- sPlot +
#       scale_color_manual(
#         name   = NULL,
#         values = colorValues
#       ) +
#       scale_linetype_manual(
#         name   = NULL,
#         values = linetypeValues
#       )
#   }
#   
#   # -------------------------------------------------------------------------
#   # Gridlines
#   # -------------------------------------------------------------------------
#   
#   if("Major" %in% gridlines) {
#     sPlot <- sPlot +
#       theme(
#         panel.grid.major = element_line(colour = "#D9D9D9")
#       )
#   }
#   
#   if("Minor" %in% gridlines) {
#     sPlot <- sPlot +
#       theme(
#         panel.grid.minor = element_line(colour = "#D9D9D9")
#       )
#   }
#   
#   return(sPlot)
# }
