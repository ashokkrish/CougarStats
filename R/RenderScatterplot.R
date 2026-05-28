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
      # Hover tooltip
      hovertemplate = paste0(
        "<b>", plotXLab, ":</b> %{x}<br>",
        "<b>", plotYLab, ":</b> %{y}<br>",
        "<extra></extra>"
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
    
    conf_int <- suppressWarnings(
      predict(model, interval = "confidence")
    )
    
    df_conf <- cbind(df, conf_int)
    
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
    
    pred_int <- suppressWarnings(
      predict(model, interval = "prediction")
    )
    
    df_pred <- cbind(df, pred_int)
    
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