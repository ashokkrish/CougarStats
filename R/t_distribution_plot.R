library(ggplot2)

plot_t_distribution <- function(df,
                                direction = c("left", "right", "between"),
                                x1,
                                x2 = NULL) {
  
  direction <- match.arg(direction)
  
  # Plotting range
  xmin <- -4
  xmax <- 4
  
  if (!is.null(x2)) {
    xmin <- min(xmin, x1 - 1, x2 - 1)
    xmax <- max(xmax, x1 + 1, x2 + 1)
  } else {
    xmin <- min(xmin, x1 - 1)
    xmax <- max(xmax, x1 + 1)
  }
  
  x <- seq(xmin, xmax, length.out = 1000)
  
  density <- data.frame(
    x = x,
    y = dt(x, df)
  )
  
  # Region to shade
  if (direction == "left") {
    
    shade <- subset(density, x <= x1)
    
  } else if (direction == "right") {
    
    shade <- subset(density, x >= x1)
    
  } else {
    
    shade <- subset(density, x >= x1 & x <= x2)
    
  }
  
  
  p <- ggplot(density, aes(x, y)) +
    
    # Shaded probability region
    geom_area(
      data = shade,
      fill = "#8FA8C3",
      alpha = 0.9
    ) +
    
    # t distribution curve
    geom_line(
      linewidth = 1.2,
      colour = "#0B2C53"
    ) +
    
    # Baseline
    geom_hline(
      yintercept = 0,
      linewidth = 1.2,
      colour = "#0B2C53"
    ) +
    
    # Dashed reference line at t = 0
    annotate(
      "segment",
      x = 0,
      xend = 0,
      y = 0,
      yend = dt(0, df),
      linetype = "dashed",
      linewidth = 0.8,
      colour = "grey45"
    )
  
  
  # Cutoff line(s)
  if (direction %in% c("left", "right")) {
    
    p <- p +
      annotate(
        "segment",
        x = x1,
        xend = x1,
        y = 0,
        yend = dt(x1, df),
        linewidth = 1.2,
        colour = "#0B2C53"
      )
    
  } else {
    
    p <- p +
      annotate(
        "segment",
        x = x1,
        xend = x1,
        y = 0,
        yend = dt(x1, df),
        linewidth = 1.2,
        colour = "#0B2C53"
      ) +
      annotate(
        "segment",
        x = x2,
        xend = x2,
        y = 0,
        yend = dt(x2, df),
        linewidth = 1.2,
        colour = "#0B2C53"
      )
  }
  
  
  p +
    labs(
      x = expression(italic(t)),
      y = NULL
    ) +
    coord_cartesian(expand = FALSE) +
    theme_classic(base_size = 18) +
    theme(
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      panel.grid = element_blank()
    )
}

# Example usage - Hide below lines before sourcing

# Left tail
plot_t_distribution(
  df = 30,
  direction = "left",
  x1 = 1.697
)

# Right tail
plot_t_distribution(
  df = 30,
  direction = "right",
  x1 = 1.697
)

# Area between

plot_t_distribution(
  df = 30,
  direction = "between",
  x1 = -2.101,
  x2 = 2.101
)
