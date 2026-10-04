# custom_box_stats() (the Tukey box statistics helper) lives in RenderBoxplot.R.
RenderSideBySideBoxplot <- function(dat, df_boxplot, plotColour, plotTitle, plotXlab, plotYlab, 
                                    boxWidth, gridlines, flip, showLabels = TRUE) {
  # determine samples dynamically
  sample_levels <- unique(df_boxplot$sample)
  n_samples <- length(sample_levels)
  
  # compute stats for each sample (the values are split by sample once, rather
  # than comparing the whole vector with every sample, which was quadratic in
  # the number of samples)
  sample_values <- split(dat, factor(match(df_boxplot$sample, sample_levels),
                                     levels = seq_len(n_samples)))
  stats_list <- lapply(sample_values, custom_box_stats)
  stats <- dplyr::bind_rows(stats_list)
  stats$x <- sample_levels
  
  # collect outliers
  outlier_values <- lapply(stats_list, function(st) st$outliers[[1]])
  df_outliers <- tibble::tibble(
    x = rep(sample_levels, lengths(outlier_values)),
    y = unlist(outlier_values, use.names = FALSE)
  )
  
  # base plot
  bp <- ggplot() +
    geom_boxplot(
      data = stats,
      aes(
        x = factor(x, levels = sample_levels),
        ymin = ymin,
        lower = lower,
        middle = middle,
        upper = upper,
        ymax = ymax
      ),
      stat = "identity",
      width = boxWidth,
      fill = plotColour,
      outlier.shape = NA
    ) +
    geom_point(
      data = df_outliers,
      aes(x = factor(x, levels = sample_levels), y = y),
      size = 2
    ) +
    labs(title = plotTitle, x = plotXlab, y = plotYlab) +
    theme_void() +
    theme(
      plot.title = element_text(size = 24, face = "bold", hjust = 0.5, margin = ggplot2::margin(0,0,5,0)),
      axis.title.x = element_text(size = 16, face = "bold", vjust = -1.5, margin = ggplot2::margin(5,0,0,0)),
      axis.title.y = element_text(size = 16, face = "bold", margin = ggplot2::margin(0,5,0,0)),
      axis.text.x.bottom = element_text(size = 16, face = "bold"),
      axis.text.y.left = element_text(size = 16, face = "bold"),
      plot.margin = unit(c(1,1,1,1), "cm"),
      axis.line = element_line(),
    ) +
    scale_y_continuous(n.breaks = 10)
  
  # manually plot whisker caps (one layer for all lower caps and one for all upper
  # caps, instead of two layers per sample)
  cap_data <- data.frame(
    xmin = seq_along(stats_list) - 0.05,
    xmax = seq_along(stats_list) + 0.05,
    ymin = stats$ymin,
    ymax = stats$ymax
  )
  
  bp <- bp +
    geom_segment(data = cap_data,
                 aes(x = xmin, xend = xmax, y = ymin, yend = ymin)) +
    geom_segment(data = cap_data,
                 aes(x = xmin, xend = xmax, y = ymax, yend = ymax))
  
  # gridlines
  if("Major" %in% gridlines) bp <- bp + theme(panel.grid.major = element_line(colour = "#D9D9D9"))
  if("Minor" %in% gridlines) bp <- bp + theme(panel.grid.minor = element_line(colour = "#D9D9D9"))
  
  # flip
  if(isTRUE(flip == 1)){
    bp <- bp + coord_flip(clip = "off") +
      theme(
        axis.text.x.bottom = element_text(size = 16, face = "bold"),
        axis.text.y.left = element_text(size = 16, face = "bold")
      ) +
      labs(x = plotYlab, y = plotXlab)
  }
  
  return(bp)
}
