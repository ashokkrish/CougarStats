library(ggplot2)
library(dplyr)

RenderSideBySideBoxplot <- function(dat, df_boxplot, plotColour, plotTitle, plotXlab, plotYlab, 
                                    boxWidth, gridlines, flip, showLabels = TRUE) {
  # determine samples dynamically
  sample_levels <- unique(df_boxplot$sample)
  n_samples <- length(sample_levels)
  
  # compute stats for each sample
  stats_list <- lapply(sample_levels, function(s) {
    custom_box_stats(dat[df_boxplot$sample == s]) %>% mutate(x = s)
  })
  stats <- bind_rows(stats_list)
  
  # collect outliers
  df_outliers <- do.call(rbind, lapply(seq_along(sample_levels), function(i) {
    s <- sample_levels[i]
    tibble(
      x = s,
      y = stats_list[[i]]$outliers[[1]]
    )
  }))
  
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
      plot.title = element_text(size = 24, face = "bold", hjust = 0.5, margin = margin(0,0,5,0)),
      axis.title.x = element_text(size = 16, face = "bold", vjust = -1.5, margin = margin(5,0,0,0)),
      axis.title.y = element_text(size = 16, face = "bold", margin = margin(0,5,0,0)),
      axis.text.x.bottom = element_text(size = 16),
      axis.text.y.left = element_text(size = 16),
      plot.margin = unit(c(1,1,1,1), "cm"),
      panel.border = element_rect(fill = NA)
    ) +
    scale_y_continuous(n.breaks = 10)
  
  # manually plot whisker caps
  for(i in seq_along(stats_list)) {
    s <- sample_levels[i]
    bp <- bp +
      geom_segment(aes(x = i - 0.05, xend = i + 0.05, y = stats_list[[i]]$ymin, yend = stats_list[[i]]$ymin)) +
      geom_segment(aes(x = i - 0.05, xend = i + 0.05, y = stats_list[[i]]$ymax, yend = stats_list[[i]]$ymax))
  }
  
  # gridlines
  if("Major" %in% gridlines) bp <- bp + theme(panel.grid.major = element_line(colour = "#D9D9D9"))
  if("Minor" %in% gridlines) bp <- bp + theme(panel.grid.minor = element_line(colour = "#D9D9D9"))
  
  # flip
  if(flip == 1){
    bp <- bp + coord_flip(clip = "off") +
      theme(axis.text.x.bottom = element_text(size = 16),
            axis.text.y.left = element_blank()) +
      labs(x = plotYlab, y = plotXlab)
  }
  
  return(bp)
}

custom_box_stats <- function(x, coef = 1.5) {
  x <- sort(x)
  
  # tukey quartile calculation
  quartiles <- list()
  if(length(x) %% 2 != 0) {
    x_no_median <- x[-ceiling(length(x)/2)]
  } else {
    x_no_median <- x
  }
  mid <- length(x_no_median) / 2
  Q1 <- median(x_no_median[1:mid])
  Q2 <- median(x)
  Q3 <- median(x_no_median[(mid+1):length(x_no_median)])
  
  IQR <- Q3 - Q1
  lower_fence <- Q1 - coef * IQR
  upper_fence <- Q3 + coef * IQR
  
  whisker_low <- min(x[x >= lower_fence])
  whisker_high <- max(x[x <= upper_fence])
  
  outliers <- x[x < lower_fence | x > upper_fence]
  
  tibble(
    x = 1,
    ymin = whisker_low,
    lower = Q1,
    middle = Q2,
    upper = Q3,
    ymax = whisker_high,
    outliers = list(outliers)
  )
}
