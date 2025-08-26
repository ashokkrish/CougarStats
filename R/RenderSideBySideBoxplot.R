library(ggplot2)

RenderSideBySideBoxplot <- function(dat, df_boxplot, df_outliers, plotColour, plotTitle, plotXlab, plotYlab, boxWidth, gridlines, flip, showLabels = TRUE) {
  
  df_outliers <- getSideBySideOutliers(
    df_boxplot$data[df_boxplot$sample == "Sample 1"],
    df_boxplot$data[df_boxplot$sample == "Sample 2"]
  )
  
  label <- ""
  
  if(showLabels && nrow(df_outliers) > 0) {
    # count duplicates of outliers per group and add count to labels if > 1
    df_outliers <- df_outliers %>%
      group_by(sample, data) %>%
      summarise(count = n(), .groups = 'drop') %>%
      mutate(label = ifelse(count > 1, paste0(data, " (", count, ")"), as.character(data)))
  }
  
  bp <- ggplot(df_boxplot, aes(x = data, y = sample)) +
    stat_boxplot(geom = 'errorbar', width = 0.15) +
    geom_boxplot(width = boxWidth,
                 fill = plotColour,
                 alpha = 1) + # this turns automatic outlier calculations off
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

getSideBySideOutliers <- function(sample1, sample2, coef = 1.5) {
  
  calc_outliers <- function(x, sample_name) {
    Q <- quantile(x, probs = c(0.25, 0.75), type = 7, na.rm = TRUE)
    IQR <- Q[2] - Q[1]
    lower_fence <- Q[1] - coef * IQR
    upper_fence <- Q[2] + coef * IQR
    outliers <- x[x < lower_fence | x > upper_fence]
    
    if(length(outliers) == 0) {
      return(data.frame(sample = character(0), data = numeric(0)))
    } else {
      return(data.frame(sample = sample_name, data = outliers))
    }
  }
  
  df_outliers <- rbind(
    calc_outliers(sample1, "Sample 1"),
    calc_outliers(sample2, "Sample 2")
  )
  
  return(df_outliers)
}