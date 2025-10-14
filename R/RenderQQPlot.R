library(ggplot2)
library(ggpubr)

RenderQQPlot <- function(dat, plotColour, plotTitle, plotXlab, plotYlab, gridlines, flip) {
  qp <- ggpubr::ggqqplot(dat, x = "values",
                         title = plotTitle,
                         xlab = plotXlab,
                         ylab = plotYlab,
                         color = plotColour) %>%
    ggpar(
      font.title = c(24, "bold", "black"),
      font.x = c(16, "bold", "black"),
      font.y = c(16, "bold", "black"),
      ggtheme = theme(
        plot.title = element_text(hjust = 0.5,
                                  margin = margin(0,0,10,0)),
        axis.text.x.bottom = element_text(size = 16,
                                          margin = margin(5,0,5,0)),
        axis.text.y.left = element_text(size = 16,
                                        margin = margin(0,5,0,5)),
        panel.border = element_rect(fill=NA))
    )
  
  if("Major" %in% gridlines) {
    qp <- ggpar(qp,
                ggtheme = theme(
                  panel.grid.major = element_line(colour = "#D9D9D9")))
  }
  
  if("Minor" %in% gridlines) {
    qp <- ggpar(qp,
                ggtheme = theme(
                  panel.grid.minor = element_line(colour = "#D9D9D9")))
  }
  
  if(flip == 1) {
    qp <- ggpar(qp,
                orientation = "horiz",
                xlab = plotYlab,
                ylab = plotXlab)
  }

  return(qp)
}


RenderWilcoxQQPlots <- function(sample1, sample2, plotColour, plotTitle, plotXlab, plotYlab, gridlines, flip) {

  df1 <- data.frame(values = sample1)
  df2 <- data.frame(values = sample2)

  qp1 <- ggpubr::ggqqplot(df1, x = "values",
                          title = "Sample 1 Q-Q Plot",
                          xlab = plotXlab,
                          ylab = plotYlab,
                          color = plotColour) %>%
    ggpar(
      font.title = c(20, "bold", "black"),
      font.x = c(14, "bold", "black"),
      font.y = c(14, "bold", "black"),
      ggtheme = theme(
        plot.title = element_text(hjust = 0.5,
                                  margin = margin(0,0,10,0)),
        axis.text.x.bottom = element_text(size = 14,
                                          margin = margin(5,0,5,0)),
        axis.text.y.left = element_text(size = 14,
                                        margin = margin(0,5,0,5)),
        panel.border = element_rect(fill=NA))
    )

  qp2 <- ggpubr::ggqqplot(df2, x = "values",
                          title = "Sample 2 Q-Q Plot",
                          xlab = plotXlab,
                          ylab = plotYlab,
                          color = plotColour) %>%
    ggpar(
      font.title = c(20, "bold", "black"),
      font.x = c(14, "bold", "black"),
      font.y = c(14, "bold", "black"),
      ggtheme = theme(
        plot.title = element_text(hjust = 0.5,
                                  margin = margin(0,0,10,0)),
        axis.text.x.bottom = element_text(size = 14,
                                          margin = margin(5,0,5,0)),
        axis.text.y.left = element_text(size = 14,
                                        margin = margin(0,5,0,5)),
        panel.border = element_rect(fill=NA))
    )

  if("Major" %in% gridlines) {
    qp1 <- ggpar(qp1, ggtheme = theme(panel.grid.major = element_line(colour = "#D9D9D9")))
    qp2 <- ggpar(qp2, ggtheme = theme(panel.grid.major = element_line(colour = "#D9D9D9")))
  }
  
  if("Minor" %in% gridlines) {
    qp1 <- ggpar(qp1, ggtheme = theme(panel.grid.minor = element_line(colour = "#D9D9D9")))
    qp2 <- ggpar(qp2, ggtheme = theme(panel.grid.minor = element_line(colour = "#D9D9D9")))
  }

  if(flip == 1) {
    qp1 <- ggpar(qp1, orientation = "horiz", xlab = plotYlab, ylab = plotXlab)
    qp2 <- ggpar(qp2, orientation = "horiz", xlab = plotYlab, ylab = plotXlab)
  }

  combined_plot <- gridExtra::grid.arrange(qp1, qp2, ncol = 2, 
                                           top = grid::textGrob(plotTitle, 
                                                                gp = grid::gpar(fontsize = 24, fontface = "bold")))
  
  return(combined_plot)
}

RenderSignedRankQQPlot <- function(dat, plotColour, plotTitle, plotXlab, plotYlab, gridlines, flip) {
  qp <- ggpubr::ggqqplot(dat, x = "values",
                         title = plotTitle,
                         xlab = plotXlab,
                         ylab = plotYlab,
                         color = plotColour) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 24, face = "bold",
                                margin = margin(0,0,10,0)),
      axis.title.x = element_text(size = 16, face = "bold"),
      axis.title.y = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(size = 16, margin = margin(5,0,5,0)),
      axis.text.y = element_text(size = 16, margin = margin(0,5,0,5)),
      panel.border = element_rect(fill = NA, colour = "black"),
      panel.grid.major = if("Major" %in% gridlines) element_line(colour = "#D9D9D9") else element_blank(),
      panel.grid.minor = if("Minor" %in% gridlines) element_line(colour = "#D9D9D9") else element_blank()
    )
  
  if(flip == 1) {
    qp <- qp + coord_flip() +
      labs(x = plotYlab, y = plotXlab)
  }
  
  return(qp)
}