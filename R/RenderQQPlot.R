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