library(ggplot2)

RenderHistogram <- function(dat, plotColour, plotTitle, plotXlab, plotYlab, gridlines, flip) {
  
  hist <- ggplot(data.frame(x = dat)) +
    geom_histogram(aes(x = x),
                   bins = 15,
                   boundary = min(dat),
                   closed = "right",
                   fill = plotColour,
                   color = "black") +
    labs(title = plotTitle,
         x = plotXlab,
         y = plotYlab) +
    theme_void() +
    theme(plot.title = element_text(size = 24,
                                    face = "bold",
                                    hjust = 0.5,
                                    margin = ggplot2::margin(0,0,10,0)),
          axis.title.x = element_text(size = 16,
                                      face = "bold",
                                      vjust = -1.5,
                                      margin = ggplot2::margin(8,0,0,0)),
          axis.title.y = element_text(size = 16,
                                      face = "bold",
                                      vjust = 1.5,
                                      margin = ggplot2::margin(0,8,0,0)),
          axis.text.x.bottom = element_text(size = 14,
                                            face = "bold",
                                            margin = ggplot2::margin(8,0,0,0)),
          axis.text.y.left = element_text(size = 14,
                                          face = "bold",
                                          margin = ggplot2::margin(0,8,0,0)),
          plot.margin = unit(c(1, 1, 1, 1),"cm"),
          panel.border = element_rect(fill = NA))
  
  hist <- hist + scale_x_continuous(n.breaks = 10)
  
  if("Major" %in% gridlines) {
    hist <- hist + theme(panel.grid.major = element_line(colour = "#D9D9D9"))
  }
  
  if("Minor" %in% gridlines) {
    hist <- hist + theme(panel.grid.minor = element_line(colour = "#D9D9D9"))
  }
  
  if(flip == 1) {
    hist <- hist + coord_flip(clip = "off") +
      labs(x = plotYlab,
           y = plotXlab) +
      scale_y_continuous(n.breaks = 10, expand = c(0, 0))
  }
  
  return(hist)
}