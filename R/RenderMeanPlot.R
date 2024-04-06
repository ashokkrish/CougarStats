library(ggplot2)


RenderMeanPlot <- function(dat, groups, plotColour, plotTitle, plotXlab, plotYlab, gridlines, flip) {
  
  data_plot = dat %>% 
    group_by(ind) %>% 
    summarise(m=mean(values), #calculate the summaries you want on the plot
              n_dat=n(),
              sem=sd(values)/sqrt(n()), 
              ci_low=m-1.96*sem, 
              ci_hi=m+1.96*sem) %>% 
    ungroup()
  
  gmp <- ggplot(data_plot, aes(x=ind, y=m, ymin=ci_low, ymax=ci_hi)) +
    geom_line(aes(group = 1), size = 1) +
    geom_errorbar(width = 0.2, color = plotColour) + 
    geom_point(size=2) +
    labs(title = plotTitle,
         x = plotXlab,
         y = plotYlab) +
    theme_void() +
    theme(plot.title = element_text(size = 24,
                                    face = "bold",
                                    hjust = 0.5,
                                    margin = margin(0,0,10,0)),
          axis.title.x = element_text(size = 18, 
                                      face = "bold", 
                                      vjust = -1.5,
                                      margin = margin(5,0,0,0)),
          axis.title.y = element_text(size = 18,
                                      colour = "black",
                                      face = "bold", 
                                      angle = 90,
                                      margin = margin(0,5,0,0)),
          axis.text.x.bottom = element_text(size = 14,
                                            margin = margin(5,0,0,0)),
          axis.text.y.left = element_text(size = 14,
                                          margin = margin(0,5,0,0)),
          plot.margin = unit(c(1, 1, 1, 1),"cm"),
          panel.border = element_rect(fill=NA)) 
  
  if("Major" %in% gridlines) {
    gmp <- gmp + theme(panel.grid.major = element_line(colour = "#D9D9D9"))
  }
  
  if("Minor" %in% gridlines) {
    gmp <- gmp + theme(panel.grid.minor = element_line(colour = "#D9D9D9"))
  }
  
  if(flip == 1) {
    gmp <- gmp + coord_flip(clip="off") +
      labs(x = plotYlab,
           y = plotXlab) 
  }
  
  return(gmp)
}