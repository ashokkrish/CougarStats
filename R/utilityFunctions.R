## String List to Numeric List
createNumLst <- function(text) {
  text <- gsub("[^0-9.,-]","", text) #purge non-numeric characters
  text <- gsub("^,", "", text)      #purge any leading commas
  text <- gsub(",(,)+", ",", text)  #transform multiple consecutive commas into a single comma
  text <- gsub(",$", "", text)      #purge any trailing commas
  split <- strsplit(text, ",", fixed = FALSE)[[1]]
  suppressWarnings(na.omit(as.numeric(split)))
}

GetPlotHeight  <- function(plotToggle, pxValue, ui) {

  ifelse(plotToggle == 'in px' && !is.na(pxValue),
         height <- pxValue,
         height <- 400)

  ifelse(ui,
         return(paste0(height, "px")),
         return(height))
}

GetPlotWidth  <- function(plotToggle, pxValue, ui) {

  if(plotToggle == 'in px' && !is.na(pxValue)) {
    width <- pxValue

    if(ui) {
      width <- paste0(width, "px")
    }
  } else {
    width <- "auto"
  }

  return(width)
}

`%then%` <- function(a, b) {
  if (is.null(a)) b else a
}

copyButton <- function(id, ns) {
  tags$button(
    class = "copy-btn",
    type = "button",
    onclick = sprintf(
      "   navigator.clipboard.writeText(document.getElementById('%s').innerText.trim());
          var btn = this;
          var old = btn.innerText;
          btn.innerText = '✓ Copied';
          setTimeout(function(){
            btn.innerText = old;
          }, 1000);",
      ns(id)
    ),
    "Copy"
  )
}

codeBox <- function(title = "R Code", boxId, outputId, ns) {
  
  div(
    id = ns(paste0(boxId, "Wrapper")),
    class = "code-container",

    div(
      class = "code-header",
      div(title),
      copyButton(boxId, ns)
    ),

    div(
      id = ns(boxId),
      class = "code-body",
      htmlOutput(ns(outputId))
    )
  )
}

codeValue <- function(x) {
  paste0('<span class="code-value">', as.character(x), '</span>')
}

# SHADED AREA FUNCTION for SLR outputs

shadeHtArea <- function(df, critValue, altHypothesis) {
  
  if(altHypothesis == 'less') {
    geom_area(data = subset(df, x <= critValue),
              aes(y=y),
              fill = "#023B70",
              color = NA,
              alpha = 0.4)
    
    
  } else if (altHypothesis == 'greater') {
    geom_area(data = subset(df, x >= critValue),
              aes(y=y),
              fill = "#023B70",
              color = NA,
              alpha = 0.4)
  }
}



# TTest Plot FUNCTION for SLR outputs

hypTTestPlot <- function(testStatistic, degfree, critValue, altHypothesis){
  tTail = qt(0.999, df = degfree, lower.tail = FALSE)
  tHead = qt(0.999, df = degfree, lower.tail = TRUE)
  x <- round(seq(from = tTail, to = tHead, by = 0.1), 2)
  
  if(altHypothesis == "two.sided") {
    CVs <- c(-critValue, critValue)
    RRLabels <- c((-critValue + tTail)/2, (critValue + tHead)/2)
  } else{
    CVs <- c(critValue)
    if(altHypothesis == 'less') {
      RRLabels <- c((critValue + tTail)/2)
    } else {
      RRLabels <- c((critValue + tHead)/2)
    }
  }
  
  xSeq <- unique(sort(c(x, testStatistic, CVs, RRLabels, 0)))
  
  df <- data.frame(x = xSeq, y = dt(xSeq, degfree))
  cvDF <- filter(df, x %in% CVs)
  RRLabelsDF <- filter(df, x %in% RRLabels)
  tsDF <- filter(df, x %in% testStatistic)
  centerDF <- filter(df, x %in% c(0))
  
  htPlot <- ggplot(df, aes(x = x, y = y))
  
  if(altHypothesis == 'two.sided') {
    htPlot <- htPlot + shadeHtArea(df, -critValue, "less") +
      shadeHtArea(df, critValue, "greater")
  } else {
    htPlot <- htPlot + shadeHtArea(df, critValue, altHypothesis)
  }
  
  htPlot <- htPlot + stat_function(fun = dt,
                                   args = list(df = degfree),
                                   geom = "density",
                                   fill = NA) +
    theme_void()  +
    scale_y_continuous(breaks = NULL) +
    ylab("") +
    xlab("t") +
    geom_segment(data = filter(df, x %in% c(0)),
                 aes(x = x, xend = x, y = 0, yend = y),
                 linetype = "dotted",
                 linewidth = 0.75,
                 color='black') +
    geom_text(data = filter(df, x %in% c(0)),
              aes(x = x, y = y/2, label = "A R"),
              size = 16 / .pt,
              fontface = "bold") +
    geom_text(data = filter(df, x %in% c(0)),
              aes(x = x, y = 0, label = "0"),
              size = 14 / .pt,
              fontface = "bold",
              nudge_y = -.03) +
    geom_segment(data = tsDF,
                 aes(x = x, xend = x, y = 0, yend = y + .03),
                 linetype = "solid",
                 linewidth = 1.25,
                 color='#BD130B') +
    geom_text(data = tsDF,
              aes(x = x, y = y, label = x),
              size = 16 / .pt,
              fontface = "bold",
              nudge_y = .075) +
    geom_segment(data = cvDF,
                 aes(x = x, xend = x, y = 0, yend = y),
                 linetype = "solid",
                 lineend = 'butt',
                 linewidth = 1.5,
                 color='#023B70') +
    geom_text(data = cvDF,
              aes(x = x, y = 0, label = x),
              size = 14 / .pt,
              fontface = "bold",
              nudge_y = -.03) +
    geom_text(data = RRLabelsDF,
              aes(x = x, y = y, label = "RR"),
              size = 16 / .pt,
              fontface = "bold",
              nudge_y = .03) +
    theme(axis.title.x = element_text(size = 16,
                                      face = "bold.italic"))
  
  return(htPlot)
}
