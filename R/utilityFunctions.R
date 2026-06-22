## Format a number for LaTeX: uses value^{exp} notation when the value would
## round to zero at 'digits' decimal places, so fractions never display 0/0.
fmt_sci_latex <- function(x, digits = 3) {
  if (!is.finite(x) || x == 0) return(sprintf("%.*f", digits, x))
  if (abs(x) < 0.5 * 10^(-digits)) {
    exp  <- floor(log10(abs(x)))
    mant <- x / 10^exp
    sprintf("%.3f^{%d}", mant, exp)
  } else {
    format(round(x, digits), nsmall = 0, scientific = FALSE)
  }
}

## Format a p-value as a LaTeX relational string for use inside \( ... \)
## Returns "= 0.0234" for ordinary values or "< 0.0001" when below eps.
pval_tex <- function(p, eps = 0.0001) {
  if (p < eps) paste0("< ", format(eps, scientific = FALSE))
  else sprintf("= %0.4f", p)
}

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

hypZTestPlot <- function(testStatistic, critValue, altHypothesis) {
  zTail <- qnorm(0.999, lower.tail = FALSE)
  zHead <- qnorm(0.999, lower.tail = TRUE)
  x     <- round(seq(from = zTail, to = zHead, by = 0.1), 2)

  if (altHypothesis == "two.sided") {
    CVs      <- c(-critValue, critValue)
    RRLabels <- c((-critValue + zTail) / 2, (critValue + zHead) / 2)
  } else {
    CVs      <- c(critValue)
    RRLabels <- if (altHypothesis == "less") c((critValue + zTail) / 2) else c((critValue + zHead) / 2)
  }

  xSeq       <- unique(sort(c(x, testStatistic, CVs, RRLabels, 0)))
  df         <- data.frame(x = xSeq, y = dnorm(xSeq))
  cvDF       <- filter(df, x %in% CVs)
  RRLabelsDF <- filter(df, x %in% RRLabels)
  tsDF       <- filter(df, x %in% testStatistic)

  htPlot <- ggplot(df, aes(x = x, y = y))

  if (altHypothesis == "two.sided") {
    htPlot <- htPlot + shadeHtArea(df, -critValue, "less") +
      shadeHtArea(df, critValue, "greater")
  } else {
    htPlot <- htPlot + shadeHtArea(df, critValue, altHypothesis)
  }

  htPlot +
    stat_function(fun = dnorm, geom = "density", fill = NA) +
    theme_void() +
    scale_y_continuous(breaks = NULL) +
    ylab("") +
    xlab("z") +
    geom_segment(data = filter(df, x %in% 0),
                 aes(x = x, xend = x, y = 0, yend = y),
                 linetype = "dotted", linewidth = 0.75, color = "black") +
    geom_text(data = filter(df, x %in% 0),
              aes(x = x, y = y / 2, label = "A R"),
              size = 16 / .pt, fontface = "bold") +
    geom_text(data = filter(df, x %in% 0),
              aes(x = x, y = 0, label = "0"),
              size = 14 / .pt, fontface = "bold", nudge_y = -.03) +
    geom_segment(data = tsDF,
                 aes(x = x, xend = x, y = 0, yend = y + .03),
                 linetype = "solid", linewidth = 1.25, color = "#BD130B") +
    geom_text(data = tsDF,
              aes(x = x, y = y, label = x),
              size = 16 / .pt, fontface = "bold", nudge_y = .075) +
    geom_segment(data = cvDF,
                 aes(x = x, xend = x, y = 0, yend = y),
                 linetype = "solid", lineend = "butt", linewidth = 1.5, color = "#023B70") +
    geom_text(data = cvDF,
              aes(x = x, y = 0, label = x),
              size = 14 / .pt, fontface = "bold", nudge_y = -.03) +
    geom_text(data = RRLabelsDF,
              aes(x = x, y = y, label = "RR"),
              size = 16 / .pt, fontface = "bold", nudge_y = .03) +
    theme(axis.title.x = element_text(size = 16, face = "bold.italic"))
}


