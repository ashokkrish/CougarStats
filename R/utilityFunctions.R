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