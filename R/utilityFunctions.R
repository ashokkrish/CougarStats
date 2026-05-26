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