library(htmltools)
library(shiny)
library(shinyWidgets)

# ================================================================ #
# Description 
# -----------
# Module for implementing a dropdown menu of plot customization 
# options. 
#
# ================================================================ #
# Example Usage 
# -----
# in ui.R:
#   plotOptionsMenuUI(id = "boxplotMenu", plotType = "Boxplot", title = "Plot",
#                     xlab = "X", ylab = "Y", colour = "#5493DD")
#
# in server.R
#   plotOptionsMenuServer("boxplotMenu")
#
# ================================================================ #
# UI Arguments 
# ------------
# id:       The id of the namespace. See ?NS for more information.
#
# plotType: The type of plot being customized. 
#           One of (Null, Boxplot). Use NULL for standard options
#           only.
#
# title:    The title of the plot.
#
# xlab:     The default value for the X-Axis label.
#
# ylab:     The default value for Y-Axis label.
#
# colour:   The primary colour for plot customization
#
# dim:      The setting for height/width calculation Default is "auto".
#
# includeGridlines:
#           Option for including major and minor gridline toggles. Included
#           by default.
# 
# includeFlip:
#           Option for include plot orientation toggle. Included by default.
#
# FUTURE WORK:
# includeOutlierLabels:
#           Option for including labels above the outlier data points.
#           Unchecked by default.
#
#
# Server Arguments 
# ----------------
# id:       The id of the namespace. See ?NS for more information.
#
# ================================================================ #
# Customization Options
# ---------------------
# Standard options: 
#   - plot title 
#   - axis labels 
#   - plot colour 
#   - height 
#   - width 
#   - adding gridlines 
#   - orientation
#
# Boxplot options:
#   - box widths
#
# Scatterplot options:
#   - plot points colour
#   - plot line width
#   - plot points size
#
# ================================================================ #
plotOptionsMenuUI <- function(id, plotType = NULL, title = "Plot", xlab = "", ylab = "", colour = "#7293AD",
                              dim = "auto", includeGridlines = TRUE, includeFlip = TRUE, includeOutlierLabels = FALSE,
                              regressionLineLabel = "Show Regression Line", includeLinearLine = FALSE,
                              includeMeansOption = FALSE) {
  ns <- NS(id)

  flip <- addFlipCheckbox(includeFlip, ns, plotType)
  grid <- addGridlines(includeGridlines, ns)
  extraOptions <- tagList()

  if(!is.null(plotType)) {
    extraOptions <- switch(
      plotType,
      "Histogram" = HistogramOptions(ns),
      "Boxplot" = BoxplotOptions(ns),
      "Scatterplot" = ScatterplotOptions(ns, regressionLineLabel, includeLinearLine, includeMeansOption, colour)
    )
  }
  
  menu <- tagList(
    dropdown(
      tags$h3("Plot Options"),
      
      textInput(
        inputId = ns("Title"), 
        label = strong("Main title and axes labels:"), 
        value = title, 
        placeholder = "main title"
      ),
      
      textInput(
        inputId = ns("Xlab"), 
        label = NULL, 
        value = xlab, 
        placeholder = "x-axis label"
      ),
      
      textInput(
        inputId = ns("Ylab"),
        label = NULL,
        value = ylab,
        placeholder = "y-axis label"
      ),

      if (!is.null(plotType) && plotType == "Scatterplot") checkboxInput(
        inputId = ns("Gridlines"),
        label   = strong("Add Gridlines"),
        value   = FALSE
      ),

    # FUTURE WORK: Outlier Labels to be added to renderBoxplot functions
    #  if (!is.null(plotType) && plotType == "Boxplot") {
    #    checkboxInput(
    #      inputId = ns("OutlierLabels"),
    #      label   = "Display outlier labels",
    #      value   = FALSE
    #    )
    #  },
      
      if (is.null(plotType) || plotType != "Scatterplot") colourpicker::colourInput(
        inputId = ns("Colour"),
        label   = strong("Plot Colour"),
        value   = colour
      ),
      
      if (is.null(plotType) || plotType != "Scatterplot") tagList(
        radioButtons(
          inputId = ns("Height"),
          label = strong("Plot Height"),
          choices = c("auto", "in px"),
          selected = dim,
          inline = TRUE
        ),

        conditionalPanel(
          ns = ns,
          condition = "input.Height == 'in px'",
          numericInput(
            inputId = ns("HeightPx"),
            label = NULL,
            value = 400,
            min = 100,
            max = 1500,
            step = 1
          )
        ),

        radioButtons(
          inputId = ns("Width"),
          label = strong("Plot Width"),
          choices = c("auto", "in px"),
          selected = dim,
          inline = TRUE
        ),

        conditionalPanel(
          ns = ns,
          condition = "input.Width == 'in px'",
          numericInput(
            inputId = ns("WidthPx"),
            label = NULL,
            value = 750,
            min = 100,
            max = 1500,
            step = 1
          )
        )
      ),
      
      grid,      
      flip,
      extraOptions,
      
      style = "jelly", 
      icon = icon("gear"),
      status = "primary", 
      width = "300px",
      animate = animateOptions(
        enter = animations$fading_entrances$fadeInDown,
        exit = animations$fading_exits$fadeOutUp)
    )
  )
}

addFlipCheckbox <- function(includeFlip, ns, plotType) {
  flip <- tagList()
  
  if(includeFlip && (is.null(plotType) || plotType != "Histogram")) {
    flip <- tagList(
      p(strong("Orientation")),
      checkboxInput(
        inputId = ns("Flip"),
        label = "Plot Horizontally",
        if(!is.null(plotType) && plotType == "Boxplot") {
          value = FALSE # set default to false FOR NOW
        } else {
          value = FALSE
        },
      )
    )
  }
}

addGridlines <- function(includeGridlines, ns) {
  grid <- tagList()

  if(includeGridlines){
    grid <- tagList(
      checkboxGroupInput(
        inputId = ns("Gridlines"),
        label = strong("Add Gridlines"),
        choices = c("Major", "Minor"),
        selected = NULL,
        inline = TRUE
      )
    )
  }
}

HistogramOptions <- function(ns) {
  tagList(
    tags$h3("Histogram Options"),
    
    checkboxInput(
      inputId = ns("Density"),
      label = "Kernel Density Estimation Curve",
      value = FALSE
    )
  )
}

BoxplotOptions <- function(ns) {
  tagList(
    tags$h3("Boxplot Options"),
    
    sliderInput(
      inputId = ns("BoxWidth"),
      label = strong("Box Width"),
      min = 1,
      max = 10,
      value = 5,
      step = 1
    )
  )
}

ScatterplotOptions <- function(ns, regressionLineLabel = "Show Regression Line", includeLinearLine = FALSE, includeMeansOption = FALSE, colour = "#7293AD") {

  tagList(
    tags$h3("Line Options"),

    colourpicker::colourInput(
      inputId = ns("PointsColour"),
      label = strong("Plot Points Colour"),
      value = "#000000"
    ),

    sliderInput(
      inputId = ns("PointSize"),
      label = strong("Point Size"),
      min = 1,
      max = 10,
      value = 3,
      step = 1
    ),

    checkboxInput(
      inputId = ns("showRegressionLine"),
      label   = regressionLineLabel,
      value   = TRUE
    ),

    conditionalPanel(
      ns = ns,
      condition = "input.showRegressionLine",
      colourpicker::colourInput(
        inputId = ns("Colour"),
        label   = strong("Regression Line Colour"),
        value   = colour
      ),
      sliderInput(
        inputId = ns("RegLineOpacity"),
        label   = strong("Regression Line Opacity"),
        min = 0, max = 100, value = 100, step = 5, post = "%"
      ),
      sliderInput(
        inputId = ns("RegLineWidth"),
        label   = strong("Regression Line Width"),
        min = 1, max = 10, value = 1, step = 1
      )
    ),

    if (includeLinearLine) checkboxInput(
      inputId = ns("showLinearLine"),
      label   = "Show Linear Regression Line",
      value   = FALSE
    ),

    checkboxInput(
      inputId = ns("confidenceInterval"),
      label = "Confidence Band for the Mean Response",
      value = FALSE
    ),

    conditionalPanel(
      ns = ns,
      condition = "input.confidenceInterval",
      colourpicker::colourInput(
        inputId = ns("ConfidenceBandColour"),
        label   = strong("Confidence Band Colour"),
        value   = "darkblue"
      ),
      sliderInput(
        inputId = ns("ConfidenceBandOpacity"),
        label   = strong("Confidence Band Opacity"),
        min = 0, max = 100, value = 100, step = 5, post = "%"
      ),
      sliderInput(
        inputId = ns("ConfidenceBandWidth"),
        label   = strong("Confidence Band Width"),
        min = 1, max = 10, value = 1, step = 1
      )
    ),

    checkboxInput(
      inputId = ns("predictionInterval"),
      label = "Prediction Band",
      value = FALSE
    ),

    conditionalPanel(
      ns = ns,
      condition = "input.predictionInterval",
      colourpicker::colourInput(
        inputId = ns("PredictionBandColour"),
        label   = strong("Prediction Band Colour"),
        value   = "purple"
      ),
      sliderInput(
        inputId = ns("PredictionBandOpacity"),
        label   = strong("Prediction Band Opacity"),
        min = 0, max = 100, value = 100, step = 5, post = "%"
      ),
      sliderInput(
        inputId = ns("PredictionBandWidth"),
        label   = strong("Prediction Band Width"),
        min = 1, max = 10, value = 1, step = 1
      )
    ),

    if (includeMeansOption) tagList(
      checkboxInput(
        inputId = ns("showMeans"),
        label   = "Show x̅ and y̅ (Means)",
        value   = FALSE
      ),
      conditionalPanel(
        ns = ns,
        condition = "input.showMeans",
        colourpicker::colourInput(
          inputId = ns("MeansColour"),
          label   = strong("Means Colour"),
          value   = "#FF0000"
        ),
        sliderInput(
          inputId = ns("MeansOpacity"),
          label   = strong("Means Opacity"),
          min = 0, max = 100, value = 50, step = 5,
          post = "%"
        ),
        sliderInput(
          inputId = ns("MeansLineWidth"),
          label   = strong("Line Thickness"),
          min = 1, max = 10, value = 2, step = 1
        ),
        sliderInput(
          inputId = ns("MeansMarkerOpacity"),
          label   = strong("Marker Opacity"),
          min = 0, max = 100, value = 50, step = 5, post = "%"
        ),
        sliderInput(
          inputId = ns("MeansMarkerSize"),
          label   = strong("Marker Size"),
          min = 5, max = 40, value = 18, step = 1
        ),
        selectInput(
          inputId  = ns("MeansMarkerShape"),
          label    = strong("Marker Shape"),
          choices  = c(
            "Asterisk"        = "asterisk-open",
            "Circle"          = "circle",
            "Circle (open)"   = "circle-open",
            "Cross"           = "cross-open",
            "Diamond"         = "diamond",
            "Diamond (open)"  = "diamond-open",
            "Square"          = "square",
            "Square (open)"   = "square-open",
            "Star"            = "star",
            "Star (open)"     = "star-open",
            "Triangle"        = "triangle-up",
            "Triangle (open)" = "triangle-up-open",
            "X"               = "x-open"
          ),
          selected = "asterisk-open"
        )
      )
    )
  )
}

plotOptionsMenuServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    #Swap the axis labels
    observeEvent(input$Flip, {
      if(!is.null(input$Xlab) && !is.null(input$Ylab)){
        xlab <- input$Xlab
        
        updateTextInput(
          inputId = "Xlab",
          value = input$Ylab
        )
        
        updateTextInput(
          inputId = "Ylab",
          value = xlab
        )
      } 
      
    }, ignoreInit = TRUE)

  })
}
