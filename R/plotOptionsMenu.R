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
                              dim = "auto", includeGridlines = TRUE, includeFlip = TRUE, includeOutlierLabels = FALSE) {
  ns <- NS(id)
  
  flip <- addFlipCheckbox(includeFlip, ns)
  grid <- addGridlines(includeGridlines, ns)
  extraOptions <- tagList()
  
  if(!is.null(plotType)) {
    extraOptions <- switch(
      plotType, 
      "Boxplot" = BoxplotOptions(ns),
      "Scatterplot" = ScatterplotOptions(ns) 
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
    
    # FUTURE WORK: Outlier Labels to be added to renderBoxplot functions
    #  if (!is.null(plotType) && plotType == "Boxplot") {
    #    checkboxInput(
    #      inputId = ns("OutlierLabels"),
    #      label   = "Display outlier labels",
    #      value   = FALSE
    #    )
    #  },
      
      colourpicker::colourInput(
        inputId = ns("Colour"), 
        label = strong("Plot Colour"), 
        value = colour
      ),
      
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

addFlipCheckbox <- function(includeFlip, ns) {
  flip <- tagList()
  
  if(includeFlip){
    flip <- tagList(
      p(strong("Orientation")),
      checkboxInput(
        inputId = ns("Flip"),
        label = "Plot Horizontally",
        value = TRUE
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

ScatterplotOptions <- function(ns) {
  
  tagList(
    tags$h3("Scatterplot Options"),
    
    colourpicker::colourInput(
      inputId = ns("PointsColour"), 
      label = strong("Plot Points Colour"), 
      value = "#000000"
    ),
    
    sliderInput(
      inputId = ns("LineWidth"),
      label = strong("Line Width"),
      min = 1,
      max = 10,
      value = 1,
      step = 1
    ),
    
    sliderInput(
      inputId = ns("PointSize"),
      label = strong("Point Size"),
      min = 1,
      max = 10,
      value = 3,
      step = 1
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
