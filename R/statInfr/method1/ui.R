#' @param id The namespace of the module containing this submodule (it should always be "statInfr" or "si")
statInfrMethodOneUI <- function(id) {
  ns <- NS(id)

  conditionalPanel <- function(condition, ...) {
    shiny::conditionalPanel(condition, ..., ns = ns)
  }

  ## NOTE: the submoduleUI is the return value of this function. The
  ## sidebarPanel or the mainPanel is selected therefrom in the integrating
  ## application.
  submoduleUI <- list(sidebarPanel = NULL, mainPanel = NULL)

  populationMeanSidebarPanel <- tagList(
    radioButtons(
      inputId = ns("dataAvailability"),
      label = strong("Data Availability"),
      choices = list("Summarized Data", "Enter Raw Data", "Upload Data"),
      inline = TRUE
    ),

    ## -------------------- Summarized Data -----------------------------------
    conditionalPanel(
      "input.dataAvailability == 'Summarized Data'",
      numericInput(
        inputId = ns("sampleSize"),
        label   = strong("Sample Size (\\( n\\))"),
        value   = 18,
        min     = 1,
        step    = 1
      ),
      numericInput(
        inputId = ns("sampleMean"),
        label   = strong("Sample Mean (\\( \\bar{x}\\))"),
        value   = 103.5375,
        step    = 0.00001
      ),

      div(id = ns("sigmaKnownSummarizedDataSibling"), style = "display: hidden;"),
      conditionalPanel(condition = "input.sigmaKnown",
                       numericInput(
                         inputId = ns("popuSD"),
                         label   = strong("Population Standard Deviation (\\( \\sigma\\)) Value"),
                         value   = 8.25,
                         min     = 0.00001,
                         step    = 0.00001
                       )),
      conditionalPanel(condition = "!input.sigmaKnown",
                       numericInput(
                         inputId = ns("sampSD"),
                         label   = strong("Sample Standard Deviation (\\( s\\)) Value"),
                         value   = 4.78,
                         min     = 0.00001,
                         step    = 0.00001
                       ))
    ),

    ## -------------------- Raw Data ------------------------------------------
    conditionalPanel(
      condition = "input.dataAvailability == 'Enter Raw Data'",
      textAreaInput(
        inputId     = ns("sample1"),
        label       = strong("Sample"),
        value       = "202, 210, 215, 220, 220, 224, 225, 228, 228, 228",
        placeholder = "Enter values separated by a comma with decimals as points",
        rows        = 3
      ),
      div(id = ns("sigmaKnownEnterRawDataSibling"), style = "display: hidden;"),
      conditionalPanel(
        condition = "input.sigmaKnown",
        numericInput(
          inputId = ns("popuSDRaw"),
          label   = strong("Population Standard Deviation (\\( \\sigma\\)) Value"),
          value   = 8.25,
          min     = 0.00001,
          step    = 0.00001
        )
      )
    ),

    conditionalPanel(
      condition = "input.dataAvailability == 'Upload Data'",
      newFileInput("upload", id),
      conditionalPanel(
        condition = "output.Uploaded",# a reactive output, written in reactives.R.
        selectizeInput(
          inputId = ns("selectUploadVariable"),
          label = strong("Choose a Column for Analysis"),
          choices = c(""),
          options = list(
            placeholder = "Select a column",
            onInitialize = I('function() { this.setValue(""); }')
          )
        ),
        div(id = ns("sigmaKnownUploadDataSibling"), style = "display: hidden;"),
        conditionalPanel(
          condition = "input.sigmaKnown",
          numericInput(
            inputId = ns("popuSDUpload"),
            label   = strong("Population Standard Deviation (\\( \\sigma\\)) Value"),
            value   = 5,
            min     = 0.00001,
            step    = 0.00001
          )
        )
      )
    )
  )

  populationProportionSidebarPanel <- tagList(
    numericInput(
      inputId = ns("numSuccesses"),
      label   = strong("Number of Successes (\\( x\\))"),
      value   = 1087,
      min     = 0,
      step    = 1
    ),
    numericInput(
      inputId = ns("numTrials"),
      label   = strong("Number of Trials (\\( n\\))"),
      value   = 1430,
      min     = 1,
      step    = 1
    )
  )

  populationStandardDeviationSidebarPanel <- tagList(
    numericInput(
      inputId = ns("SSDSampleSize"),
      label   = strong("Sample Size (\\( n\\))"),
      value   = 30,
      min     = 2,
      step    = 1
    ),
    numericInput(
      inputId = ns("SSDStdDev"),
      label   = strong("Sample Standard Deviation (\\( s\\))"),
      value   = 12.23,
      min     = 0.00001,
      step    = 0.00001
    )
  )

  submoduleUI$sidebarPanel <- sidebarPanel(
    id = ns("sidebarPanel"),
    tagList(
      ## NOTE: popuParameter and conditionalPanels dependent thereupon.
      tagList(
        radioButtons(
          inputId = ns("popuParameter"),
          label = strong("Parameter of Interest"),
          choiceValues = list(
            "Population Mean",
            "Population Standard Deviation",
            "Population Proportion"
          ),
          choiceNames = list(
            "Population Mean (\\( \\mu \\)) ",
            "Population Standard Deviation (\\( \\sigma\\)) ",
            "Population Proportion (\\( p \\))"
          ),
          selected = "Population Mean",
          inline = FALSE
        ),
        conditionalPanel(
          condition = "input.popuParameter == 'Population Mean'",
          populationMeanSidebarPanel
        ),
        conditionalPanel(
          condition = "input.popuParameter == 'Population Proportion'",
          populationProportionSidebarPanel
        ),
        conditionalPanel(
          condition = "input.popuParameter == 'Population Standard Deviation'",
          populationStandardDeviationSidebarPanel
        )
      ),# NOTE: popuParameter and conditionalPanels dependent thereupon.

      ## NOTE: inferenceType and conditionalPanels dependent thereupon.
      tagList(
        radioButtons(inputId = ns("inferenceType"),
                     label = strong("Inference Type"),
                     choices = list("Confidence Interval", "Hypothesis Testing"),
                     inline = TRUE),
        ## NOTE: inferenceType
        conditionalPanel(
          condition = "input.inferenceType == 'Confidence Interval'",
          radioButtons(inputId = ns("confidenceLevel"),
                       label = strong(r"[Confidence Level (\(1-\alpha\))]"),
                       choices = c("90%", "95%", "99%"),
                       selected = c("95%"),
                       inline = TRUE)
        ),
        ## NOTE: inferenceType
        conditionalPanel(
          condition = "input.inferenceType == 'Hypothesis Testing'",
          radioButtons(inputId = ns("significanceLevel"),
                       label = strong(r"[Significance Level (\(\alpha\))]"),
                       choices = c("10%", "5%", "1%"),
                       selected = c("5%"),
                       inline = TRUE),
          conditionalPanel(
            condition = "input.popuParameter == 'Population Mean'",
            numericInput(inputId = ns("hypMean"),
                         label   = strong("Hypothesized Population Mean (\\( \\mu_{0}\\)) Value"),
                         value   = 99,
                         step    = 0.00001)
          ),

          conditionalPanel(
            condition = "input.popuParameter == 'Population Proportion'",
            numericInput(inputId = ns("hypProportion"),
                         label   = strong("Hypothesized Population Proportion (\\( p_{0}\\)) Value"),
                         value   = 0.73,
                         min     = 0,
                         max     = 1,
                         step    = 0.00001)
          ),

          conditionalPanel(
            condition = "input.popuParameter == 'Population Standard Deviation'",
            numericInput(inputId = ns("hypStdDeviation"),
                         label   = strong(r"--{Hypothesized Population Standard Deviation (\( \sigma_{0}\)) Value}--"),
                         value   = 16.00,
                         min     = 0.001,
                         step    = 0.001)
          ),
          selectInput(inputId  = ns("altHypothesis"),
                      label    = strong("Alternate Hypothesis (\\( H_{a}\\))"),
                      choices  = lessThanInequalGreaterThanChoices123,
                      selected = 2)
        )
      ),# NOTE: inferenceType and conditionalPanels dependent thereupon.

      ## DONE: no validation or work is needed on this input in the UI.
      conditionalPanel(
        condition = "input.popuParameter == 'Population Mean' && input.dataAvailability != 'Summarized Data'",
        p(strong("Graph Options")),
        checkboxInput(
          inputId = ns("oneMeanBoxplot"),
          label   = "Boxplot for Sample Data",
          value   = TRUE
        )
      ),

      createCalculateResetButtonsGroup(ns)
    )
  )

  submoduleUI$mainPanel <- mainPanel(navbarPage(
    title = NULL,



    tabPanel("Analysis", uiOutput("oneSamplePopulationMeanAnalysis"), value = "popuParameterMeanAnalysis"),
    tabPanel("Graphs", tagList(
                         conditionalPanel(
                           condition = "input.dataAvailability != 'Summarized Data' && input.oneMeanBoxplot",
                           br(),
                           withTags(h2(u("Boxplot"))),
                           br(),
                           plotOptionsMenuUI(
                             id = ns("oneMeanBoxplot"),
                             plotType = "Boxplot",
                             title = "Boxplot"
                           ),
                           plotOutput(ns("oneMeanBoxplotOutput"))
                         ),
                         conditionalPanel(
                           condition = "input.dataAvailability != 'Summarized Data' && !input.oneMeanBoxplot",
                           br(),
                           withTags(h2(u("Histogram"))),
                           br(),
                           plotOptionsMenuUI(
                             id    = ns("oneMeanHistogram"),
                             title = "Histogram"
                           ),
                           uiOutput(ns("renderOneMeanHistogram")),
                           br()
                         )
                       ), value = "popuParameterMeanGraphs"),
    tabPanel("Uploaded Data", DTOutput(ns("UploadedData")), value = "popuParameterMeanUploadedData"),



    tabPanel("Analysis", tagList(
                           conditionalPanel(
                             condition = "input.inferenceType == 'Confidence Interval'",

                             withTags(h2(u("Confidence Interval"))),
                             br(),
                             uiOutput(ns('onePropCI')),
                             br()
                           ),
                           conditionalPanel(
                             condition = "input.inferenceType == 'Hypothesis Testing'",

                             withTags(h2(u("Hypothesis Test"))),
                             br(),
                             uiOutput(ns('onePropHT')),
                             br()
                           )
                         ), value = "popuParameterProportionAnalysis"),
    tabPanel("Graphs", tagList(
                         br(),
                         div(
                           style = "display: flex; justify-content: flex-start;",
                           plotOutput(ns("onePropBarGraph"), width = "400px")
                         ),
                         br(),
                         div(
                           style = "display: flex; justify-content: flex-start;",
                           plotOutput(ns("onePropPieChart"), width = "400px")
                         )
                       ), value = "popuParameterProportionGraphs"),
    tabPanel("Uploaded Data", uiOutput("mainPanelUploadedData"), value = "popuParameterProportionUploadedData"),



    tabPanel("Analysis", tagList(
                           conditionalPanel(
                             condition = "input.inferenceType == 'Confidence Interval'",
                             withTags(h2(u("Confidence Interval"))),
                             br(),
                             uiOutput(ns('oneSDCI')),
                             br()
                           ),
                           conditionalPanel(
                             condition = "input.inferenceType == 'Hypothesis Testing'",
                             withTags(h2(u("Hypothesis Test"))),
                             br(),
                             uiOutput(ns('onePopulationSDHT')),
                             br()
                           )
                         ), value = "popuParameterStandardDeviationAnalysis"),
    tabPanel("Graphs", tagList(), value = "popuParameterStandardDeviationGraphs"),
    tabPanel("Uploaded Data", tagList(), value = "popuParameterStandardDeviationUploadedData"),



    id = ns("mainPanelNavbarPage")),
    id = ns("mainPanel"))

  submoduleUI
}
