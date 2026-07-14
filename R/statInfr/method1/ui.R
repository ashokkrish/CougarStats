#' @param id The namespace of the module containing this submodule (it should always be "statInfr" or "si")
statInfrMethodOneUI <- function(id) {
  ns <- NS(id)

  ## NOTE: the submoduleUI is the return value of this function. The
  ## sidebarPanel or the mainPanel is selected therefrom in the integrating
  ## application.
  submoduleUI <- list(sidebarPanel = NULL, mainPanel = NULL)

  submoduleUI$sidebarPanel <- tagList(
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
        "Population Proportion (\\( p\\))"
      ),
      selected = "Population Mean",
      inline = FALSE
    ),

    ## ---------------- Mean ---------------------------------------------------
    conditionalPanel(
      ns = ns,
      condition = "input.popuParameter == 'Population Mean'",
      radioButtons(
        inputId = ns("dataAvailability"),
        label = strong("Data Availability"),
        choices = list("Summarized Data", "Enter Raw Data", "Upload Data"),
        inline = TRUE
      ),

      ## -------------------- Summarized Data -----------------------------------
      conditionalPanel(
        ns = ns,
        condition = "input.dataAvailability == 'Summarized Data'",
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
        radioButtons(
          inputId = ns("sigmaKnown"),
          label = strong("Is Population Standard Deviation (\\( \\sigma\\)) known?"),
          choices = list("Known", "Unknown"),
          inline = TRUE
        ),

        ## ------------------------ Sigma Known ----------------------------------
        conditionalPanel(
          ns = ns,
          condition = "input.sigmaKnown == 'Known'",
          numericInput(
            inputId = ns("popuSD"),
            label   = strong("Population Standard Deviation (\\( \\sigma\\)) Value"),
            value   = 8.25,
            min     = 0.00001,
            step    = 0.00001
          )
        ), # Sigma Known

        ## ------------------------ Sigma Unknown --------------------------------
        conditionalPanel(
          ns = ns,
          condition = "input.sigmaKnown == 'Unknown'",
          numericInput(
            inputId = ns("sampSD"),
            label   = strong("Sample Standard Deviation (\\( s\\)) Value"),
            value   = 4.78,
            min     = 0.00001,
            step    = 0.00001
          )
        ) ## Sigma Unknown
      ), ## One Sample Summarized Data

      ## -------------------- Raw Data ------------------------------------------
      conditionalPanel(
        ns = ns,
        condition = "input.dataAvailability == 'Enter Raw Data'",
        textAreaInput(
          inputId     = ns("sample1"),
          label       = strong("Sample"),
          value       = "202, 210, 215, 220, 220, 224, 225, 228, 228, 228",
          placeholder = "Enter values separated by a comma with decimals as points",
          rows        = 3
        ),
        radioButtons(
          inputId = ns("sigmaKnownRaw"),
          label = strong("Population Standard Deviation (\\( \\sigma\\))"),
          choiceValues = list(
            "rawKnown",
            "rawUnknown"
          ),
          choiceNames = list(
            "Known",
            "Unknown"
          ),
          selected = "rawUnknown",
          inline = TRUE
        ),

        ## ------------------------ Sigma Known ----------------------------------
        conditionalPanel(
          ns = ns,
          condition = "input.sigmaKnownRaw == 'rawKnown'",
          numericInput(
            inputId = ns("popuSDRaw"),
            label   = strong("Population Standard Deviation (\\( \\sigma\\)) Value"),
            value   = 8.25,
            min     = 0.00001,
            step    = 0.00001
          )
        ), ## Sigma Known

        ## ------------------------ Sigma Unknown --------------------------------
        conditionalPanel(
          ns = ns,
          condition = "input.sigmaKnownRaw == 'rawUnknown'"
        ) ## Sigma Unknown
      ), ## One Sample Raw Data

      ## -------------------- Uploaded Data -------------------------------------
      conditionalPanel(
        ns = ns,
        condition = "input.dataAvailability == 'Upload Data'",
        newFileInput("oneMeanUserData", id),
        selectizeInput(
          inputId = ns("oneMeanVariable"),
          label = strong("Choose a Column for Analysis"),
          choices = c(""),
          options = list(
            placeholder = "Select a column",
            onInitialize = I('function() { this.setValue(""); }')
          )
        ),
        radioButtons(
          inputId = ns("sigmaKnownUpload"),
          label = strong("Population Standard Deviation (\\( \\sigma\\))"),
          choices = list("Unknown", "Known"),
          inline = TRUE
        ),

        ## ------------------------ Sigma Known ----------------------------------
        conditionalPanel(
          ns = ns,
          condition = "input.sigmaKnownUpload == 'Known'",
          numericInput(
            inputId = ns("popuSDUpload"),
            label   = strong("Population Standard Deviation (\\( \\sigma\\)) Value"),
            value   = 5,
            min     = 0.00001,
            step    = 0.00001
          )
        ) ## Sigma Known
      ) ## One Sample upload data
    ), ## One Population Mean

    ## ---------------- Proportion ---------------------------------------------
    conditionalPanel(
      ns = ns,
      condition = "input.popuParameter == 'Population Proportion'",
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
    ), # One Population Proportion

    ## ---------------- Standard Deviation -------------------------------------
    conditionalPanel(
      ns = ns,
      condition = "input.popuParameter == 'Population Standard Deviation'",
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
    ), # One Population Standard Deviation

    radioButtons(
      inputId = ns("inferenceType"),
      label = strong("Inference Type"),
      choiceValues = list(
        "Confidence Interval",
        "Hypothesis Testing"
      ),
      choiceNames = list(
        "Confidence Interval",
        "Hypothesis Testing"
      ),
      selected = "Confidence Interval",
      inline = TRUE
    ),

    ## ---------------- Confidence Interval ------------------------------------
    conditionalPanel(
      ns = ns,
      condition = "input.inferenceType == 'Confidence Interval'",
      radioButtons(
        inputId = ns("confidenceLevel"),
        label = strong("Confidence Level (\\( 1- \\alpha\\))"),
        choices = c(
          "90%",
          "95%",
          "99%"
        ),
        selected = c("95%"),
        inline = TRUE
      )
    ), ## Confidence Interval

    ## ---------------- Hypothesis Testing -------------------------------------
    conditionalPanel(
      ns = ns,
      condition = "input.inferenceType == 'Hypothesis Testing'",
      radioButtons(
        inputId = ns("significanceLevel"),
        label = strong("Significance Level (\\( \\alpha\\))"),
        choices = c(
          "10%",
          "5%",
          "1%"
        ),
        selected = c("5%"),
        inline = TRUE
      ),
      conditionalPanel(
        ns = ns,
        condition = "input.popuParameter == 'Population Mean'",
        numericInput(
          inputId = ns("hypMean"),
          label   = strong("Hypothesized Population Mean (\\( \\mu_{0}\\)) Value"),
          value   = 99,
          step    = 0.00001
        )
      ), ## Population Mean

      conditionalPanel(
        ns = ns,
        condition = "input.popuParameter == 'Population Proportion'",
        numericInput(
          inputId = ns("hypProportion"),
          label   = strong("Hypothesized Population Proportion (\\( p_{0}\\)) Value"),
          value   = 0.73,
          min     = 0,
          max     = 1,
          step    = 0.00001
        )
      ), ## Population Proportion

      conditionalPanel(
        ns = ns,
        condition = "input.popuParameter == 'Population Standard Deviation'",
        numericInput(
          inputId = ns("hypStdDeviation"),
          label   = strong(r"--{Hypothesized Population Standard Deviation (\( \sigma_{0}\)) Value}--"),
          value   = 16.00,
          min     = 0.001,
          step    = 0.001
        )
      ), ## Population standard deviation

      selectInput(
        inputId  = ns("altHypothesis"),
        label    = strong("Alternate Hypothesis (\\( H_{a}\\))"),
        choices  = lessThanInequalGreaterThanChoices123,
        selected = 2
        ## options  = list(render = I(render))
      )
    ), ## Hypothesis Testing

    ## ---------------- Graph Options ------------------------------------------
    conditionalPanel(
      ns = ns,
      condition = "input.popuParameter == 'Population Mean' && input.dataAvailability != 'Summarized Data'",
      p(strong("Graph Options")),
      checkboxInput(
        inputId = ns("oneMeanBoxplot"),
        label   = "Boxplot for Sample Data",
        value   = TRUE
      )
    ) ## Pop Mean ! Summarized
  )

  submoduleUI$mainPanel <- tagList(
  )

  submoduleUI
}
