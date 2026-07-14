#' @param id The id of the integrating module.
statInfrMethodCategoricalUI <- function(id) {
  ns <- NS(id)

  submoduleUI <- list(sidebarPanel = NULL, mainPanel = NULL)

  submoduleUI$sidebarPanel <- tagList(
    radioButtons(
      inputId = ns("chisquareDimension"),
      label = strong("Dimension"),
      choices = c(
        "2 x 2",
        "2 x 3",
        "3 x 2",
        "3 x 3"
      ),
      inline = TRUE
    ),
    conditionalPanel(
      ns = ns,
      condition = "input.chisquareDimension == '2 x 2'",
      matrixInput(
        inputId = ns("chiSqInput2x2"),
        inputClass = "cMatrix",
        value = matrix(c(173, 599, 160, 851),
                       nrow = 2,
                       ncol = 2,
                       dimnames = list(
                         c("R1", "R2"),
                         c("C1", "C2")
                       )
                       ),
        rows = list(editableNames = TRUE),
        cols = list(editableNames = TRUE),
        class = "numeric"
      )
    ), ## 2✕2

    conditionalPanel(
      ns = ns,
      condition = "input.chisquareDimension == '2 x 3'",
      matrixInput(
        inputId = ns("chiSqInput2x3"),
        inputClass = "cMatrix",
        value = matrix(c(160, 40, 140, 60, 40, 60),
                       nrow = 2,
                       ncol = 3,
                       dimnames = list(
                         c("R1", "R2"),
                         c("C1", "C2", "C3")
                       )
                       ),
        rows = list(editableNames = TRUE),
        cols = list(editableNames = TRUE),
        class = "numeric"
      )
    ), ## 2✕3

    conditionalPanel(
      ns = ns,
      condition = "input.chisquareDimension == '3 x 2'",
      matrixInput(
        inputId = ns("chiSqInput3x2"),
        inputClass = "cMatrix",
        value = matrix(c(162, 106, 201, 353, 259, 332),
                       nrow = 3,
                       ncol = 2,
                       dimnames = list(
                         c("R1", "R2", "R3"),
                         c("C1", "C2")
                       )
                       ),
        rows = list(editableNames = TRUE),
        cols = list(editableNames = TRUE),
        class = "numeric"
      )
    ), ## 3✕2

    conditionalPanel(
      ns = ns,
      condition = "input.chisquareDimension == '3 x 3'",
      matrixInput(
        inputId = ns("chiSqInput3x3"),
        inputClass = "cMatrix",
        value = matrix(c(6, 14, 50, 38, 31, 50, 31, 4, 5),
                       nrow = 3,
                       ncol = 3,
                       dimnames = list(
                         c("R1", "R2", "R3"),
                         c("C1", "C2", "C3")
                       )
                       ),
        rows = list(editableNames = TRUE),
        cols = list(editableNames = TRUE),
        class = "numeric"
      )
    ), ## 3✕3

    textInput(
      inputId = ns("chiSquareRowHeader"),
      label = "Name for Row Variable (optional):",
      value = ""
    ),
    textInput(
      inputId = ns("chiSquareColHeader"),
      label = "Name for Column Variable (optional):",
      value = ""
    ),
    radioButtons(
      inputId = ns("chisquareMethod"),
      label = strong("Hypothesis Test"),
      choiceNames = c(
        "Chi-Square test for independence",
        "Fisher's Exact test"
      ),
      choiceValues = c(
        "Chi-Square",
        "Fisher"
      ),
      selected = c("Chi-Square"),
      inline = TRUE
    ),
    conditionalPanel(
      ns = ns,
      condition = 'input.chisquareMethod == "Chi-Square" && input.chisquareDimension == "2 x 2"',
      checkboxInput(
        inputId = ns("chiSquareYates"),
        label   = "with Yates continuity correction",
        value   = FALSE
      )
    ),
    radioButtons(
      inputId = ns("chisquareSigLvl"),
      label = strong("Significance Level (\\( \\alpha\\))"),
      choices = c(
        "10%",
        "5%",
        "1%"
      ),
      selected = c("5%"),
      inline = TRUE
    )
  )

  submoduleUI$mainPanel <- tagList(
    ## ---------------- Chi-Square Test for Independence -----------------------
    conditionalPanel(
      ns = ns,
      condition = "input.chisquareMethod == 'Chi-Square'",
      h2("Chi-Square Test for Independence"),# TODO: former titlePanel; set window title dynamically elsewhere.
      hr(),
      br(),
      h4("Observed Frequencies"),
      br(),
      uiOutput(ns("renderChiSqObs")),
      br(),
      br(),
      h4("Expected Frequencies"),
      br(),
      uiOutput(ns("renderChiSqExp")),
      br(),
      br(),
      h4("Calculation of the \\( \\chi^2 \\) statistic value"),
      br(),
      uiOutput(ns("renderChiSqResults")),
      br(),
      hr(),
      br(),
      uiOutput(ns("chiSqTest")),
      br(),
      br()
    ),

    ## ---------------- Fisher's Exact Test ------------------------------------
    conditionalPanel(
      ns = ns,
      condition = "input.chisquareMethod == 'Fisher'",
      h2("Fisher's Exact Test"),# TODO: former titlePanel; set window title dynamically elsewhere.
      hr(),
      br(),
      h4("Observed Frequencies"),
      uiOutput(ns("renderFishersObs")),
      br(),
      br(),
      hr(),
      br(),
      h4("Hypothesis Test"),
      uiOutput(ns("fishersTest")),
      br(),
      br()
    ) # Fisher
  )

  submoduleUI
}
