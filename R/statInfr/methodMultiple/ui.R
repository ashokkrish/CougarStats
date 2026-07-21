#' @param id The id of the integrating application.
function(id) {
  ns <- NS(id)

  submoduleUI <- list(sidebarPanel = NULL, mainPanel = NULL)

  ## NOTE: componenets of statInfrSidebarPanelMethodMultiple begins with ANOVA, then
  ## has Kruskal-Wallis, then is followed by the actual
  ## si__sidebarPanelMethodMultiple. See the identifier: ANOVA and KruskalWallis are
  ## suffixes to the identifier.
  sidebarPanelANOVA <- conditionalPanel(
    ns = ns,
    condition = "input.multipleMethodChoice == 'anova'",
    newFileInput("anovaUserData", id),
    hidden(tagList(
      div(
        id = ns("anovaUploadInputs"),
        radioButtons(
          inputId = ns("anovaFormat"),
          label = strong("Data Format"),
          choiceNames = c(
            "Values in multiple columns",
            "Responses and factors stacked in two columns"
          ),
          choiceValues = c(
            "Multiple",
            "Stacked"
          )
        ),
        conditionalPanel(
          ns = ns,
          condition = "input.anovaFormat == 'Multiple'",
          selectizeInput(
            inputId = ns("anovaMultiColumns"),
            label = strong("Choose columns to conduct analysis"),
            choices = c(""),
            multiple = TRUE,
            selected = NULL,
            options = list(
              hideSelected = FALSE,
              placeholder = "Select two or more columns",
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
        ), # multiple column anova

        conditionalPanel(
          ns = ns,
          condition = "input.anovaFormat == 'Stacked'",
          selectizeInput(
            inputId = ns("anovaResponse"),
            label = strong("Response Variable"),
            choices = c(""),
            selected = NULL,
            options = list(
              placeholder = "Select a variable",
              onInitialize = I('function() { this.setValue(""); }')
            )
          ),
          selectizeInput(
            inputId = ns("anovaFactors"),
            label = strong("Factors"),
            choices = c(""),
            selected = NULL,
            options = list(
              placeholder = "Select a factor",
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
        ) # stacked column anova
      ) # anovaUploadInputs div
    )), # hidden tagList

    radioButtons(
      inputId = ns("anovaSigLvl"),
      label = strong("Significance Level (\\( \\alpha\\))"),
      choices = c(
        "10%",
        "5%",
        "1%"
      ),
      selected = "5%",
      inline = TRUE
    ),
    checkboxGroupInput(
      inputId = ns("anovaOptions"),
      label = p(strong("Options")),
      choiceNames = c("Include post hoc tests"),
      choiceValues = c("posthoc"),
      selected = NULL
    ),
    selectizeInput(
      inputId = ns("anovaGraphs"),
      label = strong("Graph Options"),
      choices = c(
        "Side-by-side Boxplot",
        "Histogram of Residuals",
        "QQ Plot of Residuals",
        "Plot Group Means"
      ),
      multiple = TRUE,
      selected = c(
        "Side-by-side Boxplot",
        "Plot Group Means"
      ),
      options = list(
        hideSelected = FALSE,
        placeholder = "Select graph(s) to display"
      )
    )
  )

  sidebarPanelKruskalWallis <- conditionalPanel(
    ns = ns,
    condition = 'input.multipleMethodChoice == "kw"',
    newFileInput("kwUserData", id),
    hidden(tagList(
      div(
        id = ns("kwUploadInputs"),
        radioButtons(
          inputId = ns("kwFormat"),
          label = strong("Data Format"),
          choiceNames = c(
            "Values in multiple columns",
            "Responses and factors stacked in two columns"
          ),
          choiceValues = c(
            "Multiple",
            "Stacked"
          )
        ),
        conditionalPanel(
          ns = ns,
          condition = "input.kwFormat == 'Multiple'",
          selectizeInput(
            inputId = ns("kwMultiColumns"),
            label = strong("Choose columns to conduct analysis"),
            choices = c(""),
            multiple = TRUE,
            selected = NULL,
            options = list(
              hideSelected = FALSE,
              placeholder = "Select two or more columns",
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
        ), # multiple column kw

        conditionalPanel(
          ns = ns,
          condition = "input.kwFormat == 'Stacked'",
          selectizeInput(
            inputId = ns("kwResponse"),
            label = strong("Response Variable"),
            choices = c(""),
            selected = NULL,
            options = list(
              placeholder = "Select a variable",
              onInitialize = I('function() { this.setValue(""); }')
            )
          ),
          selectizeInput(
            inputId = ns("kwFactors"),
            label = strong("Factors"),
            choices = c(""),
            selected = NULL,
            options = list(
              placeholder = "Select a factor",
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
        ) # stacked column kw
      ) # kwUploadInputs div
    )), # hidden tagList

    radioButtons(
      inputId = ns("kwSigLvl"),
      label = strong("Significance Level (\\( \\alpha\\))"),
      choices = c(
        "10%",
        "5%",
        "1%"
      ),
      selected = "5%",
      inline = TRUE
    )
  )

  submoduleUI$sidebarPanel <- tagList(
    HTML("<label class='si-label'><b>Hypothesis Test</b></label>"),
    radioButtons(
      inputId = ns("multipleMethodChoice"),
      label = NULL,
      choiceNames = c("One-way Analysis of Variance (ANOVA)", "Kruskal-Wallis"),
      choiceValues = c("anova", "kw")
    ),

    sidebarPanelANOVA,
    sidebarPanelKruskalWallis
  )

  mainPanelANOVA <- navbarPage(
    id = ns("anovaTabset"),
    selected = "Analysis",
    title = NULL,
    tabPanel(
      id = ns("anova"),
      title = "Analysis",
      h2("One-way Analysis of Variance (ANOVA)"),# TODO: former titlePanel; set window title dynamically elsewhere.
      hr(),
      br(),
      uiOutput(ns("anovaOutput")),
      br(),
      br(),
      conditionalPanel(
        ns = ns,
        condition = "input.anovaOptions.indexOf('posthoc') > -1",
        uiOutput(ns("anovaPosthocAnalysis"))
      )
    ),
    tabPanel(
      title = "Graphs",
      conditionalPanel(
        ns = ns,
        condition = "input.anovaGraphs.indexOf('Side-by-side Boxplot') > -1",
        h2("Side-by-side Boxplot"),# TODO: former titlePanel; set window title dynamically elsewhere.
        br(),
        br(),
        plotOptionsMenuUI(
          id       = ns("anovaBoxplot"),
          plotType = "Boxplot",
          title    = "Side-by-Side Boxplot"
        ),
        uiOutput(ns("renderAnovaBoxplot"))
      ),
      conditionalPanel(
        ns = ns,
        condition = "input.anovaGraphs.indexOf('Histogram of Residuals') > -1",
        h2("Histogram of Residuals"),# TODO: former titlePanel; set window title dynamically elsewhere.
        br(),
        br(),
        plotOptionsMenuUI(
          id    = ns("anovaHistogram"),
          title = "Histogram of Residuals",
          xlab  = "Residuals",
          ylab  = "Frequency"
        ),
        uiOutput(ns("renderAnovaHistogram"))
      ),
      conditionalPanel(
        ns = ns,
        condition = "input.anovaGraphs.indexOf('QQ Plot of Residuals') > -1",
        h2("QQ Plot of Residuals"),# TODO: former titlePanel; set window title dynamically elsewhere.
        br(),
        br(),
        plotOptionsMenuUI(
          id     = ns("anovaQQplot"),
          title  = "QQ Plot of Residuals",
          xlab   = "Normal Quantiles",
          ylab   = "Residuals",
          colour = "#0F3345"
        ),
        uiOutput(ns("renderAnovaQQplot"))
      ),
      conditionalPanel(
        ns = ns,
        condition = "input.anovaGraphs.indexOf('Plot Group Means') > -1",
        h2("Group Means"),# TODO: former titlePanel; set window title dynamically elsewhere.
        br(),
        br(),
        plotOptionsMenuUI(
          id     = ns("anovaMeanPlot"),
          title  = "Group Means",
          xlab   = "Group",
          ylab   = "Mean",
          colour = "#0F3345"
        ),
        uiOutput(ns("renderAnovaMeanPlot"))
      )
    ),
    tabPanel(
      id    = ns("anovaData"),
      title = "Uploaded Data",
      uiOutput(ns("renderAnovaDataView"))
    )
  )

  mainPanelKruskalWallis <- navbarPage(
    id = ns("kwTabset"),
    selected = "Analysis",
    title = NULL,
    tabPanel(
      id = ns("kw"),
      title = "Analysis",
      h2("Hypothesis Test"),# TODO: former titlePanel; set window title dynamically elsewhere.
      br(),
      uiOutput(ns("kwHT")),
      br(),
      plotOutput(ns("kruskalWallisPlot"), width = "50%", height = "400px"),
      br(),
      uiOutput(ns("kwConclusionOutput"))
    ),
    tabPanel(
      id    = ns("kwRM"),
      title = "Data with Ranks",
      DTOutput("renderrankedmean"),
      uiOutput(ns("renderKWRM"))
    ),
    tabPanel(
      id    = ns("kwData"),
      title = "Uploaded Data",
      ## uiOutput(ns("renderKWData"))
      DTOutput(ns("kwUploadData"))
    )
  )

  submoduleUI$mainPanel <- tagList(
    conditionalPanel("input.multipleMethodChoice == 'anova'", mainPanelANOVA, ns = ns),
    conditionalPanel("input.multipleMethodChoice == 'kw'", mainPanelKruskalWallis, ns = ns)
  )

  submoduleUI
}
