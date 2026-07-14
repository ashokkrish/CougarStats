statInfrUI <- function(id) {
  ns <- NS(id)

  statInfrSidebarPanelMethodRadioButtons <- tagList(
    HTML("<label class='si-label'><b>Methodology</b></label>"),
    radioButtons(
      inputId = ns("methodology"),
      label = NULL,
      choiceValues = list(
        "One sample",
        "Two samples",
        "More than two samples",
        "Categorica data"
      ),
      choiceNames = list(
        "Inference about 1 sample\\(\\)",
        "Inference about 2 samples\\(\\)",
        "Inference about more than 2 samples (e.g. ANOVA or Kruskal-Wallis)\\(\\)",
        "Inference for Categorical Data (e.g \\( \\chi^2 \\) test)"
      )
    )
  )





  statInfrMethodOneUI <- statInfrMethodOneUI(id)
  statInfrMethodTwoUI <- statInfrMethodTwoUI(id)
  statInfrMethodMultipleUI <- statInfrMethodMultipleUI(id)
  statInfrMethodCategoricalUI <- statInfrMethodCategoricalUI(id)





  statInfrSidebarPanelMethodOne <- tagList(conditionalPanel(
    ns = ns,
    condition = "input.methodology == 'One sample'",
    statInfrMethodOneUI$sidebarPanel
  ))
  statInfrSidebarPanelMethodTwo <- tagList(conditionalPanel(
    ns = ns,
    condition = "input.methodology == 'Two samples'",
    statInfrMethodTwoUI$sidebarPanel
  ))
  statInfrSidebarPanelMethodMultiple <- tagList(conditionalPanel(
    ns = ns,
    condition = 'input.methodology == "More than two samples"',
    statInfrMethodMultiple$sidebarPanel
  ))
  statInfrSidebarPanelMethodCategorical <- tagList(conditionalPanel(
    ns = ns,
    condition = 'input.methodology == "Categorical data"',
    statInfrMethodCategoricalUI$sidebarPanel
  ))
  statInfrSidebarPanel <- sidebarPanel(
    shinyjs::useShinyjs(),
    statInfrSidebarPanelMethodRadioButtons,
    statInfrSidebarPanelMethodOne,
    statInfrSidebarPanelMethodTwo,
    statInfrSidebarPanelMethodMultiple,
    statInfrSidebarPanelMethodCategorical
  )





  statInfrMainPanelMethodOne <- tagList(conditionalPanel(
    ns = ns,
    condition = "input.methodology == '1'",
    statInfrMethodOneUI$mainPanel
  ))
  statInfrMainPanelMethodTwo <- tagList(conditionalPanel(
    ns = ns,
    condition = "input.methodology == '2'",
    statInfrMethodTwoUI$mainPanel
  ))
  statInfrMainPanelMethodMultiple <- tagList(conditionalPanel(
    ns = ns,
    condition = "input.methodology == 'Multiple'",
    statInfrMethodMultipleUI$mainPanel
  ))
  statInfrMainPanelMethodCategorical <- tagList(conditionalPanel(
    ns = ns,
    condition = "input.methodology == 'Categorical'",
    statInfrMethodCategoricalUI$mainPanel
  ))
  statInfrMainPanel <- mainPanel(
    ## FIXME: the built-in shiny::validate messages, which render where outputs
    ## lay, must be decomposed and relocated appropriately so that these
    ## messages do not display above the second-level navigation bar, breaking
    ## the visual connection. Further, they should each display within the
    ## appropriate tab or not at all!
    uiOutput(ns("inferenceValidation")),
    statInfrMainPanelMethodOne,
    statInfrMainPanelMethodTwo,

    ## FIXME: requires integration.
    statInfrMainPanelMethodMultiple,
    ## NOTE: this is part of the mainPanelMethodMultiple, and used when the choice
    ## is Kruskal-Wallis. FIXME: to be joined properly.
    uiOutput(ns("kwRawContainer")),

    statInfrMainPanelMethodCategorical
  )





  sidebarLayout(statInfrSidebarPanel, statInfrMainPanel)
}
