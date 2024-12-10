HEAD <- tags$head(tags$link(rel = "stylesheet",
                            type="text/css",
                            href="cougarstats-styles.css"),
                  tags$link(rel = "icon",
                            type="image/x-icon",
                            href="favicon.ico"))

FOOT <- tags$footer(class = "footer",
                    div(class = "appStoreButton",
                        tags$a(href="https://apps.apple.com/us/app/cougarstats/id6476070179",
                               target = "_blank",
                               tags$img(src="AppStoreLogo.svg",
                                        title="App Store Link",
                                        width="150px"))))

BODY <-
  navbarPage(
    id = "mainBanner",
    title = div(class = "navbarLogo",
                img(src ="CougarStatsLogo.png", height = 100),
                span(" CougarStats ", class = "pageTitle"),
                tags$div(style = paste("margin-top: -10px",
                                       "text-align: right",
                                       "float: right",
                                       "margin-right: -150px",
                                       sep = "; "),
                         prettySwitch(inputId = "togglemode",
                                      label = "Dark Mode",
                                      value = TRUE,
                                      fill = TRUE,
                                      status = "primary"))),
    tabPanel("Methods",
             navbarPage(title = NULL,
                        tabPanel("Descriptive Statistics", descStatsUI(id = "ds")),
                        tabPanel("Probability Distributions", probDistUI(id = "pd")),
                        tabPanel("Sample Size Estimation", sampSizeEstUI(id = "sse")),
                        tabPanel("Statistical Inference", statInfrUI(id = "si")),
                        tabPanel("Regression and Correlation", regCorrUI(id = "rc")))),
    tabPanel("Authors", authorsUI())
  )

ui <- fluidPage(id = "mainContainer",
                theme = bs_theme(version = 4, primary = "#18536F"),
                use_darkmode(),
                HEAD,
                BODY,
                FOOT)
