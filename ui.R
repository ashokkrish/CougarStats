# Old Head - HEAD <- tags$head(tags$link(rel = "stylesheet",
#                            type="text/css",
#                            href="cougarstats-styles.css"),
#                 tags$link(rel = "icon",
#                           type="image/x-icon",
#                          href="favicon.ico"))

HEAD <- tags$head(
  
  # ---- Existing assets ----
  tags$link(rel = "stylesheet",
            type = "text/css",
            href = "cougarstats-styles.css"),
  
  tags$link(rel = "icon",
            type = "image/x-icon",
            href = "favicon.ico"),
  
  # ---- Amplitude Analytics ----
  tags$script(src = "https://cdn.amplitude.com/libs/analytics-browser-2.11.1-min.js.gz"),
  tags$script(src = "https://cdn.amplitude.com/libs/plugin-session-replay-browser-1.25.0-min.js.gz"),
  
  tags$script(HTML("
    window.amplitude.add(
      window.sessionReplay.plugin({ sampleRate: 1 })
    );
    window.amplitude.init(
      '9c16daacc728f3aa3e4fe91129eca5e8',
      { autocapture: { elementInteractions: true } }
    );
  "))
)

BODY <-
  div(
    navbarPage(
      id = "mainBanner",
      title = div(class = "navbarLogo",
                  img(src ="CougarStatsLogo.png", height = 100),
                  span(" CougarStats ", class = "pageTitle")),
      tabPanel("Methods",
               navbarPage(title = NULL,
                          ## tabPanel("Data Upload and Editing",
                          ##          import_ui(id = "dataImport",
                          ##                    from = c("file", "copypaste", "env"))),
                          tabPanel("Descriptive Statistics",
                                   descStatsUI(id = "ds")),
                          tabPanel("Probability Distributions",
                                   probDistUI(id = "pd")),
                          tabPanel("Sample Size Estimation",
                                   sampSizeEstUI(id = "sse")),
                          tabPanel("Statistical Inference",
                                   statInfrUI(id = "si")),
                          tabPanel("Regression and Correlation",
                                   regressionAndCorrelationUI(id = "rc")))),
      tabPanel("Authors", authorsUI())
    )
  )

ui <- fluidPage(id = "mainContainer",
                theme = bs_theme(version = 4, primary = "#18536F"),
                use_darkmode(),
                tags$div(style = paste(
                           "position: fixed",
                           "top: 20px",
                           "right: 30px",
                           "z-index: 99",
                           "font-size: 18px",
                           "border: none",
                           "outline: none",
                           "color: white",
                           "cursor: pointer",
                           "padding: 15px",
                           "border-radius: 4px",
                           sep = "; "
                         ),
                         HTML(r"[<button id="togglemode" type="button" class="btn btn-warning">Toggle Dark Mode</button>]")),
                tags$div(style = paste("position: absolute",
                                       "top: 20px",
                                       "right: 200px",
                                       "z-index: 99",
                                       "font-size: 18px",
                                       "border: none",
                                       "outline: none",
                                       "color: white",
                                       "cursor: pointer",
                                       "padding: 15px",
                                       "border-radius: 4px",
                                       sep = "; "),
                         tags$a(href = "https://apps.apple.com/us/app/cougarstats/id6476070179",
                                target = "_blank",
                                tags$img(src = "AppStoreLogo.svg", title = "App Store Link", width = "150px"))),
                HEAD,
                BODY)
