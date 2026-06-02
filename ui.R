ui <- tagList(withTags(html(
  head(
    ## CougarStats logo and styling
    link(rel = "stylesheet", type = "text/css", href = "cougarstats-styles.css"),
    link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),

    ## ShinyDarkmode
    use_darkmode(),

    ## Authors modal styles now provided via output.css (linked from authors.html)
    link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.1/css/all.min.css"),

    ## Amplitude Analytics
    script(src = "https://cdn.amplitude.com/libs/analytics-browser-2.11.1-min.js.gz"),
    script(src = "https://cdn.amplitude.com/libs/plugin-session-replay-browser-1.25.0-min.js.gz"),
    script(HTML(r"{
    window.amplitude.add(
      window.sessionReplay.plugin({ sampleRate: 1 })
    );
    window.amplitude.init(
      '9c16daacc728f3aa3e4fe91129eca5e8',
      { autocapture: { elementInteractions: true } }
    );}"))
  ),
  navbarPage(
    id = "mainBanner",
    theme = bs_theme(version = 4, primary = "#18536F"),
    title = tagList(img(src = "CougarStatsLogo.png", height = "40px"), span("CougarStats")),
    tabPanel("Descriptive Statistics", descStatsUI(id = "ds")),
    tabPanel("Probability Distributions", probDistUI(id = "pd")),
    tabPanel("Sample Size Estimation", sampSizeEstUI(id = "sse")),
    tabPanel("Statistical Inference", statInfrUI(id = "si")),
    tabPanel("Regression and Correlation", regressionAndCorrelationUI(id = "rc")),
    tabPanel("Machine Learning", machineLearningUI(id = "ml")),
    )
)))
