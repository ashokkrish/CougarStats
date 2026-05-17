APPLE <- withTags(a(
  class = "AppStore",
  href = "https://apps.apple.com/us/app/cougarstats/id6476070179",
  target = "_blank",
  img(src = "AppStoreLogo.svg", title = "CougarStats in the Apple AppStore", width = "150px")
))

AMPLITUDE_ANALYTICS <- withTags(tagList(
  script(src = "https://cdn.amplitude.com/libs/analytics-browser-2.11.1-min.js.gz"),
  script(src = "https://cdn.amplitude.com/libs/plugin-session-replay-browser-1.25.0-min.js.gz"),
  script(r"--[
    window.amplitude.add(
      window.sessionReplay.plugin({ sampleRate: 1 })
    );
    window.amplitude.init(
      '9c16daacc728f3aa3e4fe91129eca5e8',
      { autocapture: { elementInteractions: true } }
    );
]--")
))

CougarStatsCSSFavIcon <- withTags(tagList(
  link(rel = "stylesheet", type = "text/css", href = "cougarstats-styles.css"),
  link(rel = "icon", type = "image/x-icon", href = "favicon.ico")
))

CougarStatsLogo <- withTags(div(
  class = "navbarLogo",
  img(src = "CougarStatsLogo.png", height = 100),
  span(class = "pageTitle", "CougarStats")
))

## FIXME: it is bad practice to nest these; previously we were three deep, but
## now only two!
METHODS <- page_navbar(
  ## nav_panel(
  ##   "Data Upload and Editing",
  ##   import_ui(id = "dataImport", from = c("file", "copypaste", "env"))
  ## ),
  nav_panel("Descriptive Statistics", descStatsUI("ds")),
  nav_panel("Probability Distributions", probDistUI("pd")),
  nav_panel("Sample Size Estimation", sampSizeEstUI("sse")),
  nav_panel("Statistical Inference", statInfrUI("si")),
  nav_panel("Regression and Correlation", regressionAndCorrelationUI("rc")),
  nav_panel("Machine Learning", machineLearningUI("ml"))
)

ui <- withTags(html(
  head(tagList(AMPLITUDE_ANALYTICS, CougarStatsCSSFavIcon)),
  page_navbar(
    id = "CougarStatsNavbar",# NOTE: actively used in the application CSS.
    theme = bs_theme(primary = "#18536F"),
    window_title = "CougarStats",
    nav_item(CougarStatsLogo),
    nav_item(),
    nav_panel("Methods", METHODS),
    nav_panel("Authors", authorsUI()),
    nav_spacer(),
    nav_item(APPLE),
    nav_item(input_dark_mode())
  )
))
