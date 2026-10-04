ui <- tagList(withTags(html(
  head(
    ## CougarStats logo and styling
    link(rel = "stylesheet", type = "text/css", href = "cougarstats-styles.css"),
    link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
    link(rel = "stylesheet", type = "text/css", href = "code-block.css"),

    ## copyPlotToClipboard(), used by the "Copy to Clipboard" buttons under the
    ## plotly plots in Probability Distributions, Simple Linear Regression and
    ## Polynomial Regression. Defined once here instead of inline in each module.
    script(src = "copyPlotToClipboard.js", defer = NA),

    ## ShinyDarkmode. The darkmode-js library is loaded with defer so it no
    ## longer blocks parsing of the page; deferred scripts still run before
    ## Shiny starts, so the Darkmode object exists when the server enables it.
    htmltools::tagQuery(use_darkmode())$
      find("script")$
      filter(function(el, i) !is.null(el$attribs$src))$
      addAttrs(defer = NA)$
      allTags(),

    ## Font Awesome for icons in modal content: shiny's icon() already puts
    ## Font Awesome 6 (all styles, including brands) on the page, so no second
    ## copy is loaded from a CDN.

    ## Amplitude Analytics
    ## Both libraries are loaded asynchronously so they block neither parsing
    ## nor Shiny's start-up. Amplitude is initialised once both have loaded,
    ## with the same configuration as before (sampleRate 1 records every
    ## session); if either fails to load, nothing is initialised, as before.
    script(HTML(r"{
    (function() {
      var pending = 2;
      function initAmplitude() {
        if (--pending > 0) return;
        if (!window.amplitude || !window.sessionReplay) return;
        window.amplitude.add(
          window.sessionReplay.plugin({ sampleRate: 1 })
        );
        window.amplitude.init(
          '9c16daacc728f3aa3e4fe91129eca5e8',
          { autocapture: { elementInteractions: true } }
        );
      }
      [
        'https://cdn.amplitude.com/libs/analytics-browser-2.11.1-min.js.gz',
        'https://cdn.amplitude.com/libs/plugin-session-replay-browser-1.25.0-min.js.gz'
      ].forEach(function(src) {
        var s = document.createElement('script');
        s.src = src;
        s.async = true;
        s.onload = s.onerror = initAmplitude;
        document.head.appendChild(s);
      });
    })();}")),
    ## End Amplitude Analytics

    ## NOTE: this is important to fix the Title; somehow there are a whack-tonne
    ## of title tags and none of theme are correct, and they all otherwise
    ## override the simple title= argument value.
    script(HTML(r"{
    (() => {
      const titleText = 'CougarStats';
      document.title = titleText;
      const head = document.head || document.documentElement;
      const titles = head.getElementsByTagName('title');
      while (titles.length > 1) titles[titles.length - 1].remove();
      if (titles.length === 0) {
        const t = document.createElement('title');
        t.textContent = titleText;
        head.appendChild(t);
      } else if (titles[0].textContent !== titleText) {
        titles[0].textContent = titleText;
      }
    })();
    }"))
  ),
  withTags(div(
    div(
      style = paste(
        sep = "; ",
        "background-color: #18536F",
        "display: flex",
        "align-items: flex-start",
        "justify-content: space-between"
      ),
      div(style = "align-self: center;",
          img(src = "CougarStatsLogo.png",
              style = "height: 100px; padding: 10px;"),
          span("CougarStats",
               style = paste(sep = "; ",
                             "color: var(--off-white)",
                             "font-weight: bold",
                             "font-style: italic",
                             "font-size: 24pt",
                             "vertical-align: middle"))),
      
      div(id = "top-level-action-buttons",
          style = "align-self: center; margin-right: 20px;",
          actionButton("togglemode", "Toggle Dark or Light Mode", icon = icon("sun")),
          actionButton("authors_show", "About", icon = icon("question"), class = "btn-info"))
    ),
    navbarPage(
      NULL,# NOTE: title is intentionally NULL; see previous div.
      
      tabPanel("Descriptive Statistics", descStatsUI(id = "ds")),
      tabPanel("Probability Distributions", probDistUI(id = "pd")),
      tabPanel("Sample Size Estimation", sampleSizeConfidCoeEstUI(id = "sse")),
      tabPanel("Statistical Inference", statInfrUI(id = "si")),
      tabPanel("Regression and Correlation", regressionAndCorrelationUI(id = "rc")),
      tabPanel("Machine Learning", machineLearningUI(id = "ml")),
      
      theme = bs_theme(version = 4, primary = "#18536F"),
      id = "methods-nav"
    )
  ))
)))

## Page cache. Shiny renders a static ui object to HTML again on every page
## request (renderPage: about 0.5 s of R time for this ~500 KB page, during
## which every other session waits). The page does not vary per request, so
## it is rendered once, on the first request (inside the running app, so bslib
## registers its theme as it does for a normal render), and that same response
## is served to every later visitor. Shiny returns an httpResponse from a ui
## function as-is. If the render fails, the object is handed back to Shiny so
## it renders (or fails) exactly as before, and the next request tries again.
ui_static <- ui
ui_cache <- new.env()
ui <- function(req) {
  if (is.null(ui_cache$resp)) {
    ui_cache$resp <- tryCatch({
      html <- shiny:::renderPage(ui_static, showcase = 0,
                                 testMode = shiny::getShinyOption("testmode", default = FALSE))
      shiny::httpResponse(200, content = html)
    }, error = function(e) NULL)
    if (is.null(ui_cache$resp)) return(ui_static)
  }
  ui_cache$resp
}
