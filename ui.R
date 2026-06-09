ui <- tagList(withTags(html(
  head(
    ## CougarStats logo and styling
    link(rel = "stylesheet", type = "text/css", href = "cougarstats-styles.css"),
    link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
    link(rel = "stylesheet", type = "text/css", href = "code-block.css"),
    
    ## ShinyDarkmode
    use_darkmode(),
    
    ## Font Awesome for icons in modal content.
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
    );}")),
    
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
      tabPanel("Sample Size Estimation", sampSizeEstUI(id = "sse")),
      tabPanel("Statistical Inference", statInfrUI(id = "si")),
      tabPanel("Regression and Correlation", regressionAndCorrelationUI(id = "rc")),
      tabPanel("Machine Learning", machineLearningUI(id = "ml")),
      
      theme = bs_theme(version = 4, primary = "#18536F"),
      id = "methods-nav"
    )
  ))
)))