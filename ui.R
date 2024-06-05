
# ----------------------- #  
# ---- UI components ---- 
# ----------------------- #
ui <- fluidPage(id = "mainContainer",
                theme = bs_theme(version = 4,
                                 primary = "#18536F"),
                
                tags$head(
                  tags$link(rel = "stylesheet", 
                            type="text/css", 
                            href="cougarstats-styles.css"),
                  tags$link(rel = "icon",
                            type="image/x-icon",
                            href="favicon.ico"),
                ),
                
                navbarPage(
                  id = "mainBanner",
                  
                  title = div(
                    class = "navbarLogo",
                    img(src ="CougarStatsLogo.png", height = 100), 
                    span(" CougarStats ", class = "pageTitle")),
                  
                  # --------------------- #  
                  # ---- Methods Tab ---- 
                  # --------------------- #
                  tabPanel(
                    title = "Methods",
                    
                    navbarPage(
                      title = "",
                      
                      tabPanel(
                        title = "Descriptive Statistics",
                        descStatsUI(id = "ds")
                      ),
                      
                      tabPanel(
                        title = "Probability Distributions",
                        probDistUI(id = "pd")
                      ),
                      
                      tabPanel(
                        title = "Sample Size Estimation",
                        sampSizeEstUI(id = "sse")
                      ),
                      
                      tabPanel(
                        title = "Statistical Inference",
                        statInfrUI(id = "si")
                      ),
                      
                      tabPanel(
                        title = "Regression and Correlation",
                        regCorrUI(id = "rc") 
                      )
                    ), # tabPanel "Methods"
                  ),
                  #  ------------------------- #  
                  ## ---- Methods sidebar ---- 
                  #  ------------------------- #       
                  
                  
                  tabPanel(
                    title = "Authors",
                    authorsUI()
                  )
                ), #navbarPage
                
                tags$footer(class = "footer",
                            div(class = "appStoreButton",
                                tags$a(href="https://apps.apple.com/us/app/cougarstats/id6476070179",
                                       target = "_blank",
                                       tags$img(src="AppStoreLogo.svg",
                                                title="App Store Link",
                                                width="150px")))
                )
) #fluidPage

