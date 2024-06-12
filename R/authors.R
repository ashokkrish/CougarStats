authorsUI <- function() {
  tagList(
    h3("Development Team", style= "font-weight:bold"),
    
    br(),
    
    p(span("Ashok Krishnamurthy, PhD", style= "font-weight:bold")),
    p("Project PI,"),
    p("Associate Professor, Department of Mathematics and Computing,"),
    p("Faculty of Science and Technology,"),
    p("Mount Royal University,"), 
    p("Calgary, AB, CANADA"),
    br(),
    
    p("Email:",a("akrishnamurthy@mtroyal.ca", href = "mailto:akrishnamurthy@mtroyal.ca")), 
    p("Website:", a(href = "https://bit.ly/2YKrXjX","https://bit.ly/2YKrXjX", target = "_blank")),
    p("GitHub:", a(href = "https://github.com/ashokkrish/CougarStats","https://github.com/ashokkrish/CougarStats", target = "_blank")),
    br(),
    
    p(span("Michael Myer", style= "font-weight:bold")),
    p("Senior Developer"),
    #p(span("Lead Developer", style = "font-weight:bold")),
    p("Undergraduate Student, Mount Royal University,"),
    p("Calgary, AB, CANADA"),
    
    br(),
    
    p(span("Samantha Brian", style= "font-weight:bold")),
    p("Developer"),
    #p(span("Lead Developer", style = "font-weight:bold")),
    p("Undergraduate Student, Mount Royal University,"),
    p("Calgary, AB, CANADA"),
    
    br(),
    
    p(span("Michael Walsh", style= "font-weight:bold")),
    p("Developer"),
    #p(span("Lead Developer", style = "font-weight:bold")),
    p("Undergraduate Student, Mount Royal University,"),
    p("Calgary, AB, CANADA"),
    
    br(),
    
    p("Acknowledgement: In Fall 2022 an earlier version of this interactive R Shiny app was presented as Crystal 
      Wai's COMP 5690: Senior Computer Science Project. From June - August 2023 this project was funded by a 
      student research grant conferred by the Faculty of Science and Technology at Mount Royal University. From 
      September 2023 - April 2024 this project was funded by a Provost's Teaching-Learning Enhancement Grant 
      (TLEG). Starting July 2024 this project will be funded by the Provost's Teaching-Learning Enhancement Grant 
      (TLEG) for the 2024-2025 cycle."), 
    br(),
    
    p("This interactive R Shiny app is maintained by Dr. Ashok Krishnamurthy. We welcome questions, insights, and feedback."),
    
    hr(),
    
    h5("Built with",
       img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "35px"),
       "by",
       img(src = "https://www.rstudio.com/wp-content/uploads/2018/10/RStudio-Logo.png", height = "35px"),
       "."
    )
  )
}