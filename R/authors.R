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

    p(a(href = "https://github.com/bryce-carson",
        span("Bryce Carson, B.Sc.", style = "font-weight:bold"),
        target = "_blank")),
    p("Senior Developer"),
    p("Calgary, AB, CANADA"),
    br(),
    
    p(span("Samantha Brian", style= "font-weight:bold")),
    p("Developer"),
    #p(span("Lead Developer", style = "font-weight:bold")),
    p("Undergraduate Student, Mount Royal University,"),
    p("Calgary, AB, CANADA"),
    
    br(),
    
    p(HTML("<b>Acknowledgement:</b> The development of this interactive Shiny&#174; app
has benefited from the contributions of several students. In Fall 2022, Crystal
Wai presented an early version of this app as part of her COMP 5690: Senior
Computer Science Project. From June to August 2023, Michael Myer further
developed the project, supported by a student research grant from the Faculty of
Science and Technology at Mount Royal University. From September 2023 to April
2024, Michael Myer continued his work on the project with funding from a
Provost's Teaching-Learning Enhancement Grant (TLEG). Beginning in October 2024,
this project will receive continued support through the TLEG for the
2024&ndash;2025 cycle.")),
    br(),
    
    p("This interactive R Shiny app is maintained by Dr. Ashok Krishnamurthy. We welcome questions, insights, and feedback."),
    
    hr(),
    
    HTML("<h5>Created with Posit&trade; and Shiny&#174;</h5>")
  )
}
