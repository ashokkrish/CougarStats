conditionalPanel(
  condition = "input.dropDownMenu == 'Inference'", 
  
  radioButtons(inputId = "samplesSelect",
               label = strong("Number of samples"),
               choiceValues = list("1","2"),
               choiceNames = list("1","2"),
               selected = "1", #character(0), #
               inline = TRUE,
               width = "1000px"), 
  
  radioButtons(inputId = "dataAvailability",
               label = strong("Data Availability"),
               choiceValues = list("Summarized Data","Enter Raw Data"),
               choiceNames = list("Summarized Data","Enter Raw Data"),
               selected = character(0), # "Summarized Data", #
               inline = TRUE,
               width = "1000px"),
  
  conditionalPanel(
    condition = "input.sigmaKnown == 'Known'",
    
    numericInput(inputId = "popuSD",
                 label = "Population Standard Deviation Value:",
                 value = 0, min = 0, step = 0.00001))
  )
  
  
  
