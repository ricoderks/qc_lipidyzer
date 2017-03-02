library(shiny)

shinyUI(fluidPage(
  ## Application title
  titlePanel("QC overview lipidyzer"),
  
  ## Sidebar 
  sidebarLayout(
    sidebarPanel(width = 3,
                 fileInput("result_files", 
                           "Choose XLSX files from lipidyzer",
                           multiple = TRUE),
                 selectInput(inputId = "select_sheet",
                             label = "Select a sheet to view :",
                             choices = c("Lipid class concentration" = "Lipid Class Concentration",
                                         "Lipid class composition" = "Lipid Class Composition"),
                             selected = "Lipid Class Concentration")
    ),
    
    # show my stuff
    mainPanel(textOutput("my_text"),
              DT::dataTableOutput("my_table"),
              br(),
              plotOutput("my_plot", height = "600px")
    )
  )
))
