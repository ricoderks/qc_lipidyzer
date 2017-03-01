library(shiny)

shinyUI(fluidPage(
  ## Application title
  titlePanel("QC overview lipidyzer"),
  
  ## Sidebar 
  sidebarLayout(
    sidebarPanel(fileInput("result_files", 
                           "Choose XLSX files from lipidyzer",
                           multiple = TRUE)
    ),
    
    # show my stuff
    mainPanel("the main panel for the plots",
              textOutput("my_text"),
              DT::dataTableOutput("my_table"),
              br(),
              plotOutput("my_plot")
    )
  )
))
