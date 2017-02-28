library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("QC overview lipidyzer"),

  # Sidebar 
  sidebarLayout(
    sidebarPanel(h1("Results lipidyzer"),
                 fileInput("result_files", 
                           "Choose XLSX files from lipidyzer")
      
    ),

    # show my stuff
    mainPanel("the main panel for the plots",
      tableOutput("my_table")
    )
  )
))
