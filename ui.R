library(shiny)

shinyUI(fluidPage(
  ## Application title
  titlePanel("QC overview lipidyzer"),
  
  ## Sidebar 
  sidebarLayout(
    sidebarPanel(width = 3,
                 fileInput("result_files", 
                           "Select XLSX files from lipidyzer",
                           multiple = TRUE),
                 selectInput(inputId = "select_sheet",
                             label = "Select a sheet to view :",
                             choices = c("Lipid species concentration" = "Lipid Species Concentration",
                                         "Lipid species composition" = "Lipid Species Composition",
                                         "Lipid class concentration" = "Lipid Class Concentration",
                                         "Lipid class composition" = "Lipid Class Composition"),
                             selected = "Lipid Class Concentration"),
                 conditionalPanel(condition = "input.select_sheet == 'Lipid Species Concentration' || input.select_sheet == 'Lipid Species Composition'",
                                  selectInput(inputId = "select_species",
                                              label = "Select a species :",
                                              choices = c("CE" = "CE",
                                                          "DAG" = "DAG",
                                                          "FFA" = "FFA",
                                                          "LPC" = "LPC",
                                                          "LPE" = "LPE",
                                                          "PC" = "PC",
                                                          "PE" = "PE",
                                                          "SM" = "SM",
                                                          "TAG" = "TAG"),
                                              selected = "CE")
                                  )
    ),
    
    # show my stuff
    mainPanel(
      tabsetPanel(
        tabPanel("Results",
        #textOutput("my_text"),
              DT::dataTableOutput("my_table"),
              br(),
              plotOutput("my_plot", height = "550px")),
        tabPanel("Help",
                 h3("Introduction"),
                 p("This is a first version of the web application to visualize the QC samples from a Lipidyzer study. 
                   There are some prerequisites you have to fulfill :"),
                 p("* The order of the processing of the files is alphabetically."),
                 p("* QC samples ID needs to start with ", strong("QC-"), "."),
                 p("* The sheetnames in the xlsx file should not be changed. The names are :",
                   HTML("<ul><li>Lipid Species Concentration</li><li>Lipid Species Composition</li><li>Lipid Class Concentration</li>
<li>Lipid Class Composition</li><li>Fatty Acid Concentration</li><li>Fatty Acid Composition</li>
                        </ul>")),
                 br(),
                 p("To show the QC results from the Lipidyzer study load the Excel output files (.xlsx). 
                   The QC spike samples will be automatically remove from the samples."),
                 br(),
                 br(),
                 br()))
                 #p("This still needs some styling!"),
                 #textOutput("help_session")))
    )
  )
))
