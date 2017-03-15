library(shiny)

shinyUI(navbarPage("QC lipidyzer overview", selected = "Results",
                   tabPanel("Results",
                            #textOutput("my_text"),
                            fluidRow(
                              column(4,
                                     inputPanel(
                                       fileInput("result_files", 
                                                 "Select XLSX files from lipidyzer:",
                                                 multiple = TRUE),
                                       radioButtons(inputId = "select_qc",
                                                   label = "Select which QC's you want to see:",
                                                   choices = c("normal QC" = "QC_normal",
                                                               "QC spike" = "QC_spike"),
                                                   selected = "QC_normal"),
                                       selectInput(inputId = "select_sheet",
                                                   label = "Select a sheet to view:",
                                                   choices = c("Lipid species concentration" = "Lipid Species Concentration",
                                                               "Lipid species composition" = "Lipid Species Composition",
                                                               "Lipid class concentration" = "Lipid Class Concentration",
                                                               "Lipid class composition" = "Lipid Class Composition",
                                                               "Fatty acid concentration" = "Fatty Acid Concentration",
                                                               "Fatty acid composition" = "Fatty Acid Composition"),
                                                   selected = "Lipid Class Concentration"),
                                       conditionalPanel(condition = "input.select_sheet == 'Lipid Species Concentration' || input.select_sheet == 'Lipid Species Composition' || input.select_sheet == 'Fatty Acid Concentration' || input.select_sheet == 'Fatty Acid Composition'",
                                                        selectInput(inputId = "select_class",
                                                                    label = "Select a lipid class:",
                                                                    choices = c("CE" = "CE",
                                                                                "DAG" = "DAG",
                                                                                "FFA" = "FFA",
                                                                                "LPC" = "LPC",
                                                                                "LPE" = "LPE",
                                                                                "PC" = "PC",
                                                                                "PE" = "PE",
                                                                                "SM" = "SM",
                                                                                "TAG" = "TAG"),
                                                                    selected = "CE")),
                                       conditionalPanel(condition = "input.select_sheet == 'Lipid Class Concentration' || input.select_sheet == 'Lipid Class Composition'",
                                                        radioButtons(inputId = "select_graph",
                                                                    label = "Select how to show the graph:",
                                                                    choices = c("Line" = "line",
                                                                                "Bar" = "bar"),
                                                                    selected = "line")),
                                       conditionalPanel(condition = "output.fileUploaded == true",
                                                        downloadButton(outputId = "report",
                                                                       label = "Generate report")))),
                              column(4,
                                     div(DT::dataTableOutput("my_table"), style = "font-size: 80%")),
                              column(4, 
                                     div(DT::dataTableOutput("info"), style = "font-size: 80%"))),
                            fluidRow(
                              column(12,
                                     plotOutput("my_plot", #height = "550px", 
                                                click = "plot_click",
                                                brush = "plot_brush")))),
                   tabPanel("Help",
                            h3("Introduction"),
                            p("This is a first version of the web application to visualize the QC samples from a Lipidyzer study. 
                   Currently switching Excel sheet will cause the web application to read all files again. 
                   This will be changed in the future to improve speed."),
                            p(strong("Prerequisites :")),
                            p(HTML("<ul>
                   <li>The order of the processing of the files is alphabetically.</li>
                   <li>QC spike sample ID needs to start with <b>QC_SPIKE</b>.</li>
                   <li>Normal QC samples ID needs to start with <b>QC-</b>.</li>
                   <li>The sheetnames in the xlsx file should not be changed. The names are :</li>
                   <ul>
                        <li>Lipid Species Concentrations</li>
                        <li>Lipid Species Composition</li>
                        <li>Lipid Class Concentration</li>
                        <li>Lipid Class Composition</li>
                        <li>Fatty Acid Concentration</li>
                        <li>Fatty Acid Composition</li>
                        </ul>
                    </ul>")),
                            h3("Start"),
                            p(HTML("<ul>
                        <li>To show the QC results from a Lipidyzer study load the Excel output file (.xlsx). Multiple files at once is possible.</li>
                        <li>Select which sheet from the Excel file you want to see (default <i>Lipid Class Concentration</i>).</li>
                        <li>Select which QC sample you want to see (default is the <i>normal QC samples</i>).</li>
                        <li>If you select a <b>Lipid Class *</b> sheet, you can choose between a line graph or bar graph.</li>
                        <li>If you select a <b>Lipid Species *</b> sheet, select which lipid class you want to see (default <i>CE</i>).</li>
                        <li>If you select a <b>Lipid Species *</b> sheet it is possible to plot the individual species by clicking on the row in the table. Multiple selections is possible.</li>
                        </ul>")),
                            h3("Graph"),
                            p(HTML("<ul>
                                    <li>Click on a data point to get more information.</li>
                                    <li>You can select multiple points by click and drag.</li>
                                    </ul>")),
                            h3("Report"),
                            p("A report with all graphs and tables can be downloaded by clicking the ", strong("Generate report"), " button. This may take 10-20 seconds."),
                            h3("Issues"),
                            p("If you have any issue please send me an email or go to the ", a("issue tracker.", href = "https://git.lumc.nl/rjederks/qc_lipidyzer/issues", target = "_blank")),
                            h3("Session info"),
                            htmlOutput("help_session")
                   )
)
)


