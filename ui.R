library(shiny)

shinyUI(navbarPage(title = "Lipidyzer analysis", selected = "Files", windowTitle = "Lipidyzer analysis - created with RStudio® and Shiny®",
                   tabPanel("Files",
                            fluidRow(
                              column(12,
                                     inputPanel(
                                       fileInput(inputId = "result_files", 
                                                 label = "Select XLSX files from lipidyzer:",
                                                 multiple = TRUE),
                                       p(strong("Files uploaded :")),
                                       textOutput(outputId = "show_result_files")
                                     ))),
                            fluidRow(
                              column(12,
                                     inputPanel(
                                       p(HTML("Only the <b>first sheet</b> will be read. See HELP for more information about reading a meta data file.")),
                                       fileInput(inputId = "meta_file", 
                                                 label = "Select meta data file:",
                                                 multiple = FALSE),
                                       p(strong("File uploaded :")),
                                       textOutput(outputId = "show_meta_file"),
                                       uiOutput(outputId = "select_sample_id")    # here a pull down list and a button will be placed for merging
                                     )))),
                   navbarMenu("Data",
                              tabPanel("Raw data",
                                       fluidRow(
                                         column(12,
                                                selectInput(inputId = "select_sheet_tbl",
                                                            label = "Select a sheet:",
                                                            choices = c("Lipid species concentration" = "Lipid Species Concentrations",
                                                                        "Lipid species composition" = "Lipid Species Composition",
                                                                        "Lipid class concentration" = "Lipid Class Concentration",
                                                                        "Lipid class composition" = "Lipid Class Composition",
                                                                        "Fatty acid concentration" = "Fatty Acid Concentration",
                                                                        "Fatty acid composition" = "Fatty Acid Composition"),
                                                            selected = "Lipid Class Concentration"),
                                                div(DT::dataTableOutput("tbl_all_data"), style = "font-size: 80%")
                                         ))),
                              tabPanel("Meta data",
                                       fluidRow(
                                         column(12,
                                                div(DT::dataTableOutput("meta_data"), style = "font-size: 80%")
                                         )
                                       ))),
                   tabPanel("Analysis",
                            fluidRow(
                              column(8,
                                     inputPanel(
                                       radioButtons(inputId = "select_sample_type",
                                                   label = "Select sample type:",
                                                   choices = c("normal QC" = "QC_normal",
                                                               "QC spike" = "QC_spike",
                                                               "Samples" = "Samples"),
                                                   selected = "Samples"),
                                       selectInput(inputId = "select_sheet",
                                                   label = "Select a sheet:",
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
                                                        conditionalPanel(condition = "input.select_sample_type == 'QC_normal' || input.select_sample_type == 'QC_spike'",
                                                                         radioButtons(inputId = "select_graph",
                                                                                      label = "Select QC graph style:",
                                                                                      choices = c("Line" = "line",
                                                                                                  "Bar" = "bar"),
                                                                                      selected = "line"))
                                                        ),
                                       conditionalPanel(condition = "output.fileUploaded == true && (input.select_sample_type == 'QC_normal' || input.select_sample_type == 'QC_spike')",
                                                        downloadButton(outputId = "report",
                                                                       label = "Generate report")),
                                       conditionalPanel(condition = "input.select_sample_type == 'Samples'",
                                                        selectInput(inputId = "select_group_plot",
                                                                    label = "Select a group :",
                                                                    choices = c("none" = "none")),
                                                        conditionalPanel(condition = "input.select_group_plot != 'none'",
                                                                       radioButtons(inputId = "select_sample_graph",
                                                                                    label = "Select graph style:",
                                                                                    choices = c("Heatmap" = "heatmap",
                                                                                                "Bar graph" = "bar"),
                                                                                    selected = "heatmap")))
                                       )),
                              column(4,
                                     div(DT::dataTableOutput("result_table"), style = "font-size: 80%"))),
                            fluidRow(
                              column(12,
                                     plotOutput(outputId = "my_plot", height = "800px", 
                                                # for now no zooming
                                                 brush = brushOpts(id = "plot_brush",
                                                                   direction = "y",
                                                                   delayType = "debounce",
                                                                   resetOnNew = TRUE),
                                                 dblclick = "plot_dblclick",
                                                hover = hoverOpts(id = "plot_hover",
                                                                  delay = 0)),
                                     uiOutput(outputId = "hover_info")))),
                   navbarMenu("Help",
                              tabPanel("Introduction",
                                       h3("Introduction"),
                                       p("This is a first version of the web application to visualize the results from a Lipidyzer study."),
                                       p(strong("Prerequisites :")),
                                       p(HTML("<ul>
                                                <li>The order of the processing of the files is alphabetically.</li>
                                                <li>QC spike sample ID needs to start with <b>QC_SPIKE</b>.</li>
                                                <li>Normal QC samples ID needs to start with <b>QC-</b>.</li>
                                                <li>Make sure sample ID's don't start with <b>QC</b>!!</li>
                                                <li>The sheetnames in the xlsx file should not be changed. The names are :</li>
                                                  <ul>
                                                    <li>Lipid Species Concentrations</li>
                                                    <li>Lipid Species Composition</li>
                                                    <li>Lipid Class Concentration</li>
                                                    <li>Lipid Class Composition</li>
                                                    <li>Fatty Acid Concentration</li>
                                                    <li>Fatty Acid Composition</li>
                                                  </ul>
                                                 </ul>"))),
                              tabPanel("First start",
                                       h3("Start"),
                                       p(HTML("<ul>
                                                 <li>To show the results from a Lipidyzer study load the Excel output file (.xlsx). Multiple files at once is possible.</li>
                                                 <li>Select which sheet from the Excel file you want to see (default <i>Lipid Class Concentration</i>).</li>
                                                 <li>Select which sample type you want to see (default is <i>Samples</i>).</li>
                                                 <li>For QC samples : if you select a <b>Lipid Class *</b> sheet, you can choose between a line graph or bar graph.</li>
                                                 <li>If you select a <b>Lipid Species *</b> or <b>Fatty Acid *</b> sheet, select which lipid class you want to see (default <i>CE</i>).</li>
                                                 <li>If you select a <b>Lipid Species *</b> or <b>Fatty Acid *</b> sheet it is possible to plot the individual species by clicking on the row in the table. Multiple selections is possible.</li>
                                               </ul>"))),
                              tabPanel("Meta data",
                                       h4("Meta data"),
                                       p("Note: So far there is no error checking on merging meta data with the results data! "),
                                       p(HTML("The lipidyzer results can be combined with meta data. For this an Excel sheet containing the meta data can be read. There are some prerequisites for this file :
                                               <ul>
                                                 <li>Only the first sheet is read.</li>
                                                 <li>Column names should contain NO spaces</li>
                                                 <li>Avoid special characters (e.g. #) in the column names</li>
                                                 <li>Avoid complete column / row formatting. This will import empty rows / columns</li>
                                              </ul>")),
                                       p(HTML("After clicking the merge button, you can select in the <b>Analysis</b> tab a column for which you want to split the data in the heatmap."))),
                              tabPanel("Graphs", 
                                       h3("Graph"),
                                       p(HTML("There are several graphs available depending on the sample type you choose. </br>
                                                If you select <b>QC samples</b> you can choose between line and
                                                bar graphs if you select a class sheet, otherwise only the line graph is available.</br>
                                                For the <b>Samples</b> there is a heatmap available. When you merge your data with meta data and select a column
                                                you can also generate bar graphs.
                                              <ul>
                                                <li>Hover over a data point to get more information.</li>
                                              </ul>"))),
                              tabPanel("Report",
                                       h3("Report"),
                                       p("For QC samples a report with all graphs and tables can be downloaded by clicking the ", strong("Generate report"), " button. This may take 10-20 seconds. 
                                          This button is available if the sample type is selected to one of the QC samples."),
                                       p("For the moment this is not working. This will be fixed later.", 
                                         a("See issue #2", href = "https://git.lumc.nl/rjederks/lipidyzer/issues/2", target = "_blank"),
                                         " for more information.")),
                              "----",
                              tabPanel("About", 
                                       h3("Issues"),
                                       p("If you have any issue please send me an email or go to the ", a("issue tracker.", href = "https://git.lumc.nl/rjederks/lipidyzer/issues", target = "_blank")),
                                       h3("Session info"),
                                       htmlOutput("help_session"))
                   )
)
)


