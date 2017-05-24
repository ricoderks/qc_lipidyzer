library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(DT)
library(purrr)
library(tibble)

# default sheetnames used in the result files from the lipidyzer
sheet_names <- c("Lipid Species Concentrations",
                 "Lipid Species Composition",
                 "Lipid Class Concentration",
                 "Lipid Class Composition",
                 "Fatty Acid Concentration",
                 "Fatty Acid Composition")

# load some extra helper functions
source("helper.R")

shinyServer(
  function(input, output, session) {
    # store all data in a reactive object
    values <- reactiveValues(results_data = NULL,      # store the raw results
                             meta_data = NULL,         # store the meta data
                             merged_data = NULL,       # store the merged data (results and meta data)
                             selected_data = NULL,     # data from the selected data sheet and selected parameters
                             params = NULL,            # store the parameters for selecting a sheet
                             sample_type = NULL)       # store the sample type selection
    
    
   # sample_type contains some information depending on which sample type is selected
    #sample_type <- eventReactive(input$select_sample_type, {
    observe({
      req(values$results_data)
      
      values$sample_type <- switch(input$select_sample_type,
             "QC_normal" = list(type = "qc", qc = "QC-[0-9]*", invert = FALSE),
             "QC_spike" = list(type = "qc", qc = "QC_SPIKE*", invert = FALSE),
             "Samples" = list(type = "sample", qc = "QC*", invert = TRUE))
      
      values$params <- switch(input$select_sheet,
                              "Lipid Species Concentration" = list(sheetname = "Lipid Species Concentrations", ylab = "Concentration", col_title = "Lipid species",row_selection = "multiple", type = "species"),
                              "Lipid Species Composition" = list(sheetname = "Lipid Species Composition", ylab = "Composition", col_title = "Lipid species", row_selection = "multiple", type = "species"),
                              "Lipid Class Concentration" = list(sheetname = "Lipid Class Concentration", ylab = "Concentration", col_title = "Lipid class", row_selection = "none", type = "class"),
                              "Lipid Class Composition" = list(sheetname = "Lipid Class Composition", ylab = "Composition", col_title = "Lipid class", row_selection = "none", type = "class"),
                              "Fatty Acid Concentration" = list(sheetname = "Fatty Acid Concentration", ylab = "Concentration", col_title = "FA species", row_selection = "multiple", type = "fa_species"),
                              "Fatty Acid Composition" = list(sheetname = "Fatty Acid Composition", ylab = "Composition", col_title = "FA species", row_selection = "multiple", type = "fa_species"))
      
      if (!is.null(values$results_data)) {
        if (is.null(values$merged_data)) {
          df <- values$results_data
          df %>% 
            mutate(data = map(.x = data,
                              .f = ~ select(.x, -meas_order))) -> df
        } else {
          if (input$select_group_plot == "none") {
            df <- values$results_data
            df %>% 
              mutate(data = map(.x = data,
                                .f = ~ select(.x, -meas_order))) -> df
          } else {
            df <- values$merged_data
            my_group_col <- input$select_group_plot
            # get column names
            group_names <- colnames(values$meta_data)
            # remove the column name used for merging, this one is not present anyway
            # and remove the column selected for grouping
            group_names <- subset(group_names, !(group_names %in% c(input$select_sampleID_col)))
            
            # if no group is selected I need to remove all extra columns from the merge i.e. group_names,
            # add '-' to the names to make select remove them
            group_names <- c(paste0("-", group_names), "-meas_order")
            
            # mutate the selected group column to a new column and remove the original one
            # make the column factor as well
            dots <- list(paste0("as.factor(", my_group_col, ")"))

            df %>%
              mutate(data = map(.x = data,
                                .f = ~ mutate_(.x, .dots = setNames(dots, "my_group_col")))) %>%
              mutate(data = map(.x = data,
                                .f = ~ select_(.x, .dots = as.list(group_names)))) -> df
          }
        }
            
        df %>%
          filter(sheet_names == values$params$sheetname) %>%
          select(data) %>%
          unnest %>%
          filter((values$sample_type$invert == TRUE & !grepl(x = Name, pattern = values$sample_type$qc)) |
                   (values$sample_type$invert == FALSE & grepl(x = Name, pattern = values$sample_type$qc))) -> df
        if(input$select_group_plot == "none") {
          df %>%
            gather(lipid, value, -Name, -batch, -batch_bar) -> df
        } else {
          df %>%
            gather(lipid, value, -Name, -batch, -batch_bar, -my_group_col) -> df
        }
        
        df <- switch(values$params$type,
                  "class" = {df %>%
                      mutate(Name = factor(Name, levels = unique(Name)),
                             lipid = as.factor(lipid)) },
                  "species" = {
                    df %>%
                      mutate(lipid_class = as.factor(gsub(x = lipid,
                                                          pattern = "[\\(]{0,1}[0-9.].*",
                                                          replacement = ""))) %>%
                      mutate(Name = factor(Name, levels = unique(Name)),
                             lipid = factor(lipid, levels = unique(lipid)),
                             batch = as.factor(batch))},
                  "fa_species" = {
                    df %>%
                      mutate(lipid_class = as.factor(gsub(x = lipid,
                                                          pattern = "[\\(](FA).*",
                                                          replacement = ""))) %>%
                      mutate(Name = factor(Name, levels = unique(Name)),
                             lipid = as.factor(lipid),
                             batch = as.factor(batch))
                  })
        values$selected_data <- df
      }
    })
    
    ############################ meta data stuff ####################################################    
    
    # prepare meta data filename for reading
    meta_file <- reactive({
      # check if there are already filename
      req(input$meta_file)
      
      # get the filename
      meta_file <- input$meta_file
      
      # this is nescessary for readxl to read the excel files
      file.rename(meta_file$datapath,
                  paste0(meta_file$datapath, ".xlsx"))
      meta_file$datapath = paste0(meta_file$datapath, ".xlsx")
      
      return(meta_file)
    })
    
    # show the filename of the uploaded meta data files on the files tab
    output$show_meta_file <- renderText({
      req(meta_file())
      
      meta_file()$name
    })
    
    # read the meta data from file
    observe({
      # requires the meta data file
      req(meta_file())
      
      # read the excel file and read only the first sheet
      read_excel(path = meta_file()$datapath,
                 sheet = 1,
                 col_names = TRUE) -> values$meta_data
    })
    
    # show the meta data in a table in the data tab
    # this is temporary (or I will make different tabs in the data tab)
    output$meta_data <- DT::renderDataTable({
      req(values$selected_data)

      values$selected_data %>%
        datatable(selection = "none",
                  options = list(dom = "tp",
                                 pageLength = 25))
    })

    # show pull down select for select the sample ID column to merge the result files
    # with the meta files
    output$select_sample_id <- renderUI({
      req(values$meta_data)
      
      meta_colnames <- colnames(values$meta_data)
      
      tagList(
        selectInput(inputId = "select_sampleID_col",
                    label = "Select sample ID column to merge :",
                    choices = meta_colnames),
        actionButton(inputId = "merge_data",
                     label = "Merge")
      )
    })
    ##################################### end meta data stuff ########################################
    
    ##################################### results file ###########################################
    
    # prepare result filenames for reading
    result_files <- reactive({
      # check if there are already filenames
      req(input$result_files)
      
      # get the filenames
      result_files <- input$result_files
      
      # this is nescessary for readxl to read the excel files
      file.rename(result_files$datapath,
                  paste0(result_files$datapath, ".xlsx"))
      result_files$datapath = paste0(result_files$datapath, ".xlsx")
      
      return(result_files)
    })
    
    # show the filenames of the uploaded result files
    output$show_result_files <- renderText({
      req(result_files())
      
      result_files()$name
    })
    
    # read all datafiles and structure the dataframe
    #df_all <- reactive({
    observe({
      req(result_files())
      
      # prepare data frame
      tmp <- data_frame(sheetnames = sheet_names)
      
      result_files()$datapath %>%
        list() %>%
        rep(nrow(tmp)) %>%
        data_frame(datapath = .) %>%
        bind_cols(tmp, .) -> df_all
      
      # read all excel files
      df_all %>%
        mutate(data = map2(.x = datapath,
                           .y = sheetnames,
                           .f = ~ map2(.x = .x,
                                       .y = .y,
                                       .f = ~read_excel(path = .x,
                                                        sheet = .y,
                                                        col_names = TRUE,
                                                        na = ".") %>%
                                         mutate(meas_order = 1:nrow(.)) %>%
                                         mutate(batch = factor(.x))) %>%
                             reduce(function(...) merge(..., all = TRUE)))) %>%
        mutate(data = map(.x = data,
                          .f = ~ arrange(.x, batch, meas_order))) %>%
        mutate(data = map(.x = data,
                          .f = ~ mutate(.x, batch = factor(batch, labels = 1:length(levels(batch)))))) %>%
        mutate(data = map(.x = data,
                          .f = ~ mutate(.x, batch_bar = as.factor((as.numeric(batch) %% 2))))) -> df_all
      
      values$results_data <- df_all
      
      # update the lipid class selection to the classes actual present in the QC files
      # get the columns names of dataframe 3
      class_names <- colnames(df_all$data[[3]])
      # only keep the real lipid class names
      class_names <- subset(class_names, !(class_names %in% c("Name", "batch", "batch_bar", "meas_order")))
      # update the selectInput
      updateSelectInput(session = session,
                        inputId = "select_class",
                        label = "Select a lipid class:",
                        choices = class_names)
      
      # return the dataframe
      # return(df_all)
    })
    
    # generate table to show the content of the result files
    output$tbl_all_data <- DT::renderDataTable({
      req(input$select_sheet_tbl)
      req(values$results_data)
      
      if (is.null(values$merged_data)) {
        df <- values$results_data  
      } else {
        # if the data is merged, show the merged data
        df <- values$merged_data
      }
      df %>%
        filter(sheet_names == input$select_sheet_tbl) %>%
        select(data) %>%
        unnest() %>%
        datatable(selection = "none",
                  options = list(dom = "ltp",
                                 pageLength = 25))
    })
    
    # flag to show if files are uploaded
    # to show the "Generate report" button
    output$fileUploaded <- reactive({
      req(result_files())
      
      return(TRUE)
    })
    
    # flag to show if files are uploaded 
    outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
    
    ################################### end result files ###########################################
    
    ##################################### QC report ################################################    
    # create report download
    output$report <- downloadHandler(
      filename = "report.html",
      content = function(file) {
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy(from = "report.Rmd", to = tempReport, overwrite = TRUE)
        
        params <- list(result_files = result_files())
        
        withProgress(message = "Generating report...",
                     detail = "This may take a while!", {
                       rmarkdown::render(input = tempReport, 
                                         output_file = file,
                                         params = params,
                                         envir = new.env(parent = globalenv()))
                     })
      }
    )
    ################################ end QC report ####################################################    
   
######################################## merge meta data with results ########################################    
    # is the merge button clicked?!
    data_merged <- eventReactive(input$merge_data, {
      return(TRUE)
    })
    
    #outputOptions(output, "data_merged", suspendWhenHidden = FALSE)
    
    # merge the meta data with the results
    # each sheet should contain the meta data
    observeEvent(input$merge_data, {
      req(values$meta_data)
      req(values$results_data)
      req(input$select_sampleID_col)

      # Name (i.e. sample ID in result file is a character). convert column choosen to character
      dots <- list(paste0("as.character(", input$select_sampleID_col, ")"))
      
      # create a tibble with the meta data
      values$meta_data %>%
        mutate_(.dots = setNames(dots, input$select_sampleID_col)) %>%
        #mutate(sampleID = as.character(sampleID)) %>%
        list() %>%
        rep(nrow(values$results_data)) %>%
        data_frame(sample_info = .) %>%
        # combine the meta data tibble with the results tibble
        bind_cols(values$results_data) %>%
        # do the actual merge of meta data and results for each sheet
        mutate(data = map2(.x = data,
                           .y = sample_info,
                           .f = ~ left_join(x = .x,
                                            y = .y, 
                                            by = c("Name" = input$select_sampleID_col)))) %>%
        # keep only the data
        select(sheetnames, data) -> values$merged_data
      
      # update group selection pull down list
      # get the columns names from the meta data file
      # and remove the column name which was used for merging
      group_names <- c("none", colnames(values$meta_data))
      group_names <- subset(group_names, !(group_names %in% input$select_sampleID_col))
      
      # update the selectInput
      updateSelectInput(session = session,
                        inputId = "select_group_plot",
                        label = "Select a group :",
                        choices = group_names,
                        selected = "none")      
      

    })
########################################## end merge meta data with results ###############################
    
    output$result_table <- DT::renderDataTable({
      req(values$selected_data)
      req(values$params)
      req(input$select_class)
      
      #do the stats
      tbl_df <- switch(values$params$type,
                       "class" = values$selected_data,
                       "species" = values$selected_data %>% filter(lipid_class == input$select_class),
                       "fa_species" = values$selected_data %>% filter(lipid_class == input$select_class))
      tbl_df %>%
        select(lipid, value, Name) %>%
        group_by(lipid) %>%
        summarise(mean = mean(value, na.rm = TRUE),
                  stdev = sd(value, na.rm = TRUE),
                  RSD = stdev / mean * 100) %>%
        datatable(colnames = c(values$params$col_title, "Mean", "St.dev.", "RSD [%]"),
                  options = list(dom = "tp",
                                 pageLength = 5), 
                  selection = values$params$row_selection,            # remove the search field
                  rownames = FALSE) %>% 
        formatRound(columns = c("mean", "stdev", "RSD"), digits = 1)
    })
 
###################################################  plotting ##############################################################   
    # prepare the dataframe for plotting
    plot_df <- reactive({
      req(values$selected_data)
      req(values$params)
      
      plot_df <- switch(values$params$type,
                        "class" = values$selected_data,
                        "species" = {
                          if (length(input$result_table_rows_selected) == 0) {
                            df <- values$selected_data %>% filter(lipid_class == input$select_class) %>% droplevels()
                          } else {
                            df <- values$selected_data %>% filter(lipid_class == input$select_class) %>% droplevels()
                            x <- levels(droplevels(df$lipid))[input$result_table_rows_selected]
                            df %>% filter(lipid %in% x)
                          }},
                        "fa_species" = {
                          if (length(input$result_table_rows_selected) == 0) {
                            df <- values$selected_data %>% filter(lipid_class == input$select_class) %>% droplevels()
                          } else {
                            df <- values$selected_data %>% filter(lipid_class == input$select_class) %>% droplevels()
                            x <- levels(droplevels(df$lipid))[input$result_table_rows_selected]
                            df %>% filter(lipid %in% x)
                          }})
      return(plot_df)
    })

    # do the plotting    
    output$my_plot <- renderPlot({
      req(plot_df())
      req(values$params)
      req(values$sample_type)
      req(input$select_graph)
      
      graph_type <- switch(values$params$type,
                        "class" = input$select_graph,
                        "species" = "line",
                        "fa_species" = "line")
      # select the correct graph
      switch(values$sample_type$type,
             "qc" = {
               switch(graph_type,
                      "line" = { p <- qc_line(data = plot_df(), num_batches = max(as.numeric(plot_df()$batch)), params = values$params) },
                      "bar" = { p <- qc_bar(data = plot_df(), params = values$params) } )
             },
             if (input$select_group_plot == "none") {
               "sample" = { p <- sample_heatmap(data = plot_df(), params = values$params) }
             } else {
               "sample" = { p <- sample_heatmap(data = plot_df(), params = values$params, facet = TRUE) }
             })
      p
    })
    
    # show selected datapoints
    output$info <- renderDataTable({
      req(plot_df())
      req(values$params)
      
      if (!is.null(input$plot_click)) {
        data_point <- nearPoints(df = plot_df(), coordinfo = input$plot_click, threshold = 20, maxpoints = 1)
      }
      if (!is.null(input$plot_brush)) {
        data_point <- brushedPoints(df = plot_df(), brush = input$plot_brush)
      }
      # this is what is returned!!
      if (exists("data_point")) {
        if (nrow(data_point) > 0) {
          data_point %>% 
            select(Name, lipid, value) %>%
            filter(Name != "") %>%    # remove some empty rows
            datatable(options = list(dom = "tp", pageLength = 5), selection = "none", rownames = FALSE)
        }
      } 
    })
 ############################################ end plotting ######################################################
       
    output$help_session <- renderPrint({
      print(sessionInfo())
    })
    
    # output$my_text <- renderText({
    #    if (is.null(result_files())) {
    #      return("")
    #    } else {
    #      tmp <- df() %>% filter(lipid_class == input$select_class)
    #      x <-levels(droplevels(tmp$lipid))[input$my_table_rows_selected]
    #      paste(input$select_class, x, sep = "\n")
    #    }
    #  })
    
  })