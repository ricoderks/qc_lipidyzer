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
    # params contains several general parameters linked to which excel sheet is selected
    params <- reactive({
      switch(input$select_sheet,
             "Lipid Species Concentration" = list(sheetname = "Lipid Species Concentrations", ylab = "Concentration", col_title = "Lipid species",row_selection = "multiple", type = "species"),
             "Lipid Species Composition" = list(sheetname = "Lipid Species Composition", ylab = "Composition", col_title = "Lipid species", row_selection = "multiple", type = "species"),
             "Lipid Class Concentration" = list(sheetname = "Lipid Class Concentration", ylab = "Concentration", col_title = "Lipid class", row_selection = "none", type = "class"),
             "Lipid Class Composition" = list(sheetname = "Lipid Class Composition", ylab = "Composition", col_title = "Lipid class", row_selection = "none", type = "class"),
             "Fatty Acid Concentration" = list(sheetname = "Fatty Acid Concentration", ylab = "Concentration", col_title = "FA species", row_selection = "multiple", type = "fa_species"),
             "Fatty Acid Composition" = list(sheetname = "Fatty Acid Composition", ylab = "Composition", col_title = "FA species", row_selection = "multiple", type = "fa_species"))
    })
    
    # sample_type contains some information depending on which sample type is selected
    sample_type <- reactive({
      switch(input$select_sample_type,
             "QC_normal" = list(type = "qc", qc = "QC-[0-9]*", invert = FALSE),
             "QC_spike" = list(type = "qc", qc = "QC_SPIKE*", invert = FALSE),
             "Samples" = list(type = "sample", qc = "QC*", invert = TRUE))
    })

    myfiles <- reactive({
      # check if there are already filenames
      req(input$result_files)
      
      # get the filenames
      my_files <- input$result_files
      
      # this is nescessary for readxl to read the excel files
      file.rename(my_files$datapath,
                  paste0(my_files$datapath, ".xlsx"))
      my_files$datapath = paste0(my_files$datapath, ".xlsx")
      
      return(my_files)
    })

    output$fileUploaded <- reactive({
      req(myfiles())
      return(TRUE)
    })
    
    outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
    
    output$report <- downloadHandler(
      filename = "report.html",
      content = function(file) {
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy(from = "report.Rmd", to = tempReport, overwrite = TRUE)
        
        params <- list(myfiles = myfiles())
        
        withProgress(message = "Generating report...",
                     detail = "This may take a while!", {
        rmarkdown::render(input = tempReport, 
                          output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv()))
        })
      }
    )
    
    df <- reactive({
      req(myfiles())
      
      # read all excel files
      # add datapath and sheet_names to dataframe
      df <- data_frame(datapath = rep(myfiles()$datapath, each = length(sheet_names)), 
                       sheet_names = rep(sheet_names, nrow(myfiles())))
      # read all the files
      df <- df %>%
        mutate(batch = rep(seq(1, length(unique(datapath))), each = length(unique(sheet_names)))) %>%
        mutate(batch_bar = rep(c(1, 2), each = length(unique(sheet_names)), length.out = length(unique(sheet_names)) * length(unique(datapath)))) %>%
        mutate(data = map2(.x = datapath,
                           .y = sheet_names,
                           .f = ~ read_excel(path = .x,
                                             sheet = .y,
                                             col_names = TRUE,
                                             na = "."))) %>%
        mutate(data = map2(.x = data,
                           .y = batch,
                           .f = ~ mutate(.x, batch = .y))) %>%
        mutate(data = map2(.x = data,
                           .y = batch_bar,
                           .f = ~ mutate(.x, batch_bar = .y)))
      
      # update the lipid class selection to the classes actual present in the QC files
      # get the columns names of dataframe 3
      class_names <- colnames(df$data[[3]])
      # only keep the real lipid class names
      class_names <- subset(class_names, !(class_names %in% c("Name", "batch", "batch_bar")))
      # update the selectInput
      updateSelectInput(session = session,
                        inputId = "select_class",
                        label = "Select a lipid class:",
                        choices = class_names)
      
      # return the dataframe
      return(df)
  })
    
    all <- reactive({
      req(df())
      req(params())
      req(sample_type())
      
      # get the parameters
      myparams <- params()
      my_sample_type <- sample_type()
      
      # merge into one dataframe
      all <- df() %>%
        filter(sheet_names == myparams$sheetname) %>%
        select(data) %>%
        unnest %>%
        filter((my_sample_type$invert == TRUE & !grepl(x = Name, pattern = my_sample_type$qc)) |
                 (my_sample_type$invert == FALSE & grepl(x = Name, pattern = my_sample_type$qc)))     #  select which samples you want to see
      
      # looking at the lipid classes or species
      switch(myparams$type,
             "class" = {all %>%
                 gather(lipid, value, -Name, -batch, -batch_bar)  %>%
                 mutate(Name = factor(Name, levels = unique(Name)),
                        lipid = as.factor(lipid),
                        batch = as.factor(batch),
                        batch_bar = as.factor(batch_bar)) },
             "species" = {
               all %>%
                 gather(lipid, value, -Name, -batch)  %>%
                 mutate(lipid_class = as.factor(gsub(x = lipid,
                                                     pattern = "[\\(]{0,1}[0-9.].*",
                                                     replacement = ""))) %>%
                 mutate(Name = factor(Name, levels = unique(Name)),
                        lipid = factor(lipid, levels = unique(lipid)),
                        batch = as.factor(batch))},
             "fa_species" = {
               all %>%
                 gather(lipid, value, -Name, -batch)  %>%
                 mutate(lipid_class = as.factor(gsub(x = lipid,
                                                     pattern = "[\\(](FA).*",
                                                     replacement = ""))) %>%
                 mutate(Name = factor(Name, levels = unique(Name)),
                        lipid = as.factor(lipid),
                        batch = as.factor(batch))
             })
    })

    output$my_table <- DT::renderDataTable({
      req(all())
      req(params())
      
      myparams <- params()
      #do the stats
      all <- switch(myparams$type,
                    "class" = all(),
                    "species" = all() %>% filter(lipid_class == input$select_class),
                    "fa_species" = all() %>% filter(lipid_class == input$select_class))
      all %>%
        select(lipid, value, Name) %>%
        group_by(lipid) %>%
        summarise(mean = mean(value, na.rm = TRUE),
                  stdev = sd(value, na.rm = TRUE),
                  RSD = stdev / mean * 100) %>%
        datatable(colnames = c(params()$col_title, "Mean", "St.dev.", "RSD [%]"),
                  options = list(dom = "tp"), 
                  selection = myparams$row_selection,            # remove the search field
                  rownames = FALSE) %>% 
        formatRound(columns = c("mean", "stdev", "RSD"), digits = 2)
    })

    output$info <- renderDataTable({
      req(all())
      req(params())
      
      myparams <- params()
      all <- switch(myparams$type,
                    "class" = all(),
                    "species" = {
                      if (length(input$my_table_rows_selected) == 0) {
                        all <- all() %>% filter(lipid_class == input$select_class) 
                      } else {
                        all <- all() %>% filter(lipid_class == input$select_class)
                        x <- levels(droplevels(all$lipid))[input$my_table_rows_selected]
                        all %>% filter(lipid %in% x)
                      }},
                    "fa_species" = {
                      if (length(input$my_table_rows_selected) == 0) {
                        all <- all() %>% filter(lipid_class == input$select_class) 
                      } else {
                        all <- all() %>% filter(lipid_class == input$select_class)
                        x <- levels(droplevels(all$lipid))[input$my_table_rows_selected]
                        all %>% filter(lipid %in% x)
                      }})
      
      if (!is.null(input$plot_click)) {
        data_point <- nearPoints(all, input$plot_click, threshold = 10, maxpoints = 1)
      }
      if (!is.null(input$plot_brush)) {
        data_point <- brushedPoints(all, input$plot_brush)
      }
      if (exists("data_point")) {
        if (nrow(data_point) > 0) {
          data_point %>% 
            select(Name, lipid, value) %>%
            filter(Name != "") %>%    # remove some empty rows
            datatable(options = list(dom="tp"), selection = "none", rownames = FALSE)
        }
      } 
    })

    output$my_plot <- renderPlot({
      req(all())
      req(params())
      req(input$select_graph)
      req(myfiles())
      req(sample_type())
      
      myparams <- params()
      mygraph <- input$select_graph
      
      all <- switch(myparams$type,
                    "class" = all(),
                    "species" = {
                      mygraph <- "line"
                      if (length(input$my_table_rows_selected) == 0) {
                        all <- all() %>% filter(lipid_class == input$select_class) 
                      } else {
                        all <- all() %>% filter(lipid_class == input$select_class)
                        x <- levels(droplevels(all$lipid))[input$my_table_rows_selected]
                        all %>% filter(lipid %in% x)
                      }},
                    "fa_species" = {
                      mygraph <- "line"
                      if (length(input$my_table_rows_selected) == 0) {
                        all <- all() %>% filter(lipid_class == input$select_class) 
                      } else {
                        all <- all() %>% filter(lipid_class == input$select_class)
                        x <- levels(droplevels(all$lipid))[input$my_table_rows_selected]
                        all %>% filter(lipid %in% x)
                      }})
      # select the correct graph
      switch(sample_type()$type,
             "qc" = {
               switch(mygraph,
                      "line" = { p <- qc_line(data = all, my_files = myfiles(), params = myparams) },
                      "bar" = { p <- qc_bar(data = all, my_files = myfiles(), params = myparams) } )
             },
             "sample" = { p <- qc_line(data = all, my_files = myfiles(), params = myparams) } )
      p
    })
    
    output$help_session <- renderPrint({
      print(sessionInfo())
    })
    
    # output$my_text <- renderText({
    #    if (is.null(myfiles())) {
    #      return("")
    #    } else {
    #      tmp <- df() %>% filter(lipid_class == input$select_class)
    #      x <-levels(droplevels(tmp$lipid))[input$my_table_rows_selected]
    #      paste(input$select_class, x, sep = "\n")
    #    }
    #  })
    
  })