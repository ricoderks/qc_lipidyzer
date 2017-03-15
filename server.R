library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(DT)
library(purrr)
library(tibble)

sheet_names <- c("Lipid Species Concentrations",
                 "Lipid Species Composition",
                 "Lipid Class Concentration",
                 "Lipid Class Composition",
                 "Fatty Acid Concentration",
                 "Fatty Acid Composition")

shinyServer(
  function(input, output) {
    params <- reactive({
      switch(input$select_sheet,
             "Lipid Species Concentration" = list(sheetname = "Lipid Species Concentrations", ylab = "Concentration", col_title = "Lipid species",row_selection = "multiple", type = "species"),
             "Lipid Species Composition" = list(sheetname = "Lipid Species Composition", ylab = "Composition", col_title = "Lipid species", row_selection = "multiple", type = "species"),
             "Lipid Class Concentration" = list(sheetname = "Lipid Class Concentration", ylab = "Concentration", col_title = "Lipid class", row_selection = "none", type = "class"),
             "Lipid Class Composition" = list(sheetname = "Lipid Class Composition", ylab = "Composition", col_title = "Lipid class", row_selection = "none", type = "class"),
             "Fatty Acid Concentration" = list(sheetname = "Fatty Acid Concentration", ylab = "Concentration", col_title = "FA species", row_selection = "multiple", type = "fa_species"),
             "Fatty Acid Composition" = list(sheetname = "Fatty Acid Composition", ylab = "Composition", col_title = "FA species", row_selection = "multiple", type = "fa_species"))
    })
    
    params_qc <- reactive({
      switch(input$select_qc,
             "QC_normal" = list(qc = "QC-[0-9]*"),
             "QC_spike" = list(qc = "QC_SPIKE*"))
    })

    myfiles <- reactive({
      # get the filenames
      my_files <- input$result_files

      if (is.null(my_files)) {
        # no files selected yet
        return(NULL)
      } else {
        # this is nescessary for readxl to read the excel files
        file.rename(my_files$datapath,
                    paste0(my_files$datapath, ".xlsx"))
        my_files$datapath = paste0(my_files$datapath, ".xlsx")

        return(my_files)
      }
    })

    output$fileUploaded <- reactive({
      return(!is.null(myfiles()))
    })
    outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
    
    output$report <- downloadHandler(
      filename = "report.html",
      content = function(file) {
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy(from = "report.Rmd", to = tempReport, overwrite = TRUE)
        
        params <- list(myfiles = myfiles())
        
        withProgress(message = "Generating report..." ,{
        rmarkdown::render(input = tempReport, 
                          output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv()))
        })
      }
    )
    
    df <- reactive({
      # read all excel files
      if (is.null(myfiles())) {
        return(NULL)
      } else {
        #myparams_qc <- params_qc()
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
      }
  })
    
    all <- reactive({
      if (is.null(df())) {
        return(NULL)
      } else {
        # get the parameters
        myparams <- params()
        myparams_qc <- params_qc()
        
        # merge into one dataframe
        all <- df() %>%
          filter(sheet_names == myparams$sheetname) %>%
          select(data) %>%
          unnest %>%
          filter(grepl(Name, pattern = myparams_qc$qc))     #  select which qc sample you want to see
        
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
      }
    })

    output$my_table <- DT::renderDataTable({
      if (is.null(all())) {
        return(NULL)
      } else {
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
                    options = list(dom = "pt"), selection = myparams$row_selection) %>%             # remove the search field
          formatRound(columns = c("mean", "stdev", "RSD"), digits = 2)
      }
    })

    output$info <- renderPrint({
      if (!is.null(input$plot_click)) {
        nearPoints(all(), input$plot_click, threshold = 10, maxpoints = 1)      
      }
    })
    
    output$my_plot <- renderPlot({
      if (is.null(all())) {
        return(NULL)
      } else {
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
        if (mygraph == "line") {
          p <- all %>%
            ggplot() +
            geom_point(aes(x = Name,
                           y = value,
                           color = lipid,
                           shape = batch),
                       size = 3) +
            geom_path(aes(x = Name,
                          y = value,
                          color = lipid,
                          group = lipid))
          if (nrow(myfiles()) == 1) {
            p <- p + guides(color = "none",
                            shape = "none")
          } else {
            p <- p + guides(color = "none",
                            shape = guide_legend(title = "Batch")) 
          }
          p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            xlab("QC sample ID") +
            ylab(params()$ylab)
          if (myparams$type == "class") {
            p <- p + facet_wrap(~ lipid, ncol = 3, scales = "free_y")
          }
        } else {
          p <- all %>%
            group_by(lipid) %>%
            mutate(mean = mean(value, na.rm = TRUE),
                   stdev = sd(value, na.rm = TRUE),
                   zscore = abs((value - mean) / stdev),
                   RSD = round((stdev / mean * 100), digits = 1 )) %>%
            ggplot() +
            geom_bar(aes(x = Name, 
                         y = value,
                         fill = zscore,
                         linetype = batch_bar),
                     stat = "identity",
                     color = "black") +
            geom_line(aes(x = Name,
                          y = mean,
                          group = 1),
                      color = "black",
                      size = 1) +
            facet_wrap(~ lipid, ncol = 3, scales = "free_y") +
            scale_fill_gradientn(colors = c("green", "yellow", "red"),
                                 values = scales::rescale(x = c(0, 2, 4))) +     # needs to be scaled between 0 and 1!!!
            guides(linetype = "none") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            xlab("QC sample ID") +
            ylab(params()$ylab)
        }
        p
      }
    })
    
    # output$help_session <- renderPrint({
    #   sessionInfo()
    # })
    
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