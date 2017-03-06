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
             "Lipid Class Composition" = list(sheetname = "Lipid Class Composition", ylab = "Composition", col_title = "Lipid class", row_selection = "none", type = "class"))
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

    df <- reactive({
      # read all excel files
      if (is.null(myfiles())) {
        return(NULL)
      } else {
        # add datapath and sheet_names to dataframe
        df <- data_frame(datapath = rep(myfiles()$datapath, each = length(sheet_names)), 
                         sheet_names = rep(sheet_names, nrow(myfiles())))
        # read all the files
        df <- df %>%
          mutate(batch = rep(seq(1, length(unique(datapath))), each = length(unique(sheet_names)))) %>%
          mutate(data = map2(.x = datapath,
                             .y = sheet_names,
                             .f = ~ read_excel(path = .x,
                                               sheet = .y,
                                               col_names = TRUE,
                                               na = ".") %>%
                               filter(! (grepl(Name, pattern = "QC_SPIKE*")) & grepl(Name, pattern = "QC-*")))) %>% # remove QC spike and select the normal QC samples
          mutate(data = map2(.x = data,
                             .y = batch,
                             .f = ~ mutate(.x, batch = .y)))
      }
  })
    
    all <- reactive({
      if (is.null(df())) {
        return(NULL)
      } else {
        # get the parameters
        myparams <- params()
        
        # merge into one dataframe
        all <- df() %>%
          filter(sheet_names == myparams$sheetname) %>%
          select(data) %>%
          unnest
        
        # looking at the lipid classes or species
        switch(myparams$type,
               "class" = {all %>%
                   gather(lipid, value, -Name, -batch)  %>%
                   mutate(Name = factor(Name, levels = unique(Name)),
                          lipid = as.factor(lipid),
                          batch = as.factor(batch)) },
               "species" = {
                 all %>%
                   gather(lipid, value, -Name, -batch)  %>%
                   mutate(lipid_class = as.factor(gsub(x = lipid,
                                                       pattern = "[\\(]{0,1}[0-9.].*",
                                                       replacement = ""))) %>%
                   mutate(Name = factor(Name, levels = unique(Name)),
                          lipid = factor(lipid, levels = unique(lipid)),
                          batch = as.factor(batch))})
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
                     "species" = all() %>% filter(lipid_class == input$select_class))
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

    output$my_plot <- renderPlot({
      if (is.null(all())) {
        return(NULL)
      } else {
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
                       }
                     })
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