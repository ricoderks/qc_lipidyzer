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
             "Lipid Species Concentration" = list(sheetname = "Lipid Species Concentrations", ylab = "Concentration", col_title = "Lipid species", type = "species"),
             "Lipid Species Composition" = list(sheetname = "Lipid Species Composition", ylab = "Composition", col_title = "Lipid species", type = "species"),
             "Lipid Class Concentration" = list(sheetname = "Lipid Class Concentration", ylab = "Concentration", col_title = "Lipid class", type = "class"),
             "Lipid Class Composition" = list(sheetname = "Lipid Class Composition", ylab = "Composition", col_title = "Lipid class", type = "class"))
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
      # read the excel files
      if (is.null(myfiles())) {
        return(NULL)
      } else {
        myparams <- params()
        # read all data from file
        # merge into 1 data frame
        # do the stats
        df <- myfiles()$name %>%
          data_frame(filenames = .) %>%
          as.tbl %>%
          mutate(batch = 1:n()) %>%
          mutate(data = map2(.x = myfiles()$datapath,
                             .y = batch,
                             .f = ~ read_excel(path = .x,
                                               sheet = myparams$sheetname,
                                               col_names = TRUE,
                                               na = ".") %>%
                               filter(! (grepl(Name, pattern = "QC_SPIKE*")) & grepl(Name, pattern = "QC-*")) %>%
                               mutate(batch = .y))) %>%
          mutate(data = map(.x = data,
                            .f = ~ data.frame(.x)))     # join_all can not handle tibbles!!))

        all <- plyr::join_all(df$data, type = "full")
        
        # looking at the lipid classes
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
                   # mutate(lipid_class = gsub(x = lipid_class,
                   #                           pattern = "PE\\([OP]-",
                   #                           replacement = "PE")) %>%
                   mutate(Name = factor(Name, levels = unique(Name)),
                          lipid = factor(lipid, levels = unique(lipid)),
                          batch = as.factor(batch))})
      }
    })

    output$my_table <- DT::renderDataTable({
      if (is.null(df())) {
        return(NULL)
      } else {
        myparams <- params()
        #do the stats
        df <- switch(myparams$type,
                     "class" = df(),
                     "species" = df() %>% filter(lipid_class == input$select_species))
        df %>%
          select(lipid, value, Name) %>%
          group_by(lipid) %>%
          summarise(mean = mean(value, na.rm = TRUE),
                    stdev = sd(value, na.rm = TRUE),
                    RSD = stdev / mean * 100) %>%
          datatable(colnames = c(params()$col_title, "Mean", "St.dev.", "RSD [%]"),
                    options = list(dom = "pt")) %>%             # remove the search field
          formatRound(columns = c("mean", "stdev", "RSD"), digits = 2)
      }
    })

    output$my_plot <- renderPlot({
      if (is.null(df())) {
        return(NULL)
      } else {
        myparams <- params()
        df <- switch(myparams$type,
                     "class" = df(),
                     "species" = {
                       if (length(input$my_table_rows_selected) == 0) {
                         df <- df() %>% filter(lipid_class == input$select_species) 
                       } else {
                         df <- df() %>% filter(lipid_class == input$select_species)
                         x <- levels(droplevels(df$lipid))[input$my_table_rows_selected]
                         df %>% filter(lipid %in% x)
                       }
                     })
        p <- df %>%
          ggplot() +
          geom_point(aes(x = Name,
                         y = value,
                         color = lipid,
                         shape = batch),
                     size = 3) +
          geom_path(aes(x = Name,
                        y = value,
                        color = lipid,
                        group = lipid)) +
          guides(color = "none",
                 shape = guide_legend(title = "Batch")) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
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
    #      tmp <- df() %>% filter(lipid_class == input$select_species)
    #      x <-levels(droplevels(tmp$lipid))[input$my_table_rows_selected]
    #      paste(input$select_species, x, sep = "\n")
    #    }
    #  })
    
  })