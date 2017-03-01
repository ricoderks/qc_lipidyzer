library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(DT)
library(purrr)

# define the sheet names found in the result files from the lipidyzer
sheet_names = c("Lipid Species Concentrations",
                "Lipid Species Composition",
                "Lipid Class Concentration",
                "Lipid Class Composition",
                "Fatty Acid Concentration",
                "Fatty Acid Composition")

shinyServer(function(input, output) {
  # output$my_text <- reactive({
  #   myfile <- input$result_files
  #   if (is.null(myfile)) {
  #     return("")
  #   } else {
  #     myfile$datapath <- paste0(myfile$datapath, ".xlsx")
  #     myfile$datapath
  #   }
  # })
      
  df <- reactive({
    my_file <- input$result_files
    
    if (is.null(my_file)) {
      return(NULL)
    } else {
      # need to rename the file because of problem with readxl
      # it needs the xlsx extension
      # rename the actual temporary file
      file.rename(my_file$datapath,
                  paste0(my_file$datapath, ".xlsx"))
      # change the filename in the dataframe
      my_file$datapath <- paste0(my_file$datapath, ".xlsx")
      
      # read the excel file
      df <- my_file$name %>%
        data_frame(filenames = .) %>%
        as.tbl %>%
        mutate(batch = 1:n()) %>%
        mutate(data = map(.x = my_file$datapath,
                          .f = ~ read_excel(path = .x,
                                            sheet = "Lipid Class Concentration",
                                            col_names = TRUE,
                                            na = ".")))
      
      # get only the QC's and prep for stats and visualisation
      df <- df %>% 
        mutate(data = map2(.x = data,
                           .y = batch,
                           .f = ~ filter(.x,
                                         ! grepl(Name, pattern = "QC_SPIKE*") & grepl(Name, pattern = "QC-*")) %>%
                             mutate(batch = .y)))
      
      # merge all batches
      all <- df$data %>%
        Reduce(function(df1, df2) {full_join(df1, df2)}, .)
      
      all %>%
        gather(lipid_class, concentration, -Name, -batch) %>%
        mutate(lipid_class = as.factor(lipid_class),
               batch = as.factor(batch))
    }
  })

  output$my_table <- DT::renderDataTable({
    if (is.null(df())) {
      return(NULL)
    } else {
      df() %>%
        group_by(lipid_class) %>%
        summarise(mean = mean(concentration),
                stdev = sd(concentration)) %>%
        mutate(RSD = stdev / mean * 100) %>%
        datatable(colnames = c("Lipid class", "Mean", "St.dev.", "RSD [%]"),
                  options = list(dom = "t")) %>%             # remove the search field
        formatRound(columns = c("mean", "stdev", "RSD"), digits = 2)
    }
  })

  output$my_plot <- renderPlot({
    if (is.null(df())) {
      return(NULL)
    } else {
      df() %>%
        mutate(Name = factor(Name, levels = unique(Name))) %>%
        ggplot() +
        geom_point(aes(x = Name,
                       y = concentration,
                       color = lipid_class,
                       shape = batch),
                   size = 3) +
        geom_path(aes(x = Name,
                      y = concentration,
                      color = lipid_class,
                      group = lipid_class)) +
        facet_wrap(~ lipid_class, ncol = 3, scales = "free_y") +
        guides(color = "none",
               shape = guide_legend(title = "Batch")) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        xlab("QC sample ID") +
        ylab("Concentration") +
        ggtitle("QC overview per lipid class")
    }
  })
})
