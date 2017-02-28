library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)

shinyServer(function(input, output) {
  df <- reactive({
    my_file <- input$result_files
    
    if (is.null(my_file)) {
      return(NULL)
    } else {
      # need to rename the file because of problem with readxl
      # it needs the xlsx extension
      file.rename(my_file$datapath,
                  paste0(my_file$datapath, ".xlsx"))
      df <- read_excel(path = paste0(my_file$datapath, ".xlsx"),
                 sheet = "Lipid Class Composition",
                 col_names = TRUE)
      df %>% filter(! grepl(Name, pattern = "QC_SPIKE*"))      # remove QC_SPIKE samples
    }
  })
  
  output$my_table <- renderTable({
    if (is.null(df())) {
      return(NULL)
    } else {
      df() %>%
        filter(grepl(Name, pattern = "QC-*")) %>%
        gather(lipid_class, concentration, -Name) %>%
        mutate(lipid_class = as.factor(lipid_class),
               concentration = as.numeric(concentration)) %>%
        group_by(lipid_class) %>%
        summarise(mean = mean(concentration, na.rm = TRUE),
                stdev = sd(concentration, na.rm = TRUE)) %>%
        mutate(RSD = stdev / mean * 100) %>%
        as.data.frame
    }
  })
  
  output$my_plot <- renderPlot({
    if (is.null(df())) {
      return(NULL)
    } else {
      df() %>% 
        filter(grepl(Name, pattern = "QC-*")) %>%
        gather(lipid_class, concentration, -Name) %>%
        mutate(lipid_class = as.factor(lipid_class),
               concentration = as.numeric(concentration)) %>%                      # batch 1 give characters instead of numbers
        ggplot() +
          geom_point(aes(x = factor(Name, levels = unique(Name)),
                     y = concentration,
                     color = lipid_class)) +
          facet_wrap(~ lipid_class, ncol = 3, scales = "free_y")
    }
  })
})
