library(shiny)
library(readxl)
library(dplyr)

shinyServer(function(input, output) {
    output$my_table <- renderTable({
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
        df <- df %>% filter(grepl(Name, pattern = "QC-*"))
        head(df)
      }
      })
})
