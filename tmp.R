myfiles <- data.frame(name = c("Results_BATCH1.xlsx", "Results_BATCH2.xlsx", "Results_BATCH3.xlsx"), 
                      datapath = c("./data/batch1.xlsx", "./data/batch2.xlsx", "./data/batch3.xlsx"))
myfiles$datapath <- as.character(myfiles$datapath)


aap <- myfiles$name %>% 
  data_frame(filenames = .) %>% 
  as.tbl %>% 
  mutate(batch = 1:n()) %>%
  mutate(data = map(.x = myfiles$datapath, 
                     .f = ~ read_excel(path = .x, 
                                       sheet = "Lipid Class Composition", 
                                       col_names = TRUE, na = ".")))
aap <- aap %>% 
  mutate(data = map2(.x = data,
                     .y = batch,
                     .f = ~ filter(.x,
                                   ! grepl(Name, pattern = "QC_SPIKE*") & grepl(Name, pattern = "QC-*")) %>%
                        mutate(batch = .y)))
                     # %>%
                     #    gather(lipid_class, concentration, -Name, -batch) %>%
                     #    mutate(lipid_class = as.factor(lipid_class) )))

# how to join multiple rows (tables) into one dataframe (tibble)

all <- aap$data %>% Reduce(function(df1, df2) { full_join(df1, df2) }, .)

all