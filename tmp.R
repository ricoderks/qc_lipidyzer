library(plotly)

myfiles <- data.frame(name = c("Results_BATCH1.xlsx", "Results_BATCH2.xlsx", "Results_BATCH3.xlsx"), 
                      datapath = c("./data/Results_BATCH1.xlsx", "./data/Results_BATCH2.xlsx", "./data/Results_BATCH3.xlsx"))
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
                        mutate(batch = .y))) %>%
  mutate(data = map(.x = data,
                    .f = ~ data.frame(.x)))     # join_all can not handle tibbles!!))

# how to join multiple rows (tables) into one dataframe (tibble)

all <- plyr::join_all(aap$data, type = "full")

p <- all %>%
  gather(lipid_class, concentration, -Name, -batch)  %>%
  mutate(Name = factor(Name, levels = unique(Name)),
         lipid_class = as.factor(lipid_class),
         batch = as.factor(batch)) %>%
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
  ylab("Concentratoion")

p
#ggplotly(p)
