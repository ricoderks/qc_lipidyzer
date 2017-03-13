library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(DT)
library(purrr)
library(tibble)

myfiles <- data.frame(name = c("Results_BATCH1.xlsx", "Results_BATCH2.xlsx", "Results_BATCH3.xlsx"), 
                      datapath = c("./data/Results_BATCH1.xlsx", "./data/Results_BATCH2.xlsx", "./data/Results_BATCH3.xlsx"))
myfiles$datapath <- as.character(myfiles$datapath)


sheet_names <- c("Lipid Species Concentrations",
                "Lipid Species Composition",
                "Lipid Class Concentration",
                "Lipid Class Composition",
                "Fatty Acid Concentration",
                "Fatty Acid Composition")

aap <- data_frame(datapath = rep(myfiles$datapath, each = length(sheet_names)), sheet_names = rep(sheet_names, nrow(myfiles)))

aap %>% slice(1:12) -> aap

aap <-aap %>%
  # this is for the line graph
  #mutate(batch = rep(seq(1, length(unique(datapath))), each = length(unique(sheet_names)))) %>% 
  # this is for the bar graph
  mutate(batch = rep(c(1, 2), each = length(unique(sheet_names)), length.out = length(unique(datapath)) * length(unique(sheet_names)))) %>%
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

all <- aap %>%
  filter(sheet_names == sheet_names[3]) %>%
  select(data) %>%
  unnest
  
## line grah
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
  ylab("Concentratsion")

p
#ggplotly(p)


## bar graph, color by zscore

all %>% 
  gather(lipid_class, concentration, -Name, -batch)  %>%
  mutate(Name = factor(Name, levels = unique(Name)),
         lipid_class = as.factor(lipid_class),
         batch = as.factor(batch)) %>%
  group_by(lipid_class) %>%
  mutate(mean = mean(concentration, na.rm = TRUE),
         stdev = sd(concentration, na.rm = TRUE),
         zscore = abs((concentration - mean) / stdev),
         RSD = stdev / mean * 100) %>%
  ggplot() +
  geom_bar(aes(x = Name, 
               y = concentration,
               fill = zscore,
               linetype = batch),
           stat = "identity",
           color = "black") +
  geom_line(aes(x = Name,
                y = mean,
                group = 1),
            color = "black") +
  facet_wrap(~ lipid_class, ncol = 3, scales = "free_y") +
  scale_fill_gradientn(colors = c("green", "yellow", "red"),
                       values = scales::rescale(x = c(0, 2, 4))) +     # needs to be scaled between 0 and 1!!!
  guides(linetype = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("QC sample ID") +
  ylab("Concentration")


