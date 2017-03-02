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

# this is not really great!!
aap <- myfiles %>%
  as.tbl %>%
  mutate(batch = 1:n()) %>%
  mutate(LS_conc = map(.x = datapath,
                       .f = ~ read_excel(path = .x,
                                         sheet = sheet_names[1],
                                         col_names = TRUE,
                                         na = "."))) %>%
  mutate(LS_comp = map(.x = datapath,
                       .f = ~ read_excel(path = .x,
                                         sheet = sheet_names[2],
                                         col_names = TRUE,
                                         na = "."))) %>%
  mutate(LC_conc = map(.x = datapath,
                      .f = ~ read_excel(path = .x,
                                        sheet = sheet_names[3],
                                        col_names = TRUE,
                                        na = "."))) %>%
  mutate(LC_comp = map(.x = datapath,
                       .f = ~ read_excel(path = .x,
                                         sheet = sheet_names[4],
                                         col_names = TRUE,
                                         na = "."))) %>%
  mutate(FA_conc = map(.x = datapath,
                        .f = ~ read_excel(path = .x,
                                          sheet = sheet_names[5],
                                          col_names = TRUE,
                                          na = "."))) %>%
  mutate(FA_comp = map(.x = datapath,
                        .f = ~ read_excel(path = .x,
                                          sheet = sheet_names[6],
                                          col_names = TRUE,
                                          na = "."))) %>%
  # only the QC's
  mutate_at(vars(matches("^[LF][SCA]")),
            funs(map(., ~ filter(.x, ! (grepl(Name, pattern = "QC_SPIKE*")) & grepl(Name, pattern = "QC-*"))))) 

aap <- aap %>% select(-name, -datapath) %>% mutate(batch = as.factor(batch))

all <- tibble(LS_conc = list(unnest(aap, LS_conc)),
              LS_comp = list(unnest(aap, LS_comp)),
              LC_conc = list(unnest(aap, LC_conc)),
              LC_comp = list(unnest(aap, LC_comp)),
              FA_conc = list(unnest(aap, FA_conc)),
              FA_comp = list(unnest(aap, FA_comp)))

p <- all %>% 
  select_("LC_comp") %>%
  unnest %>%
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
