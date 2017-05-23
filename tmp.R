library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(DT)
library(purrr)
library(tibble)
library(profvis)

###################
# a new labeler function, to show the RSD value in the facet strip
label_rsd <- function(value) {
  value <- paste0("RSD: ", value, "%")
  #value <- sprintf("RSD: %.1f%%", value)
}

###################


myfiles <- data.frame(name = c("Results_BATCH1.xlsx", "Results_BATCH2.xlsx"), 
                      datapath = c("./data/Results_BATCH1.xlsx", "./data/Results_BATCH2.xlsx"))
myfiles$datapath <- as.character(myfiles$datapath)


sheet_names <- c("Lipid Species Concentrations",
                "Lipid Species Composition",
                "Lipid Class Concentration",
                "Lipid Class Composition",
                "Fatty Acid Concentration",
                "Fatty Acid Composition")

aapje <- data_frame(sheetnames = sheet_names)

myfiles$datapath %>%
  list() %>%
  rep(nrow(aapje)) %>%
  data_frame(datapath = .) %>%
  bind_cols(aapje, .) -> aapje

aapje %>% 
  mutate(data = map2(.x = datapath,
                     .y = sheetnames,
                     .f = ~ map2(.x = .x,
                                 .y = .y,
                                .f = ~read_excel(path = .x,
                                                 sheet = .y,
                                                 col_names = TRUE,
                                                 na = ".") %>%
                                  mutate(meas_order = 1:nrow(.)) %>%
                                  mutate(batch = factor(.x))) %>%
                       reduce(function(...) merge(..., all = TRUE)))) %>%
  mutate(data = map(.x = data,
                    .f = ~ arrange(.x, batch, meas_order))) %>%
  mutate(data = map(.x = data,
                    .f = ~ mutate(.x, batch = factor(batch, labels = 1:length(levels(batch)))))) %>%
  mutate(data = map(.x = data,
                    .f = ~ mutate(.x, batch_bar = as.factor((as.numeric(batch) %% 2)))))-> aapje





df_meta <- read_excel(path = "./data/MetaData.xlsx",
                      sheet = 1,
                      col_names = TRUE)

mycol <- "sampleID"

# df_meta %>% 
#   mutate_(mycol = as.character(mycol)) -> df_meta

dots <- list(paste0("as.character(", mycol, ")"))

df_meta %>%
  mutate_(.dots = setNames(dots, mycol)) %>%
  list() %>%
  rep(nrow(aapje)) %>%
  data_frame(sample_info = .) %>%
  bind_cols(aapje) %>%
  mutate(data = map2(.x = data,
                     .y = sample_info,
                     .f = ~ left_join(x = .x,
                                      y = .y,
                                      by = c("Name" = "sampleID")))) -> aapje


my_invert <- FALSE

all <- aapje %>%
  filter(sheet_names == sheet_names[3]) %>%
  select(data) %>%
  unnest %>%
  select(-meas_order) %>%
  filter((my_invert == TRUE & !grepl(Name, pattern = "QC*")) | (my_invert == FALSE & grepl(Name, pattern = "QC-[0-9]*"))) %>%
  gather(lipid, value, -Name, -batch, -batch_bar)  %>%
  mutate(Name = factor(Name, levels = unique(Name)),
         lipid = factor(lipid, levels = unique(lipid)),
         batch = as.factor(batch))

  
## line grah
p <- all  %>%
  filter(lipid_class == "CE") %>%
  droplevels() %>%
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
  gather(lipid_class, concentration, -Name, -batch, -batch_bar)  %>%
  mutate(Name = factor(Name, levels = unique(Name)),
         lipid_class = as.factor(lipid_class),
         batch = as.factor(batch),
         batch_bar = as.factor(batch_bar)) %>%
  group_by(lipid_class) %>%
  mutate(mean = mean(concentration, na.rm = TRUE),
         stdev = sd(concentration, na.rm = TRUE),
         zscore = abs((concentration - mean) / stdev),
         RSD = round((stdev / mean * 100), digits = 1 )) %>%
  ggplot() +
  geom_bar(aes(x = Name, 
               y = concentration,
               fill = zscore,
               linetype = batch_bar),
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


subset(colnames(aap$data[[3]]), !(colnames(aap$data[[3]]) %in% c("Name", "batch", "batch_bar")))
