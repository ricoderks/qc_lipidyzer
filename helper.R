### This file contains several functions which will be called by server.R

######################################################
# plot a QC plot as a line graph
qc_line <- function(data, num_batches, params) {
p <- data %>%
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
if (num_batches == 1) {
  p <- p + guides(color = "none",
                  shape = "none")
} else {
  p <- p + guides(color = "none",
                  shape = guide_legend(title = "Batch")) 
}
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("QC sample ID") +
  ylab(params$ylab)
if (params$type == "class") {
  p <- p + facet_wrap(~ lipid, ncol = 3, scales = "free_y")
}
return(p)
}

######################################################
# plot a QC plot as a bar graph
qc_bar <- function(data, params) {
  p <- data %>%
    group_by(lipid) %>%
    mutate(mean = mean(value, na.rm = TRUE),
           stdev = sd(value, na.rm = TRUE),
           zscore = abs((value - mean) / stdev),
           RSD = round((stdev / mean * 100), digits = 1 )) %>%
    ggplot() +
    geom_bar(aes(x = Name, 
                 y = value,
                 fill = zscore,
                 linetype = batch_bar),
             stat = "identity",
             color = "black") +
    geom_line(aes(x = Name,
                  y = mean,
                  group = 1),
              color = "black",
              size = 1) +
    facet_wrap(~ lipid, ncol = 3, scales = "free_y") +
    scale_fill_gradientn(colors = c("green", "yellow", "red"),
                         values = scales::rescale(x = c(0, 2, 4))) +     # needs to be scaled between 0 and 1!!!
    guides(linetype = "none") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("QC sample ID") +
    ylab(params$ylab)
  
  return(p)
}

######################################################
# plot samples as heatmap
sample_heatmap <- function(data, params, my_facet) {
  p <- data %>%
    ggplot() +
    geom_raster(aes(x = Name,
                    y = lipid,
                    fill = value)) +
    scale_fill_gradientn(colours = rainbow(4), 
                         na.value = "white") +
    xlab("Sample ID") +
    guides(fill = guide_colourbar(title = params$ylab))
  
  p <- switch(params$type,
         "class" = { p + ylab("Lipid class") },
         "species" = { p + ylab("Lipid species") },
         "fa_species" = { p + ylab("Fatty acid species") }
  )
  
  return(p)
}