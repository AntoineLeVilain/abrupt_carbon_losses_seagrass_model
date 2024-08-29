rm(list=ls())

# Requirements
package_list <- c("sensobol", "data.table", "ggplot2", "deSolve", "foreach", "parallel", "readxl", "doParallel", "rootSolve", "dplyr", "rlang", "pracma", "matrixcalc", "data.table", "nleqslv")

for (package in package_list){
  
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
  }
  
  library(package, character.only = TRUE)
  
}

setwd('/sensitivity/1_f') # set the working directory of the sensitivity analysis results of the feedback case

ind.dummy <- readRDS("ld_deter_ma_S.dummy.RData")
ind <- readRDS("ld_deter_ma_S.RData")

feedback_dummy <- ind.dummy %>%
  filter(sensitivity == "Si") %>%
  select(original)

feedback_dummy <- feedback_dummy[1, 1]

feedback_sobol <- ind$results %>%
  filter(sensitivity == "Si") %>%
  mutate(category = "feedback") %>%
  select(original, low.ci, parameters)

setwd('/sensitivity/1_no_f') # set the working directory of the sensitivity analysis results of the no feedback case

ind.dummy <- readRDS("ld_deter_ma_S.dummy.RData")
ind <- readRDS("ld_deter_ma_S.RData")

no_feedback_dummy <- ind.dummy %>%
  filter(sensitivity == "Si") %>%
  select(original)

no_feedback_dummy <- no_feedback_dummy[1, 1]

no_feedback_sobol <- ind$results %>%
  filter(sensitivity == "Si") %>%
  mutate(category = "no_feedback") %>%
  select(original, low.ci, parameters)

rm(ind)
rm(ind.dummy)

feedback_sobol <- feedback_sobol %>%
  filter(low.ci > feedback_dummy) %>%
  mutate(original = original / sum(original)) %>%
  select(original, parameters)
  
no_feedback_sobol <- no_feedback_sobol %>%
  filter(low.ci > no_feedback_dummy) %>%
  mutate(original = original / sum(original)) %>%
  select(original, parameters)

# Define a color palette
color_palette <- c(
  "rmax" = "#377eb8", "topt" = "#ff7f00", "tmax" = "#4daf4a", 
  "l" = "#f781bf", "nr" = "#a65628", "i0" = "#984ea3", 
  "ir" = "#999999", "hp" = "#e41a1c", "a" = "#dede00",
  "mh" = "#f0027f", "alpha" = "#b3de69", "h" = "#fccde5",
  "phida" = "#80b1d3", "phids" = "#fdb462", "phidb" = "#b3cde3", 
  "ea" = "#ccebc5", "hea" = "#ffed6f", "hes" = "#8dd3c7", 
  "fb1" = "#bebada", "fb2" = "#fb8072", "beta" = "#80b1d3",
  "gamma" = "#fdb462", "delta" = "#b3cde3", "ms" = "#fccde5", 
  "td" = "#d9d9d9"
)

feedback_plot <- ggplot(feedback_sobol, aes(x = "", y = original, fill = parameters)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = color_palette) +
  theme_void()

no_feedback_plot <- ggplot(no_feedback_sobol, aes(x = "", y = original, fill = parameters)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = color_palette) +
  theme_void()

setwd('/sensitivity/1_f') # set your saving working directory

ggsave("ld_deter_ma_f_S.png", 
       feedback_plot, 
       width = 10, height = 6)

setwd('/sensitivity/1_no_f') # set your saving working directory

ggsave("ld_deter_ma_nf_S.png", 
       no_feedback_plot, 
       width = 10, height = 6)
