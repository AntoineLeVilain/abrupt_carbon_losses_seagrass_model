# working directory

setwd('/supplementary/figureS14/figureS14abc') # set your own working directory

# requirements

package_list <- c("ggplot2", "readxl", "gridExtra", "dplyr", "scales", "wesanderson", "ggalt")

for (package in package_list){
  
  library(package, character.only = TRUE)
  
}

# data prep

data <- read.csv("sh_temp.csv")

data2 <- read.csv("sh_ma.csv")

data3 <- read.csv("sh_pmax.csv")

# plot ma

plot_1 <- ggplot() +
  geom_point(data = data, aes(x = temp, y = sh, color = stability), size = 1, shape = 15) +
  scale_color_manual(breaks = c("seagrass", "bistable", "bare"),
                     values=c("#52854C", "black","#C4961A")) +
  xlab("T") + ylab("Sh") + theme_classic() + theme(legend.position = "none") + xlim(298.85, 308.15)

# plot pmax

plot_2 <- ggplot() + geom_point(data = data2, aes(x = ma, y = sh, color = stability), size = 1, shape = 15) + 
  scale_color_manual(breaks = c("seagrass", "bistable", "bare"),
                     values=c("#52854C", "black","#C4961A")) +
  xlab("ma") + ylab("Sh") + theme_classic() + theme(legend.position = "none") + xlim(0, 0.006)

# plot t

plot_3 <- ggplot() + geom_point(data = data3, aes(x = pmax, y = sh, color = stability), size = 1, shape = 15) + 
  scale_color_manual(breaks = c("seagrass", "bistable", "bare"),
                     values=c("#52854C", "black","#C4961A")) +
  xlab("Pmax") + ylab("Sh") + theme_classic() + theme(legend.position = "none") + xlim(0.5, 2.5)

# Save plots as a vectorized PDF
tiff("mortality.tiff", units = 'in', width = 15, height = 3, res = 700)
grid.arrange(plot_2, plot_3, plot_1, ncol = 3)
grid.arrange(plot_2, plot_3, plot_1, ncol = 3)
