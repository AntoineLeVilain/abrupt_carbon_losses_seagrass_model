# Working directory

setwd('figure2/figure2abc') # set your own working directory

# Requirements

package_list <- c("ggplot2", "readxl", "gridExtra", "dplyr", "scales", "wesanderson", "ggalt")

for (package in package_list){
  
  library(package, character.only = TRUE)
  
}

x1 <- (96 + 997)*0.335 #low realistic biomass
x2 <- (1294 + 2003)*0.335 #high realistic biomass

# data prep

data <- read.csv("sh_temp.csv")

seagrass <- data %>%
  filter(stability == 'seagrass' | stability == 'bistable')

realistic1 <- seagrass %>%
  filter(S >= x1) %>%
  filter(S <= x2)


data2 <- read.csv("sh_ma.csv")

seagrass2 <- data2 %>%
  filter(stability == 'seagrass' | stability == 'bistable')

realistic2 <- seagrass2 %>%
  filter(S >= x1) %>%
  filter(S <= x2)

data3 <- read.csv("sh_pmax.csv")

seagrass3 <- data3 %>%
  filter(stability == 'seagrass' | stability == 'bistable')

realistic3 <- seagrass3 %>%
  filter(S >= x1) %>%
  filter(S <= x2)

# color palette

pal <- wes_palette("Zissou1", 100, type = "continuous")

data_pal <- rbind(seagrass[, 5:9], seagrass2[, 5:9], seagrass3[, 5:9])

common_color_scale <- scale_color_gradientn(colors = pal, limits = c(min(log(data_pal$S)), max(log(data_pal$S))))

# plot1

plot_1 <- ggplot() +
  geom_point(data = seagrass, aes(x = temp, y = sh, color = log(S)), size = 1, shape = 15) +
  xlab("T") + ylab("Sh") + theme_classic() + theme(legend.position = "none") +
  common_color_scale +
  geom_encircle(data = realistic1, aes(x = temp, y = sh), s_shape = 1, expand = 0.01, position = "identity", linetype = "dashed", color = "black", size = 3) + # Circle them
  xlim(298.85, 308.15)

# plot2

plot_2 <- ggplot() + geom_point(data = seagrass2, aes(x = ma, y = sh, color = log(S)), size = 1, shape = 15) + 
  xlab("ma") + ylab("Sh") + theme_classic() + theme(legend.position = "none") + xlim(0, 0.006) +
  common_color_scale +
  geom_encircle(data = realistic2, aes(x = ma, y = sh), s_shape = 1, expand = 0.01, position = "identity", linetype = "dashed", color = "black", size = 3)

# plot3

plot_3 <- ggplot() + geom_point(data = seagrass3, aes(x = pmax, y = sh, color = log(S)), size = 1, shape = 15) + 
  xlab("Pmax") + ylab("Sh") + theme_classic() + theme(legend.position = "none") + xlim(0.5, 2.5) +
  common_color_scale +
  geom_encircle(data = realistic3, aes(x = pmax, y = sh), s_shape = 1, expand = 0.01, position = "identity", linetype = "dashed", color = "black", size = 3)

# Save plots 
plot <- grid.arrange(plot_2, plot_3, plot_1, ncol = 3)

ggsave("figure2a.png", 
       plot, 
       width = 15, height = 3)
