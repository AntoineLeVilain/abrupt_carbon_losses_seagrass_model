# working directory

setwd('/supplementary/figureS9') # set your own working directory

# requirements

package_list <- c("ggplot2", "readxl", "dplyr", "scales", "gridExtra")

for (package in package_list){
  
  library(package, character.only = TRUE)
  
}

color_start = "#FF0000"

data <- read.csv("ma_da.csv")

da_values <- unique(data$da)

# no da

data_1 <- data %>%
  filter(da == da_values[1])

## seagrass

plot_no_da_s <- ggplot() + geom_line(data = data_1,
                                     aes(x = ma, y = S_seagrass),
                                     color = "#FF0000", size = 1, linetype = "solid") + xlab("ma") + ylab("S") + theme_classic() + theme(legend.position = "none")

plot_no_da_s <- plot_no_da_s + geom_line(data = data_1,
                                         aes(x = ma, y = S_bistable),
                                         color = "#FF0000", size = 1, linetype = "dashed")

plot_no_da_s <- plot_no_da_s + geom_line(data = data_1,
                                         aes(x = ma, y = S_bare),
                                         color = "#FF0000", size = 1, linetype = "solid")

ggsave("plot_no_da_s.png", 
       plot_no_da_s, 
       width = 5, height = 3)

## carbon

plot_no_da_cb <- ggplot() + geom_line(data = data_1,
                                     aes(x = ma, y = CB_seagrass),
                                     color = "#FF0000", size = 1, linetype = "solid") + xlab("ma") + ylab("CB") + theme_classic() + theme(legend.position = "none")

plot_no_da_cb <- plot_no_da_cb + geom_line(data = data_1,
                                         aes(x = ma, y = CB_bistable),
                                         color = "#FF0000", size = 1, linetype = "dashed")

plot_no_da_cb <- plot_no_da_cb + geom_line(data = data_1,
                                         aes(x = ma, y = CB_bare),
                                         color = "#FF0000", size = 1, linetype = "solid")

ggsave("plot_no_da_cb.png", 
       plot_no_da_cb, 
       width = 5, height = 3)

# normal da

data_2 <- data %>%
  filter(da == da_values[2])

## seagrass

plot_normal_da_s <- ggplot() + geom_line(data = data_2,
                                     aes(x = ma, y = S_seagrass),
                                     color = "#FF0000", size = 1, linetype = "solid") + xlab("ma") + ylab("S") + theme_classic() + theme(legend.position = "none")

plot_normal_da_s <- plot_normal_da_s + geom_line(data = data_2,
                                         aes(x = ma, y = S_bistable),
                                         color = "#FF0000", size = 1, linetype = "dashed")

plot_normal_da_s <- plot_normal_da_s + geom_line(data = data_2,
                                         aes(x = ma, y = S_bare),
                                         color = "#FF0000", size = 1, linetype = "solid")

ggsave("plot_normal_da_s.png", 
       plot_normal_da_s, 
       width = 5, height = 3)

## carbon

plot_normal_da_cb <- ggplot() + geom_line(data = data_2,
                                      aes(x = ma, y = CB_seagrass),
                                      color = "#FF0000", size = 1, linetype = "solid") + xlab("ma") + ylab("CB") + theme_classic() + theme(legend.position = "none")

plot_normal_da_cb <- plot_normal_da_cb + geom_line(data = data_2,
                                           aes(x = ma, y = CB_bistable),
                                           color = "#FF0000", size = 1, linetype = "dashed")

plot_normal_da_cb <- plot_normal_da_cb + geom_line(data = data_2,
                                           aes(x = ma, y = CB_bare),
                                           color = "#FF0000", size = 1, linetype = "solid")

ggsave("plot_normal_da_cb.png", 
       plot_normal_da_cb, 
       width = 5, height = 3)

# high da

data_3 <- data %>%
  filter(da == da_values[3])

## seagrass

plot_high_da_s <- ggplot() + geom_line(data = data_3,
                                         aes(x = ma, y = S_seagrass),
                                         color = "#FF0000", size = 1, linetype = "solid") + xlab("ma") + ylab("S") + theme_classic() + theme(legend.position = "none")

plot_high_da_s <- plot_high_da_s + geom_line(data = data_3,
                                                 aes(x = ma, y = S_bistable),
                                                 color = "#FF0000", size = 1, linetype = "dashed")

plot_high_da_s <- plot_high_da_s + geom_line(data = data_3,
                                                 aes(x = ma, y = S_bare),
                                                 color = "#FF0000", size = 1, linetype = "solid")

ggsave("plot_high_da_s.png", 
       plot_high_da_s, 
       width = 5, height = 3)

## carbon

plot_high_da_cb <- ggplot() + geom_line(data = data_3,
                                          aes(x = ma, y = CB_seagrass),
                                          color = "#FF0000", size = 1, linetype = "solid") + xlab("ma") + ylab("CB") + theme_classic() + theme(legend.position = "none")

plot_high_da_cb <- plot_high_da_cb + geom_line(data = data_3,
                                                   aes(x = ma, y = CB_bistable),
                                                   color = "#FF0000", size = 1, linetype = "dashed")

plot_high_da_cb <- plot_high_da_cb + geom_line(data = data_3,
                                                   aes(x = ma, y = CB_bare),
                                                   color = "#FF0000", size = 1, linetype = "solid")

ggsave("plot_high_da_cb.png", 
       plot_high_da_cb, 
       width = 5, height = 3)
