# Library
library(minpack.lm)
library(readxl)
library(tidyverse)

# Working directory
setwd('/supplementary/figureS8') # set your working directory

data <- read_excel("co2_temperature.xlsx")

data <- data %>% 
  select(T, CO2, Treatment, Replica)

data <- data %>%
  mutate(CO2 = CO2*0.000001*12.01*86400) %>%
  mutate(T = T + 273.15) %>%
  mutate(CO2 = log(CO2)) %>%
  mutate(T = -1/(8.314*T))

pre_exponential <- c()
energy_activation <- c()

for (i in unique(data$Treatment)){
  
  subset <- data %>%
    filter(Treatment == i)
  
  for (j in unique(subset$Replica)){
    
    subset_2 <- subset %>%
      filter(Replica == j)
    
    model <- lm(I(CO2) ~ T, data=subset_2)
    
    pre_exponential <- c(pre_exponential, model$coefficients[1])
    energy_activation <- c(energy_activation, model$coefficients[2])
    
  }
  
}

ea <- mean(energy_activation)

data <- read_excel("co2_temperature.xlsx")

ea <- 58000

ea1 <- 48000
ea2 <- 68000

x <- seq(288.15, 305.15, 0.1)

tiff("figureS8.tiff", units = 'in', width = 7, height = 5, res = 700)

plot(x, exp(-ea/(8.314*x))*0.99/(exp(-ea/(8.314*303.65))), xlab = "Temperature (K)", ylab = "Relative decomposition rate", type = 'l', col = "red", ylim = c(0, 1.1), xaxs="i", yaxs="i")
lines(x, exp(-ea1/(8.314*x))*0.99/(exp(-ea1/(8.314*302.15))), col="black", type="l")
lines(x, exp(-ea2/(8.314*x))*0.99/(exp(-ea2/(8.314*305.15))), col="black", type="l")

x_combined <- c(x, rev(x))
y_combined <- c(exp(-ea2/(8.314*x))*0.99/(exp(-ea2/(8.314*305.15))), rev(exp(-ea1/(8.314*x))*0.99/(exp(-ea1/(8.314*302.15)))))
polygon(x_combined, y_combined, col="grey", border=NA)

lines(x, exp(-ea/(8.314*x))*0.99/(exp(-ea/(8.314*303.65))), col="red", type="l")

legend("topright", legend=c("Fitted function", "Uncertainty"), pch=19, col=c("red", "grey"), bty="n")

plot(x, exp(-ea/(8.314*x))*0.99/(exp(-ea/(8.314*303.65))), xlab = "Temperature (K)", ylab = "Relative decomposition rate", type = 'l', col = "red", ylim = c(0, 1.1), xaxs="i", yaxs="i")
lines(x, exp(-ea1/(8.314*x))*0.99/(exp(-ea1/(8.314*302.15))), col="black", type="l")
lines(x, exp(-ea2/(8.314*x))*0.99/(exp(-ea2/(8.314*305.15))), col="black", type="l")

x_combined <- c(x, rev(x))
y_combined <- c(exp(-ea2/(8.314*x))*0.99/(exp(-ea2/(8.314*305.15))), rev(exp(-ea1/(8.314*x))*0.99/(exp(-ea1/(8.314*302.15)))))
polygon(x_combined, y_combined, col="grey", border=NA)

lines(x, exp(-ea/(8.314*x))*0.99/(exp(-ea/(8.314*303.65))), col="red", type="l")

legend("topright", legend=c("Fitted function", "Uncertainty"), pch=19, col=c("red", "grey"), bty="n")
