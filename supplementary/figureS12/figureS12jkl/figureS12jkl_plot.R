# working directory

setwd('/supplementary/figureS12/figureS12jkl') # set your own working directory

# requirements

package_list <- c("ggplot2", "readxl", "gridExtra", "dplyr", "scales", "wesanderson", "ggalt")

for (package in package_list){
  
  library(package, character.only = TRUE)
  
}

# parameter values

rmax <- 0.011
ms <- 0.0014
topt <- 298.85
tmax <- 307.05
nr <- 0.015
i0 <- 709
ir <- 296
sh <- 22.7
hp <- 0.023
a <- 0.07
mh <- 0.028
alpha <- 0.194
h <- 0.68
phida <- 0.024
phids <- 0.00024
phidb <- 0.00005
ea <- 58000
r <- 8.314
td <- 303.65
hea <- 12.53
hes <- 12.53
fb1 <- 0.0022
fb2 <- 1335
beta <- 0.292
gamma <- 0.137
delta <- 0.052
l <- 0.01

z <- 20
temp <- 298.85
hmax <- 0.05
pmax <- 0.5
ma <- 0
i <- 0.02

# model

model <- function(x){
  
  S <- rmax*((tmax - temp)/(tmax - topt))*((temp/topt)^(topt/(tmax - topt)))*x[5]/(x[5] + nr)*(i0*exp(-a*pmax*z)/(i0*exp(-a*pmax*z) + ir))*x[1] - x[1]*(ms + ma + mh*hmax*sh/(sh + x[1]))
  
  CA <- alpha*h*pmax*(1 - (hmax*sh/(sh + x[1]))/(hmax*sh/(sh + x[1]) + hp)) - x[2]*(phida*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp)) + (hmax*sh/(sh + x[1]))/(hmax*sh/(sh + x[1]) + hea) + 1/(1+exp(-fb1*(x[2] + x[3] - fb2))))
  
  CS <- beta*x[1]*(ms + ma + mh*hmax*sh/(sh + x[1])) - x[3]*(phids*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp)) + (hmax*sh/(sh + x[1]))/(hmax*sh/(sh + x[1]) + hes) + 1/(1+exp(-fb1*(x[2] + x[3] - fb2))))
  
  CB <- x[2]*1/(1+exp(-fb1*(x[2] + x[3] - fb2))) + x[3]*1/(1+exp(-fb1*(x[2] + x[3] - fb2))) - x[4]*phidb*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp))
  
  N <- i + gamma*(x[2]*phida*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp)) + x[3]*phids*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp)) + x[4]*phidb*1/(exp(-ea/(8.314*td)))*exp(-ea/(8.314*temp))) - delta*(rmax*((tmax - temp)/(tmax - topt))*((temp/topt)^(topt/(tmax - topt)))*x[5]/(x[5] + nr)*(i0*exp(-a*pmax*z))/(i0*exp(-a*pmax*z) + ir))*x[1]-l*x[5]
  
  c(S = S, CA = CA, CS = CS, CB = CB, N = N)
  
}

# data prep

data <- read.csv("nr_temp.csv")

data2 <- read.csv("nr_ma.csv")

data3 <- read.csv("nr_pmax.csv")

# plot1

plot_1 <- ggplot() +
  geom_point(data = data, aes(x = temp, y = nr, color = stability), size = 1, shape = 15) +
  scale_color_manual(breaks = c("seagrass", "bistable", "bare"),
                     values=c("#52854C", "black","#C4961A")) +
  xlab("T") + ylab("Sh") + theme_classic() + theme(legend.position = "none") + xlim(298.85, 308.15)

# plot2

plot_2 <- ggplot() + geom_point(data = data2, aes(x = ma, y = nr, color = stability), size = 1, shape = 15) + 
  scale_color_manual(breaks = c("seagrass", "bistable", "bare"),
                     values=c("#52854C", "black","#C4961A")) +
  xlab("ma") + ylab("Sh") + theme_classic() + theme(legend.position = "none") + xlim(0, 0.006)

# plot3

plot_3 <- ggplot() + geom_point(data = data3, aes(x = pmax, y = nr, color = stability), size = 1, shape = 15) + 
  scale_color_manual(breaks = c("seagrass", "bistable", "bare"),
                     values=c("#52854C", "black","#C4961A")) +
  xlab("Pmax") + ylab("Sh") + theme_classic() + theme(legend.position = "none") + xlim(0.5, 2.5)

# Save plots as a vectorized PDF
tiff("nr.tiff", units = 'in', width = 15, height = 3, res = 700)
grid.arrange(plot_2, plot_3, plot_1, ncol = 3)
grid.arrange(plot_2, plot_3, plot_1, ncol = 3)
