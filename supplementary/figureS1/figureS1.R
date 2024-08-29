# Library
library(minpack.lm)
library(readxl)
library(tidyverse)

# Working directory
setwd('/supplementary/figureS1') # set your working directory

data <- read_excel("growth_rate_temperature.xlsx")

data <- data %>% 
  filter(Species == 'P. oceanica') %>%
  select(`RGR (d-1)`, `Temperature treatment (°C)`)

colnames(data) <- c('growth_rate', 't')

light <- 219/(219 + 225)

data <- data%>%
  mutate(growth_rate = growth_rate/light)

# Define the function
my_fun <- function(rmax, t, topt, tmax) rmax*((tmax - t)/(tmax - topt))*(t/topt)^((topt)/(tmax - topt))

# Initial estimates for the parameters
start_params <- c(rmax = 0, topt = 20, tmax = 30)

# Lower bounds for the parameters
lower_bounds <- c(rmax = 0, topt = 0, tmax = 0)

# Fit the model with constraints
fit <- nlsLM(growth_rate ~ my_fun(rmax, t, topt, tmax),
             data = data,
             start = start_params,
             lower = lower_bounds,
             control = nls.lm.control(maxiter = 500))

print(summary(fit))

# Get the estimated parameters
estimated_params <- coef(fit)

# Print the estimated parameters
print(estimated_params)

# Calculate 5% and 95% confidence intervals
conf_intervals <- confint(fit, level = 0.90)  # Adjust the level as needed
print(conf_intervals)

x <- seq(0, 40, 0.1)

fitted_lower <- conf_intervals[1, 1]*((conf_intervals[3, 1] - x)/(conf_intervals[3, 1] - conf_intervals[2, 1]))*(x/conf_intervals[2, 1])^((conf_intervals[2, 1])/(conf_intervals[3, 1] - conf_intervals[2, 1]))
fitted_upper <- conf_intervals[1, 2]*((conf_intervals[3, 2] - x)/(conf_intervals[3, 2] - conf_intervals[2, 2]))*(x/conf_intervals[2, 2])^((conf_intervals[2, 2])/(conf_intervals[3, 2] - conf_intervals[2, 2]))


tiff("figureS1.tiff", units = 'in', width = 7, height = 5, res = 700)

plot(data$t + 273.15, data$growth_rate, 
     xlab="Temperature T (K)", ylab="r/(219/(219+225)) (day-1)", pch=19, col="blue", xlim=c(283.15, 36+273.15), ylim=c(-0.01, 0.03), xaxs="i", yaxs="i")
lines(x + 273.15, estimated_params[1]*((estimated_params[3] - x)/(estimated_params[3] - estimated_params[2]))*(x/estimated_params[2])^((estimated_params[2])/(estimated_params[3] - estimated_params[2])), col="red", type="l")
legend("topright", legend=c("Data points", "Fitted function"), pch=19, col=c("blue", "red"), bty="n")

plot(data$t + 273.15, data$growth_rate, 
     xlab="Temperature T (K)", ylab="r/(219/(219+225)) (day-1)", pch=19, col="blue", xlim=c(283.15, 36+273.15), ylim=c(-0.01, 0.03), xaxs="i", yaxs="i")
lines(x + 273.15, estimated_params[1]*((estimated_params[3] - x)/(estimated_params[3] - estimated_params[2]))*(x/estimated_params[2])^((estimated_params[2])/(estimated_params[3] - estimated_params[2])), col="red", type="l")
legend("topright", legend=c("Data points", "Fitted function", "95% CI"), pch=19, col=c("blue", "red"), bty="n")

