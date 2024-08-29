# Library
library(minpack.lm)
library(tidyverse)

# Working directory
setwd('/supplementary/figureS5') # set your working directory

# Data
data <- data.frame(
  r = c(192, 128, 286, 328, 332, 333, 459, 536, 617, 756, 708), 
  io = c(0.46, 0.83, 0.61, 0.47, 1.03, 1.81, 1.51, 2.57, 2.41, 3.68, 4.35)
)

data <- data %>%
  mutate(io = io/3600*1000000)

# Define the function
my_fun <- function(io, ir) 750*(io / (io + ir))

# Initial estimates for the parameters
start_params <- c(ir = median(data$io))

# Lower bounds for the parameters
lower_bounds <- c(io = 0)

# Fit the model with constraints
fit <- nlsLM(r ~ my_fun(io, ir),
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

# Extracting lower and upper bounds
lower_bound <- conf_intervals[1]
upper_bound <- conf_intervals[2]

x <- seq(0, 1000, 0.1)

# Generating fitted values for the bounds
fitted_lower <- 750*(x / (x + lower_bound))
fitted_upper <- 750*(x / (x + upper_bound))

tiff("figureS5.tiff", units = 'in', width = 7, height = 5, res = 700)

# Plot
plot(data$io, data$r, 
     xlab="Irradiance I (uE m-2 s-1)", ylab="Productivity P (ug C g DW-1 h-1)", pch=19, col="blue", xlim=c(0, 800), ylim=c(0, 800), xaxs="i", yaxs="i")
lines(x, 750*(x / (x + estimated_params)), col="red", type="l")

# Add lines for the confidence interval bounds
lines(x, fitted_lower, col="black", type="l")
lines(x, fitted_upper, col="black", type="l")

# Create a polygon to fill the area between the lower and upper bounds
x_combined <- c(x, rev(x))  # Combine and reverse x for a closed shape
y_combined <- c(fitted_lower, rev(fitted_upper))
polygon(x_combined, y_combined, col="grey", border=NA)

# Re-draw the main fitted line and data points to overlay the grey area
lines(x, 750*(x / (x + estimated_params)), col="red", type="l")
points(data$io, data$r, pch=19, col="blue")
legend("topright", legend=c("Data points", "Fitted function", "95% CI"), pch=19, col=c("blue", "red", "grey"), bty="n")

# Plot
plot(data$io, data$r, 
     xlab="Irradiance I (uE m-2 s-1)", ylab="Productivity P (ug C g DW-1 h-1)", pch=19, col="blue", xlim=c(0, 800), ylim=c(0, 800), xaxs="i", yaxs="i")
lines(x, 750*(x / (x + estimated_params)), col="red", type="l")

# Add lines for the confidence interval bounds
lines(x, fitted_lower, col="black", type="l")
lines(x, fitted_upper, col="black", type="l")

# Create a polygon to fill the area between the lower and upper bounds
x_combined <- c(x, rev(x))  # Combine and reverse x for a closed shape
y_combined <- c(fitted_lower, rev(fitted_upper))
polygon(x_combined, y_combined, col="grey", border=NA)

# Re-draw the main fitted line and data points to overlay the grey area
lines(x, 750*(x / (x + estimated_params)), col="red", type="l")
points(data$io, data$r, pch=19, col="blue")
legend("topright", legend=c("Data points", "Fitted function", "95% CI"), pch=19, col=c("blue", "red", "grey"), bty="n")
