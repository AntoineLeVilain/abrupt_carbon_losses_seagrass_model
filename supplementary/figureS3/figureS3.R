# Library
library(minpack.lm)

# Working directory
setwd('/supplementary/figureS3') # set your working directory

# Data
data <- data.frame(
  p = c(0.08, 0.15, 0.39, 0.41), 
  h = c(0.05*22.7/(22.7+170), 0.05*22.7/(22.7+170), 0.1*22.7/(22.7+170), 0.1*22.7/(22.7+170))
)

# Define the function
my_fun <- function(h, hp) h / (h + hp)

# Initial estimates for the parameters
start_params <- c(hp = median(data$h))

# Lower bounds for the parameters
lower_bounds <- c(hp = 0)

# Fit the model with constraints
fit <- nlsLM(p ~ my_fun(h, hp),
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

x <- seq(0, 1, 0.0001)

# Generating fitted values for the bounds
fitted_lower <- x / (x + lower_bound)
fitted_upper <- x / (x + upper_bound)

tiff("figureS3.tiff", units = 'in', width = 7, height = 5, res = 700)

# Plot
plot(data$h, data$p, 
     xlab="Flow velocity H (m s-1)", ylab="Relative number of suspended particles P/Pmax", pch=19, col="blue", xlim=c(0, 0.5), ylim=c(0, 1), xaxs="i", yaxs="i")
lines(x, x/(x+estimated_params), col="red", type="l")

# Add lines for the confidence interval bounds
lines(x, fitted_lower, col="black", type="l")
lines(x, fitted_upper, col="black", type="l")

# Create a polygon to fill the area between the lower and upper bounds
x_combined <- c(x, rev(x))  # Combine and reverse x for a closed shape
y_combined <- c(fitted_lower, rev(fitted_upper))
polygon(x_combined, y_combined, col="grey", border=NA)

# Re-draw the main fitted line and data points to overlay the grey area
lines(x, x/(x+estimated_params), col="red", type="l")
points(data$h, data$p, pch=19, col="blue")
legend("topright", legend=c("Data points", "Fitted function", "95% CI"), pch=19, col=c("blue", "red", "grey"), bty="n")

# Plot
plot(data$h, data$p, 
     xlab="Flow velocity H (m s-1)", ylab="Relative number of suspended particles P/Pmax", pch=19, col="blue", xlim=c(0, 0.5), ylim=c(0, 1), xaxs="i", yaxs="i")
lines(x, x/(x+estimated_params), col="red", type="l")

# Add lines for the confidence interval bounds
lines(x, fitted_lower, col="black", type="l")
lines(x, fitted_upper, col="black", type="l")

# Create a polygon to fill the area between the lower and upper bounds
x_combined <- c(x, rev(x))  # Combine and reverse x for a closed shape
y_combined <- c(fitted_lower, rev(fitted_upper))
polygon(x_combined, y_combined, col="grey", border=NA)

# Re-draw the main fitted line and data points to overlay the grey area
lines(x, x/(x+estimated_params), col="red", type="l")
points(data$h, data$p, pch=19, col="blue")
legend("topright", legend=c("Data points", "Fitted function", "95% CI"), pch=19, col=c("blue", "red", "grey"), bty="n")
