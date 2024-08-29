# Library
library(minpack.lm)

# Working directory
setwd('/supplementary/figureS6') # set your working directory

# Data
data <- data.frame(
  p = c(1, 0.9, 0.89, 0.80, 0.65, 0.54, 0.60, 0.57, 0.46, 0.45, 0.39, 0.40, 0.37, 0.27, 0.30, 0.33), 
  t = 0:15
)

# Define the function
my_fun <- function(t, a) exp(-a*t)

# Initial estimates for the parameters
start_params <- c(a = 0)

# Lower bounds for the parameters
lower_bounds <- c(a = 0)

# Fit the model with constraints
fit <- nlsLM(p ~ my_fun(t, a),
             data = data,
             start = start_params,
             lower = lower_bounds,
             control = nls.lm.control(maxiter = 500))

print(summary(fit))

# Get the estimated parameters
estimated_params <- coef(fit)
estimated_params/365

# Calculate 5% and 95% confidence intervals
conf_intervals <- confint(fit, level = 0.90)  # Adjust the level as needed
conf_intervals/365

# Extracting lower and upper bounds
lower_bound <- conf_intervals[1]
upper_bound <- conf_intervals[2]

x <- seq(0, 15, 0.01)

# Generating fitted values for the bounds
fitted_lower <- exp(-lower_bound*x)*100
fitted_upper <- exp(-upper_bound*x)*100

tiff("figureS6.tiff", units = 'in', width = 7, height = 5, res = 700)

# Plot
plot(data$t, data$p*100, 
     xlab="Years", ylab="Remaining weight (%)", pch=19, col="blue", xlim=c(0, 16), ylim=c(0, 105), xaxs="i", yaxs="i")
lines(x, exp(-estimated_params*x)*100, col="red", type="l")

# Add lines for the confidence interval bounds
lines(x, fitted_lower, col="black", type="l")
lines(x, fitted_upper, col="black", type="l")

# Create a polygon to fill the area between the lower and upper bounds
x_combined <- c(x, rev(x))  # Combine and reverse x for a closed shape
y_combined <- c(fitted_lower, rev(fitted_upper))
polygon(x_combined, y_combined, col="grey", border=NA)

# Re-draw the main fitted line and data points to overlay the grey area
lines(x, exp(-estimated_params*x)*100, col="red", type="l")
points(data$t, data$p*100, pch=19, col="blue")
legend("topright", legend=c("Data points", "Fitted function", "95% CI"), pch=19, col=c("blue", "red", "grey"), bty="n")

plot(data$t, data$p*100, 
     xlab="Years", ylab="Remaining weight (%)", pch=19, col="blue", xlim=c(0, 16), ylim=c(0, 105), xaxs="i", yaxs="i")
lines(x, exp(-estimated_params*x)*100, col="red", type="l")

# Add lines for the confidence interval bounds
lines(x, fitted_lower, col="black", type="l")
lines(x, fitted_upper, col="black", type="l")

# Create a polygon to fill the area between the lower and upper bounds
x_combined <- c(x, rev(x))  # Combine and reverse x for a closed shape
y_combined <- c(fitted_lower, rev(fitted_upper))
polygon(x_combined, y_combined, col="grey", border=NA)

# Re-draw the main fitted line and data points to overlay the grey area
lines(x, exp(-estimated_params*x)*100, col="red", type="l")
points(data$t, data$p*100, pch=19, col="blue")
legend("topright", legend=c("Data points", "Fitted function", "95% CI"), pch=19, col=c("blue", "red", "grey"), bty="n")
