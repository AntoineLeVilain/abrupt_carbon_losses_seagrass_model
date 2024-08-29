# Library
library(minpack.lm)

# Working directory
setwd('/supplementary/figureS7') # set your working directory

# Data
data <- data.frame(
corg = c(0.0043, 0.0051, 0.0067, 0.0118, 0.0128, 0.0113, 0.0255), 
h = c(0.05, 0.07, 0.1, 0.15, 0.17, 0.21, 0.26)
)

# Define the function
my_fun <- function(h, hea) h / (h + hea)

# Initial estimates for the parameters
start_params <- c(hea = median(data$h))

# Lower bounds for the parameters
lower_bounds <- c(hea = 0)

# Fit the model with constraints
fit <- nlsLM(corg ~ my_fun(h, hea),
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

x <- seq(0, 5, 0.01)

# Generating fitted values for the bounds
fitted_lower <- x / (x + lower_bound)
fitted_upper <- x / (x + upper_bound)

tiff("figureS7.tiff", units = 'in', width = 7, height = 5, res = 700)

plot(data$h, data$corg*100, 
     xlab="Flow velocity H (m s-1)", ylab="Corg lost dCA/(CAdt) (%)", pch=19, col="blue", xlim=c(0, 0.3), ylim=c(0, 10), xaxs="i", yaxs="i")
lines(x, x/(x+estimated_params)*100, col="red", type="l")

# Add lines for the confidence interval bounds
lines(x, fitted_lower*100, col="black", type="l")
lines(x, fitted_upper*100, col="black", type="l")

# Create a polygon to fill the area between the lower and upper bounds
x_combined <- c(x, rev(x))  # Combine and reverse x for a closed shape
y_combined <- c(fitted_lower, rev(fitted_upper))*100
polygon(x_combined, y_combined, col="grey", border=NA)

lines(x, x/(x+estimated_params)*100, col="red", type="l")
points(data$h, data$corg*100, pch=19, col="blue")
legend("topright", legend=c("Data points", "Fitted function", "95% CI"), pch=19, col=c("blue", "red", "grey"), bty="n")

plot(data$h, data$corg*100, 
     xlab="Flow velocity H (m s-1)", ylab="Corg lost dCA/(CAdt) (%)", pch=19, col="blue", xlim=c(0, 0.3), ylim=c(0, 10), xaxs="i", yaxs="i")
lines(x, x/(x+estimated_params)*100, col="red", type="l")

# Add lines for the confidence interval bounds
lines(x, fitted_lower*100, col="black", type="l")
lines(x, fitted_upper*100, col="black", type="l")

# Create a polygon to fill the area between the lower and upper bounds
x_combined <- c(x, rev(x))  # Combine and reverse x for a closed shape
y_combined <- c(fitted_lower, rev(fitted_upper))*100
polygon(x_combined, y_combined, col="grey", border=NA)

lines(x, x/(x+estimated_params)*100, col="red", type="l")
points(data$h, data$corg*100, pch=19, col="blue")
legend("topright", legend=c("Data points", "Fitted function", "95% CI"), pch=19, col=c("blue", "red", "grey"), bty="n")
