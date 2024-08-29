# Working directory
setwd('/supplementary/figureS4') # set your working directory

# Data
data <- data.frame(
  tss = c(3.8, 5.8, 6.0, 5.8, 6.3, 6.9, 7.1, 8.1, 9.1, 10.2, 10.3, 11.4, 12.9, 13.3, 11.8, 11.0, 18.1, 18.6, 19.2, 21.0, 32.8, 40.9),
  k = c(1.09, 0.75, 0.98, 1.01, 1.26, 1.18, 1.38, 1.24, 1.38, 1.08, 1.50, 0.82, 0.81, 0.96, 1.91, 2.27, 2.40, 2.81, 1.98, 1.98, 2.62, 3.6)
)

# Linear Regression with intercept set to 1
model <- lm(I(k) ~ tss, data=data)

# Print the estimated parameter
print(summary(model))

# Calculate 5% and 95% confidence intervals
conf_intervals <- confint(model, level = 0.90)  # Adjust the level as needed
print(conf_intervals)

# Extracting lower and upper bounds
lower_bound <- conf_intervals[2, 1]
upper_bound <- conf_intervals[2, 2]

x <- seq(0, 50, 1)

# Generating fitted values for the bounds
fitted_lower <- lower_bound*x + conf_intervals[1, 1]
fitted_upper <- upper_bound*x + conf_intervals[1, 2]

# Plotting
tiff("figureS4.tiff", units = 'in', width = 7, height = 5, res = 700)

plot(data$tss, data$k, 
     xlab="Total suspended particles TSS (g m-3)", ylab="Light attenuation coefficient Kd (m-1)", pch=19, col="blue", xlim=c(0, 40), ylim=c(0, 5), xaxs="i", yaxs="i")
abline(a=coef(model)[1], b=coef(model)[2], col="red")  # adds the linear regression line with intercept at 1

# Add lines for the confidence interval bounds
lines(x, fitted_lower, col="black", type="l")
lines(x, fitted_upper, col="black", type="l")

# Create a polygon to fill the area between the lower and upper bounds
x_combined <- c(x, rev(x))  # Combine and reverse x for a closed shape
y_combined <- c(fitted_lower, rev(fitted_upper))
polygon(x_combined, y_combined, col="grey", border=NA)

# Re-draw the main fitted line and data points to overlay the grey area
abline(a=coef(model)[1], b=coef(model)[2], col="red")
points(data$tss, data$k, pch=19, col="blue")
legend("topright", legend=c("Data points", "Fitted function", "95% CI"), pch=19, col=c("blue", "red", "grey"), bty="n")

plot(data$tss, data$k, 
     xlab="Total suspended particles TSS (g m-3)", ylab="Light attenuation coefficient Kd (m-1)", pch=19, col="blue", xlim=c(0, 40), ylim=c(0, 5), xaxs="i", yaxs="i")
abline(a=coef(model)[1], b=coef(model)[2], col="red")  # adds the linear regression line with intercept at 1

# Add lines for the confidence interval bounds
lines(x, fitted_lower, col="black", type="l")
lines(x, fitted_upper, col="black", type="l")

# Create a polygon to fill the area between the lower and upper bounds
x_combined <- c(x, rev(x))  # Combine and reverse x for a closed shape
y_combined <- c(fitted_lower, rev(fitted_upper))
polygon(x_combined, y_combined, col="grey", border=NA)

# Re-draw the main fitted line and data points to overlay the grey area
abline(a=coef(model)[1], b=coef(model)[2], col="red")
points(data$tss, data$k, pch=19, col="blue")
legend("topright", legend=c("Data points", "Fitted function", "95% CI"), pch=19, col=c("blue", "red", "grey"), bty="n")
