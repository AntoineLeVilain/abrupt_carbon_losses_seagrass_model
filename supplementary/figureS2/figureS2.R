# Working directory
setwd('/supplementary/figureS2') # set your working directory

# Data
data <- data.frame(
  s = c(0.97, 0.81, 0.56, 0.27, 0.22, 0.16, 0.13),
  hmax_rmax = c(9.090909091, 13.18181818, 17.27272727, 20.90909091, 24.54545455, 28.63636364, 32.72727273)
)

# Linear Regression with intercept set to 1
model <- lm(I(s-1) ~ hmax_rmax + 0, data=data)

# Print the estimated parameter
print(summary(model))

# Calculate 5% and 95% confidence intervals
conf_intervals <- confint(model, level = 0.90)  # Adjust the level as needed
print(conf_intervals)

# Extracting lower and upper bounds
lower_bound <- conf_intervals[1]
upper_bound <- conf_intervals[2]

x <- seq(0, 35, 1)

# Generating fitted values for the bounds
fitted_lower <- lower_bound*x + 1
fitted_upper <- upper_bound*x + 1

# Plotting
tiff("figureS2.tiff", units = 'in', width = 7, height = 5, res = 700)

plot(data$hmax_rmax, data$s, 
     xlab="H/0.011 (m s-1 day)", ylab="Seagras percent cover s", pch=19, col="blue", xlim=c(0, 35), ylim=c(0, 1), xaxs="i", yaxs="i")
abline(a=1, b=coef(model)[1], col="red")  # adds the linear regression line with intercept at 1

# Add lines for the confidence interval bounds
lines(x, fitted_lower, col="black", type="l")
lines(x, fitted_upper, col="black", type="l")

# Create a polygon to fill the area between the lower and upper bounds
x_combined <- c(x, rev(x))  # Combine and reverse x for a closed shape
y_combined <- c(fitted_lower, rev(fitted_upper))
polygon(x_combined, y_combined, col="grey", border=NA)

# Re-draw the main fitted line and data points to overlay the grey area
abline(a=1, b=coef(model)[1], col="red")
points(data$hmax_rmax, data$s, pch=19, col="blue")
legend("topright", legend=c("Data points", "Fitted function", "95% CI"), pch=19, col=c("blue", "red", "grey"), bty="n")

plot(data$hmax_rmax, data$s, 
     xlab="H/0.011 (m s-1 day)", ylab="Seagras percent cover s", pch=19, col="blue", xlim=c(0, 35), ylim=c(0, 1), xaxs="i", yaxs="i")
abline(a=1, b=coef(model)[1], col="red")  # adds the linear regression line with intercept at 1

# Add lines for the confidence interval bounds
lines(x, fitted_lower, col="black", type="l")
lines(x, fitted_upper, col="black", type="l")

# Create a polygon to fill the area between the lower and upper bounds
x_combined <- c(x, rev(x))  # Combine and reverse x for a closed shape
y_combined <- c(fitted_lower, rev(fitted_upper))
polygon(x_combined, y_combined, col="grey", border=NA)

# Re-draw the main fitted line and data points to overlay the grey area
abline(a=1, b=coef(model)[1], col="red")
points(data$hmax_rmax, data$s, pch=19, col="blue")
legend("topright", legend=c("Data points", "Fitted function", "95% CI"), pch=19, col=c("blue", "red", "grey"), bty="n")

