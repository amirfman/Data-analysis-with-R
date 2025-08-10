# Load required libraries
library(mgcv)
library(MASS)
library(caroline)

# Define the data frame
fs <- data.frame(
  temp = c(
    3.38, 4.28, 5.36, 6.46, 7.44, 8.58, 9.58, 10.80, 12.44, 14.14,
    3.38, 4.28, 5.36, 6.46, 7.44, 8.58, 9.58, 10.80, 12.44, 14.14,
    3.38, 4.28, 5.36, 6.46, 7.44, 8.58, 9.58, 10.80, 12.44, 14.14,
    3.38, 4.28, 5.36, 6.46, 7.44, 8.58, 9.58, 10.80, 12.44, 14.14
  ),
  FERTILIZATION_Type = rep(
    c('PF1FERTILIZATION SUCCESS', 'PF2FERTILIZATION SUCCESS', 'PF3FERTILIZATION SUCCESS', 'PF4FERTILIZATION SUCCESS'),
    each = 10
  ),
  fert = c(72.000000, 78.481013, 71.621622, 79.487179, 68.888889, 78.048780, 80.000000, 61.956522, 63.333333, 52.941176,
           40.677966, 43.548387, 50.847458, 38.095238, 48.484848, 47.297297, 56.923077, 49.275362, 46.666667, 33.783784,
           77.083333, 82.978723, 70.930233, 86.315789, 89.873418, 80.851064, 79.591837, 78.651685, 71.428571, 60.810811,
           78.301887, 69.306931, 72.727273, 69.306931, 69.523810, 60.714286, 62.626263, 66.336634, 56.074766, 41.747573)
)

# Fit a GAM model
mod <- gam(fert ~ s(temp, k = 3), data = fs)

# Calculate AIC
aic <- AIC(mod)
cat("AIC:", aic, "\n")

# Predict using the model
p <- data.frame(temp = seq(min(fs$temp), max(fs$temp), length = 500))
pp <- predict(mod, newdata = p, type = "response")

# Plot the results (you can customize the plot as needed)
plot(fs$fert ~ fs$temp)
lines(pp ~ p$temp, col = "red")

# Save results to a file (replace 'your_output_filename.txt' with the desired file name)
write.table(data.frame(p$temp, pp), file = 'your_output_filename.txt', row.names = FALSE, col.names = FALSE)


# Define a sequence of 'temp' values for prediction
p <- data.frame(temp = seq(min(fs$temp), max(fs$temp), length = 500))

# Predict using the GAM model
pp <- predict(mod, newdata = p, type = "response")

# Create a scatterplot of 'fs$fert' against 'fs$temp'
plot(fs$fert ~ fs$temp)

# Add the GAM predictions to the plot
lines(pp ~ p$temp, data = p, col = "red")

# Calculate the lpmatrix of the predictions
Xp <- predict(mod, p, type = "lpmatrix")

# Extract the coefficients and variance-covariance matrix
beta <- coef(mod)
Vb <- vcov(mod)

# Set the random seed for reproducibility
set.seed(10)

# Perform a Monte Carlo simulation
n <- 1000
mrand <- mvrnorm(n, beta, Vb)

# Initialize an empty vector to store the optimal values
opt <- rep(NA, n)

# Define the inverse link function
ilink <- family(mod)$linkinv

# Perform simulations and find optimal 'temp' values
for (i in seq_len(n)) { 
  pred <- ilink(Xp %*% mrand[i, ])
  opt[i] <- p$temp[which.max(pred)]
}

# Calculate quantiles of the optimal 'temp' values
ci <- quantile(opt, c(0.05, 0.95))
cat("Confidence Interval:", ci, "\n")

# Find the 'temp' value corresponding to the maximum 'pp'
x <- p$temp[which.max(pp)]
cat("Maximum 'temp' for maximum 'pp':", x, "\n")

# Store the current maximum 'temp' value
xm <- x

# Plot the data with the maximum 'temp'
plot(fs$fert ~ fs$temp)

# Add a vertical line at the maximum 'temp' value
abline(v = p$temp[which.max(pp)], lty = "dashed", col = "grey")

# Add the GAM predictions
lines(pp ~ temp, data = p, col = "red")

# Add blue lines indicating the confidence interval
lines(y = rep(min(fs$fert), 2), x = ci, col = "blue")

# Add a blue point at the minimum 'fs$fert'
points(y = min(fs$fert), x = p$temp[which.max(pp)], pch = 16, col = "blue")

# Subsetting data for 'temp' values less than or equal to 'xm'
px <- as.data.frame(p[p$temp <= xm, ])
colnames(px) <- colnames(p)

# Predict for the subset of data
pp <- predict(mod, newdata = px, type = "response")

# Create a scatterplot for the subset
plot(fs$fert ~ fs$temp)
lines(pp ~ temp, data = px, col = "red")

# Calculate lpmatrix, coefficients, and variance-covariance matrix for the subset
Xp <- predict(mod, px, type = "lpmatrix")
beta <- coef(mod)
Vb <- vcov(mod)

# Set the random seed for reproducibility
set.seed(10)

# Define a variance parameter
var <- 0.90

# Initialize an empty vector to store optimal 'temp' values
opt <- rep(NA, n)

# Perform simulations for the subset and find optimal 'temp' values
for (i in seq_len(n)) { 
  pred <- ilink(Xp %*% mrand[i, ])
  opt[i] <- px$temp[which.min(abs(pred - max(pred) * var))]
}

# Calculate quantiles of the optimal 'temp' values for the subset
ci <- quantile(opt, c(0.05, 0.95))
cat("Confidence Interval for Subset:", ci, "\n")

# Find the 'temp' value corresponding to the minimum prediction for the subset
x <- px$temp[which.min(abs(pred - max(pred) * var))]
cat("Optimal 'temp' for Subset:", x, "\n")

# Plot the data for the subset
plot(fs$fert ~ fs$temp)
abline(v = px$temp[which.min(abs(pred - max(pred) * var))], lty = "dashed", col = "grey")
lines(pp ~ temp, data = px, col = "red")
lines(y = rep(min(fs$fert), 2), x = ci, col = "blue")
points(y = min(fs$fert), x = px$temp[which.min(abs(pred - max(pred) * var))], pch = 16, col = "blue")

# Subset the data for 'temp' values greater than or equal to 'xm'
px <- as.data.frame(p[p$temp >= xm, ])
colnames(px) <- colnames(p)

# Predict for the subset of data
pp <- predict(mod, newdata = px, type = "response")

# Create a scatterplot for the subset
plot(fs$fert ~ fs$temp)
lines(pp ~ temp, data = px, col = "red")

# Calculate lpmatrix, coefficients, and variance-covariance matrix for the subset
Xp <- predict(mod, px, type = "lpmatrix")
beta <- coef(mod)
Vb <- vcov(mod)

# Set the random seed for reproducibility
set.seed(10)

# Define a variance parameter
var <- 0.90

# Initialize an empty vector to store optimal 'temp' values
opt <- rep(NA, n)

# Perform simulations for the subset and find optimal 'temp' values
for (i in seq_len(n)) { 
  pred <- ilink(Xp %*% mrand[i, ])
  opt[i] <- px$temp[which.min(abs(pred - max(pred) * var))]
}

# Calculate quantiles of the optimal 'temp' values for the subset
ci <- quantile(opt, c(0.05, 0.95))
cat("Confidence Interval for Subset (Right):", ci, "\n")

# Find the 'temp' value corresponding to the minimum prediction for the subset
x <- px$temp[which.min(abs(pred - max(pred) * var))]
cat("Optimal 'temp' for Subset (Right):", x, "\n")

# Plot the data for the subset
plot(fs$fert ~ fs$temp)
abline(v = px$temp[which.min(abs(pred - max(pred) * var))], lty = "dashed", col = "grey")
lines(pp ~ temp, data = px, col = "red")
lines(y = rep(min(fs$fert), 2
              
              



