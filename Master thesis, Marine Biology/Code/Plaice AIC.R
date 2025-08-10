# Load necessary libraries if not already loaded
library(mgcv)  # For GAM
library(lme4)  # For LMEM

# Create your data frame
data <- data.frame(
  PMEANTEMPERATURE = c(3.38, 4.28, 5.36, 6.46, 7.44, 8.58, 9.58, 10.80, 12.44, 14.14,
                       3.38, 4.28, 5.36, 6.46, 7.44, 8.58, 9.58, 10.80, 12.44, 14.14,
                       3.38, 4.28, 5.36, 6.46, 7.44, 8.58, 9.58, 10.80, 12.44, 14.14,
                       3.38, 4.28, 5.36, 6.46, 7.44, 8.58, 9.58, 10.80, 12.44, 14.14),
  FERTILIZATION_Type = rep(c('PF1FERTILIZATION SUCCESS', 'PF2FERTILIZATION SUCCESS', 'PF3FERTILIZATION SUCCESS', 'PF4FERTILIZATION SUCCESS'), each = 10),
  FERTILIZATION_Success = c(76.623377, 82.142857, 75.949367, 82.926829, 68.888889, 78.048780, 80.000000, 61.956522,
                            63.333333, 52.941176,
                            40.677966, 43.548387, 48.214286, 38.095238, 48.484848, 47.297297, 56.923077, 49.275362,
                            46.666667, 30.000000,
                            77.551020, 82.978723, 75.000000, 86.315789, 89.873418, 80.851064, 79.591837, 78.651685,
                            71.428571, 60.810811,
                            78.301887, 69.306931, 72.727273, 69.306931, 69.523810, 60.714286, 62.626263, 66.336634,
                            56.074766, 41.747573)
)

# Fit the GAM model
gam_model <- gam(FERTILIZATION_Success ~ s(PMEANTEMPERATURE), data = data)
gam_aic <- AIC(gam_model)

# Fit the GLM model
glm_model <- glm(FERTILIZATION_Success ~ PMEANTEMPERATURE, data = data, family = gaussian())
glm_aic <- AIC(glm_model)

# Fit the LMEM model
lmem_model <- lmer(FERTILIZATION_Success ~ PMEANTEMPERATURE + (1|FERTILIZATION_Type), data = data)
lmem_aic <- AIC(lmem_model)

# Print the AIC values for comparison
cat("AIC for GAM Model:", gam_aic, "\n")
cat("AIC for GLM Model:", glm_aic, "\n")
cat("AIC for LMEM Model:", lmem_aic, "\n")
