library(mgcv)
# Load the 'mgcv' package if not already loaded
if (!require(mgcv)) {
  install.packages("mgcv")
  library(mgcv)
}

# Create a data frame from the provided data
data <- data.frame(
  CMEANTEMP = c(-1.5, 0.3, 2.2, 3.8, 5.4, 6.9, 8.2, 9.4, 10.8, 12.0,
                       -1.5, 0.3, 2.2, 3.8, 5.4, 6.9, 8.2, 9.4, 10.8, 12.0,
                       -1.5, 0.3, 2.2, 3.8, 5.4, 6.9, 8.2, 9.4, 10.8, 12.0,
                       -1.5, 0.3, 2.2, 3.8, 5.4, 6.9, 8.2, 9.4, 10.8, 12.0,
                       -1.5, 0.3, 2.2, 3.8, 5.4, 6.9, 8.2, 9.4, 10.8, 12.0),
  FERTILIZATION_Type = rep(c('CF1FERTILIZATION SUCCESS', 'CF2FERTILIZATION SUCCESS', 
                             'CF3FERTILIZATION SUCCESS', 'CF4FERTILIZATION SUCCESS', 'CF5FERTILIZATION SUCCESS'), each = 10),
  FERTILIZATION_Success = c(58.119658, 69.930070, 83.333333, 84.745763, 88.549618,
                            91.836735, 96.402878, 87.500000, 84.892086, 73.958333,
                            50.495050, 83.443709, 89.932886, 95.394737, 91.608392,
                            94.890511, 94.927536, 86.805556, 80.985915, 82.051282,
                            77.981651, 85.470085, 97.894737, 91.150442, 92.920354,
                            85.714286, 86.554622, 84.070796, 83.333333, 68.354430,
                            80.000000, 86.400000, 83.333333, 85.000000, 81.343284,
                            85.517241, 82.945736, 80.434783, 83.941606, 56.603774,
                            2.702703, 31.818182, 43.243243, 60.810811, 61.585366,
                            79.393939, 74.585635, 83.139535, 84.242424, 81.904762)
)

# Fit a Generalized Additive Model (GAM)
gam_model <- gam(FERTILIZATION_Success ~ s(CMEANTEMP), data = data)

# Obtain the summary table
summary_table <- summary(gam_model)
summary_table

