install.packages("Matrix")

# Get the current working directory
current_directory <- getwd()



# Print the current working directory
print(current_directory)
# Set a new working directory
setwd("C:/uni hamburg/Thesis")
print(getwd())


data <- read.delim('Cod data_FD.csv',header=TRUE, sep = ",")
print(data)
# Check the column names
names(data)


selected_columns <- data[, c("Temperature","fert.success..","female" )]
print(selected_columns)

# Fit a GAM
gam_model <- gam(fert.success.. ~ s(Temperature) + female, data = selected_columns)

# Print the summary of the model
summary(gam_model)

gamm_model <- gamm(fert.success.. ~ s(Temperature, by = female) + s(Temperature, by = female), random = list(female = ~1), data = selected_columns)

# Print the summary of the model
summary(gamm_model)




























#selected_columns <- selected_columns %>% arrange(Temperature)
#print(selected_columns)

# Fit Linear Mixed Effects Model
lmm_model <- lme(fert.success.. ~ Temperature, random = ~1 | female, data = selected_columns)

# Print the summary of the model
summary(lmm_model)

# Check levels of 'female'
levels(selected_columns$female)

# Make sure 'female' is a factor
selected_columns$female <- as.factor(selected_columns$female)

emm <- emmeans(lmm_model, "Temperature", by = "female")
pairwise_comparisons <- pairs(emm)
print(pairwise_comparisons)
plot(pairwise_comparisons)


# Get emmeans for the temperature contrast
emm <- emmeans(lmm_model, specs = pairwise ~ Temperature, by = "female")

# Conduct Tukey's HSD for the temperature contrast
tukey_results <- emmeans::contrast(emm, method = "tukey")

# Display Tukey's HSD results
print(tukey_results)

# Plot the results
plot(tukey_results)





# Extract predicted values and standard errors
predicted_values <- predict(lmm_model, level = 0)
standard_errors <- predict(lmm_model, level = 1)

# Combine the data
plot_data <- cbind(selected_columns, predicted_values, standard_errors)

# Scatter plot with error bars
ggplot(plot_data, aes(x = Temperature, y = fert.success.., group = female, color = female)) +
  geom_point() +
  geom_errorbar(aes(ymin = fert.success.. - standard_errors, ymax = fert.success.. + standard_errors), width = 0.2) +
  geom_line(aes(y = predicted_values), linetype = "dashed") +
  labs(title = "Scatter Plot with Error Bars",
       x = "Temperature",
       y = "fert.success..") +
  theme_minimal()




