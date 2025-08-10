library(ggplot2)
library(mgcv)
library(MASS)
library(caroline)
library(tidyverse)
library(tidyr)
library(scales)
library(dplyr)
library(knitr)
library(kableExtra)

# Get the current working directory
current_directory <- getwd()



# Print the current working directory
print(current_directory)
# Set a new working directory
setwd("C:/uni hamburg/Thesis")
print(getwd())


data <- read.delim('plaice data_FD.csv',header=TRUE, sep = ",")
print(data)
# Check the column names
names(data)


selected_columns <- data[, c("regular", "abnormal", "necrotic","nonviable","unfertilized","female")]
print(selected_columns)

selected_columns <- selected_columns %>%
  group_by(female) %>%
  summarise_all(mean)


print(selected_columns)

# Reshape the data to long format
selected_columns_long <- pivot_longer(as_tibble(selected_columns), cols = -female, names_to = "Category", values_to = "Value")

# Reorder the levels of "Category"
selected_columns_long$Category <- factor(
  selected_columns_long$Category,
  levels = c("unfertilized", "necrotic","abnormal","nonviable","regular")
)

# Convert "female" to a factor
selected_columns_long$female <- factor(selected_columns_long$female)

ggplot(selected_columns_long, aes(x = female, y = Value, fill = Category)) +
  geom_bar(stat = "identity", position = position_stack()) +  # Start from zero
  scale_fill_manual(values = c("regular" = "blue" ,"abnormal" = "green", "necrotic" = "yellow", "unfertilized" = "grey", "nonviable" = "red")) +
  labs(title = "",
       x = "female", y = "Percent") +
  theme_minimal() +
  scale_x_discrete() +  # Use scale_x_discrete() for discrete x-axis
  theme(legend.position = "top") +
  theme(text = element_text(size = 16)) +
  theme(text = element_text(size = 20))

# Fit ANOVA model
anova_model <- aov(Value ~ Category * female, data = selected_columns_long)

# Summary of ANOVA
summary(anova_model)










