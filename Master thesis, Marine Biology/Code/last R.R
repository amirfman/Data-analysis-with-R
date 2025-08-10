library(ggplot2)
library(mgcv)
library(MASS)
library(caroline)
library(tidyverse)
library(tidyr)
library(scales)
library(dplyr)

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


selected_columns <- data[, c("Temperature","regular", "abnormal", "necrotic","nonviable","unfertilized","female")]
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

# Plotting with position = "stack"
ggplot(selected_columns_long, aes(x = female, y = Value, fill = Category)) +
  geom_bar(stat = "identity", position ="stack") +
  scale_fill_manual(values = c("regular" = "blue", "abnormal" = "green", "necrotic" = "yellow", "unfertilized" = "grey", "nonviable" = "red")) +
  labs(title = "",
       x = "female°C", y = "Percent") +
  theme_minimal() +
  
  # Set x-axis breaks
  scale_x_continuous(breaks = c(-1.5, 0.3, 2.2, 3.8, 5.4, 6.9, 8.2, 9.4, 10.8, 12.0))+
  theme(legend.position = "top")+   # Adjust the limits as needed
  
  # Increase font size
  theme(text = element_text(size = 16))+   # Adjust the limits as needed
  
  # Increase font size
  theme(text = element_text(size = 20))  # Adjust the size as needed


