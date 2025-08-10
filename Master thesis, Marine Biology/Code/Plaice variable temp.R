library(ggplot2)
library(mgcv)
library(MASS)
library(caroline)
library(tidyverse)
library(tidyr)
library(scales)
library(dplyr)
library(reshape2)

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


selected_columns <- data[, c("regular","Temperature" )]
print(selected_columns)



selected_columns_sd <- selected_columns %>%
  group_by(Temperature) %>%
  summarise_all(sd)

print(selected_columns_sd)

selected_columns_mean <- selected_columns %>%
  group_by(Temperature) %>%
  summarise_all(mean)

print(selected_columns_mean)

df <- merge(selected_columns_mean, selected_columns_sd, by = "Temperature")

print(df)


# Plotting
ggplot(df, aes(x = Temperature, y = regular.x)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = regular.x - regular.y, ymax = regular.x + regular.y),
                width = 0.3, position = position_dodge(0.7)) +
  labs(title = "",
       x = "Temperature",
       y = "Regular Mean (%)",
       caption = "Error bars represent standard deviation") +
  theme_minimal()



selected_columns <- data[, c("abnormal","Temperature" )]
print(selected_columns)



selected_columns_sd <- selected_columns %>%
  group_by(Temperature) %>%
  summarise_all(sd)

print(selected_columns_sd)

selected_columns_mean <- selected_columns %>%
  group_by(Temperature) %>%
  summarise_all(mean)

print(selected_columns_mean)

df <- merge(selected_columns_mean, selected_columns_sd, by = "Temperature")

print(df)


# Plotting
ggplot(df, aes(x = Temperature, y = abnormal.x)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = abnormal.x - abnormal.y, ymax = abnormal.x + abnormal.y),
                width = 0.3, position = position_dodge(0.7)) +
  labs(title = "",
       x = "Temperature",
       y = "Abnormal Mean (%)",
       caption = "Error bars represent standard deviation") +
  theme_minimal()

selected_columns <- data[, c("necrotic","Temperature" )]
print(selected_columns)



selected_columns_sd <- selected_columns %>%
  group_by(Temperature) %>%
  summarise_all(sd)

print(selected_columns_sd)

selected_columns_mean <- selected_columns %>%
  group_by(Temperature) %>%
  summarise_all(mean)

print(selected_columns_mean)

df <- merge(selected_columns_mean, selected_columns_sd, by = "Temperature")

print(df)


# Plotting
ggplot(df, aes(x = Temperature, y = necrotic.x)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = necrotic.x - necrotic.y, ymax = necrotic.x + necrotic.y),
                width = 0.3, position = position_dodge(0.7)) +
  labs(title = "",
       x = "Temperature",
       y = " Necrotic Mean (%)",
       caption = "Error bars represent standard deviation") +
  theme_minimal()

selected_columns <- data[, c("unfertilized","Temperature" )]
print(selected_columns)



selected_columns_sd <- selected_columns %>%
  group_by(Temperature) %>%
  summarise_all(sd)

print(selected_columns_sd)

selected_columns_mean <- selected_columns %>%
  group_by(Temperature) %>%
  summarise_all(mean)

print(selected_columns_mean)

df <- merge(selected_columns_mean, selected_columns_sd, by = "Temperature")

print(df)


# Plotting
ggplot(df, aes(x = Temperature, y = unfertilized.x)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = unfertilized.x - unfertilized.y, ymax = unfertilized.x + unfertilized.y),
                width = 0.3, position = position_dodge(0.7)) +
  labs(title = "",
       x = "Temperature",
       y = "Unfertilized Mean (%)",
       caption = "Error bars represent standard deviation") +
  theme_minimal()


selected_columns <- data[, c("nonviable","Temperature" )]
print(selected_columns)



selected_columns_sd <- selected_columns %>%
  group_by(Temperature) %>%
  summarise_all(sd)

print(selected_columns_sd)

selected_columns_mean <- selected_columns %>%
  group_by(Temperature) %>%
  summarise_all(mean)

print(selected_columns_mean)

df <- merge(selected_columns_mean, selected_columns_sd, by = "Temperature")

print(df)


# Plotting
ggplot(df, aes(x = Temperature, y = nonviable.x)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = nonviable.x - nonviable.y, ymax = nonviable.x + nonviable.y),
                width = 0.3, position = position_dodge(0.7)) +
  labs(title = "",
       x = "Temperature",
       y = " Nonviable Mean (%)",
       caption = "Error bars represent standard deviation") +
  theme_minimal()

selected_columns <- data[, c("fert.success..","Temperature" )]
print(selected_columns)



selected_columns_sd <- selected_columns %>%
  group_by(Temperature) %>%
  summarise_all(sd)

print(selected_columns_sd)

selected_columns_mean <- selected_columns %>%
  group_by(Temperature) %>%
  summarise_all(mean)

print(selected_columns_mean)

df <- merge(selected_columns_mean, selected_columns_sd, by = "Temperature")

print(df)


# Plotting
ggplot(df, aes(x = Temperature, y = fert.success...x)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = fert.success...x - fert.success...y, ymax = fert.success...x + fert.success...y),
                width = 0.3, position = position_dodge(0.7)) +
  labs(title = "",
       x = "Temperature",
       y = "Fertilization success Mean (%)",
       caption = "Error bars represent standard deviation") +
  theme_minimal()



# Read the data
data <- read.delim('Plaice data_FD.csv', header = TRUE, sep = ",")

# Select relevant columns
selected_columns <- data[, c("regular", "abnormal", "necrotic", "unfertilized", "nonviable", "Temperature")]

# Melt the data to long format
selected_columns_long <- melt(selected_columns, id.vars = "Temperature")

# Calculate mean and standard deviation for each variable
summary_stats <- selected_columns_long %>%
  group_by(variable, Temperature) %>%
  summarise_all(list(mean = mean, sd = sd)) %>%
  ungroup()

# Plotting
ggplot(summary_stats, aes(x = Temperature, y = mean, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.3, position = position_dodge(width = 0.7)) +
  labs(title = "",
       x = "Temperature",
       y = "Mean Value (%)",
       caption = "Error bars represent standard deviation") +
  theme_minimal() +
  facet_wrap(~variable, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = c(3.38, 4.28, 5.36, 6.46, 7.44, 8.58, 9.58, 10.8, 12.44, 14.14))+
  # Increase font size
  theme(text = element_text(size = 15))  # Adjust the size as needed



