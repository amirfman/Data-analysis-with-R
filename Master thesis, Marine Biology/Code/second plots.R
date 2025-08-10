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


data <- read.delim('Cod data_FD.csv',header=TRUE, sep = ",")
print(data)
# Check the column names
names(data)


selected_columns <- data[, c("regular","female" )]
print(selected_columns)



selected_columns_sd <- selected_columns %>%
  group_by(female) %>%
  summarise_all(sd)

print(selected_columns_sd)

selected_columns_mean <- selected_columns %>%
  group_by(female) %>%
  summarise_all(mean)

print(selected_columns_mean)

df <- merge(selected_columns_mean, selected_columns_sd, by = "female")

print(df)


# Plotting
ggplot(df, aes(x = female, y = regular.x)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = regular.x - regular.y, ymax = regular.x + regular.y),
                width = 0.3, position = position_dodge(0.7)) +
  labs(title = "",
       x = "Cod Female Groups",
       y = "Regular Mean (%)",
       caption = "Error bars represent standard deviation") +
  theme_minimal()



selected_columns <- data[, c("abnormal","female" )]
print(selected_columns)



selected_columns_sd <- selected_columns %>%
  group_by(female) %>%
  summarise_all(sd)

print(selected_columns_sd)

selected_columns_mean <- selected_columns %>%
  group_by(female) %>%
  summarise_all(mean)

print(selected_columns_mean)

df <- merge(selected_columns_mean, selected_columns_sd, by = "female")

print(df)


# Plotting
ggplot(df, aes(x = female, y = abnormal.x)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = abnormal.x - abnormal.y, ymax = abnormal.x + abnormal.y),
                width = 0.3, position = position_dodge(0.7)) +
  labs(title = "",
       x = "Cod Female Groups",
       y = "Abnormal Mean (%)",
       caption = "Error bars represent standard deviation") +
  theme_minimal()

selected_columns <- data[, c("necrotic","female" )]
print(selected_columns)



selected_columns_sd <- selected_columns %>%
  group_by(female) %>%
  summarise_all(sd)

print(selected_columns_sd)

selected_columns_mean <- selected_columns %>%
  group_by(female) %>%
  summarise_all(mean)

print(selected_columns_mean)

df <- merge(selected_columns_mean, selected_columns_sd, by = "female")

print(df)


# Plotting
ggplot(df, aes(x = female, y = necrotic.x)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = necrotic.x - necrotic.y, ymax = necrotic.x + necrotic.y),
                width = 0.3, position = position_dodge(0.7)) +
  labs(title = "",
       x = "Cod Female Groups",
       y = " Necrotic Mean (%)",
       caption = "Error bars represent standard deviation") +
  theme_minimal()

selected_columns <- data[, c("unfertilized","female" )]
print(selected_columns)



selected_columns_sd <- selected_columns %>%
  group_by(female) %>%
  summarise_all(sd)

print(selected_columns_sd)

selected_columns_mean <- selected_columns %>%
  group_by(female) %>%
  summarise_all(mean)

print(selected_columns_mean)

df <- merge(selected_columns_mean, selected_columns_sd, by = "female")

print(df)


# Plotting
ggplot(df, aes(x = female, y = unfertilized.x)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = unfertilized.x - unfertilized.y, ymax = unfertilized.x + unfertilized.y),
                width = 0.3, position = position_dodge(0.7)) +
  labs(title = "",
       x = "Cod Female Groups",
       y = "Unfertilized Mean (%)",
       caption = "Error bars represent standard deviation") +
  theme_minimal()


selected_columns <- data[, c("nonviable","female" )]
print(selected_columns)



selected_columns_sd <- selected_columns %>%
  group_by(female) %>%
  summarise_all(sd)

print(selected_columns_sd)

selected_columns_mean <- selected_columns %>%
  group_by(female) %>%
  summarise_all(mean)

print(selected_columns_mean)

df <- merge(selected_columns_mean, selected_columns_sd, by = "female")

print(df)


# Plotting
ggplot(df, aes(x = female, y = nonviable.x)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = nonviable.x - nonviable.y, ymax = nonviable.x + nonviable.y),
                width = 0.3, position = position_dodge(0.7)) +
  labs(title = "",
       x = "Cod Female Groups",
       y = " Nonviable Mean (%)",
       caption = "Error bars represent standard deviation") +
  theme_minimal()

selected_columns <- data[, c("fert.success..","female" )]
print(selected_columns)



selected_columns_sd <- selected_columns %>%
  group_by(female) %>%
  summarise_all(sd)

print(selected_columns_sd)

selected_columns_mean <- selected_columns %>%
  group_by(female) %>%
  summarise_all(mean)

print(selected_columns_mean)

df <- merge(selected_columns_mean, selected_columns_sd, by = "female")

print(df)


# Plotting
ggplot(df, aes(x = female, y = fert.success...x)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = fert.success...x - fert.success...y, ymax = fert.success...x + fert.success...y),
                width = 0.3, position = position_dodge(0.7)) +
  labs(title = "",
       x = "Cod Female Groups",
       y = "Fertilization success Mean (%)",
       caption = "Error bars represent standard deviation") +
  theme_minimal()





data <- read.delim('Cod data_FD.csv', header = TRUE, sep = ",")
selected_columns <- data[, c("regular","abnormal", "necrotic", "unfertilized", "nonviable", "female")]

# Melt the data to long format
selected_columns_long <- melt(selected_columns, id.vars = "female")

# Calculate mean and standard deviation for each variable
summary_stats <- selected_columns_long %>%
  group_by(variable, female) %>%
  summarise_all(list(mean = mean, sd = sd)) %>%
  ungroup()

# Plotting
ggplot(summary_stats, aes(x = female, y = mean, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.3, position = position_dodge(width = 0.7)) +
  labs(title = "",
       x = "Cod Female Groups",
       y = "Mean Value (%)",
       caption = "Error bars represent standard deviation") +
  theme_minimal() +
  facet_wrap(~ variable, scales = "free_y", ncol = 2)+   # Adjust the limits as needed
  
  # Increase font size
  theme(text = element_text(size = 20))  # Adjust the size as needed





