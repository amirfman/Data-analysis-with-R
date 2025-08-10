library(ggplot2)
library(dplyr)
library(tidyr)

# Read the data
data <- read.delim('plaice data_FD.csv', header = TRUE, sep = ",")

# Select relevant columns
selected_columns <- data[, c("regular", "abnormal", "necrotic", "unfertilized", "nonviable", "Temperature")]

# Melt the data to long format
selected_columns_long <- melt(selected_columns, id.vars = "Temperature")

# Calculate mean and standard deviation for each variable
summary_stats <- selected_columns_long %>%
  group_by(variable, Temperature) %>%
  summarise_all(list(mean = mean, sd = sd)) %>%
  ungroup()

# Define the size of each subplot
subplot_size <- 10
subplot_name_size <- 20

# Plotting
p <- ggplot(summary_stats, aes(x = Temperature, y = mean, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.3, position = position_dodge(width = 0.7)) +
  labs(title = "",
       x = "Temperature",
       y = "Mean Value (%)",
       caption = "Error bars represent standard deviation") +
  theme_minimal() +
  facet_wrap(~ variable, scales = "free_y", ncol = 1, drop = TRUE, strip.position = "right") +
  scale_x_continuous(breaks = c(3.38, 4.28, 5.36, 6.46, 7.44, 8.58, 9.58, 10.8, 12.44, 14.14)) +
  # Increase font size
  theme(text = element_text(size = 15),
        strip.text = element_text(size = subplot_size))

# Print the plot
print(p)

# Calculate mean and standard deviation for each variable
summary_stats <- selected_columns_long %>%
  group_by(variable, Temperature) %>%
  summarise_all(list(mean = mean, sd = sd)) %>%
  ungroup()
mean_at_minus_1_5 <- summary_stats %>%
  filter(Temperature == -1.5, variable == "fertilization_success") %>%
  select(mean)

print(mean_at_minus_1_5)

