library(dplyr)
library(ggplot2)

manhattan_data <- lease_data %>%
  filter(market == "Manhattan")

# Create a time variable for better x-axis labeling
manhattan_data <- manhattan_data %>%
  mutate(time = paste(year, "Q", quarter, sep = ""))

################################### Rent Trend ##############################
# Group by time and calculate average rent per quarter
manhattan_rent_trend <- manhattan_data %>%
  group_by(time) %>%
  summarize(avg_rent = mean(overall_rent, na.rm = TRUE)) %>%
  arrange(time)

# Plot the line graph
manhattan_rent_plot <- ggplot(manhattan_rent_trend, aes(x = time, y = avg_rent, group = 1)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "darkblue") +
  labs(title = "Overall Rent Trend in Manhattan (2018 Q1 – 2024 Q4)",
       x = "Quarter",
       y = "Average Overall Rent (USD)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

############################ Availability proportion #########################
# Group by quarter and keep only one availability_proportion per quarter
manhattan_availability_prop <- lease_data %>%
  filter(market == "Manhattan") %>%
  mutate(time = paste(year, "Q", quarter, sep = "")) %>%
  group_by(time) %>%
  summarize(availability_proportion = first(availability_proportion)) %>%
  arrange(time)

# Convert to ordered factor to ensure correct x-axis order
manhattan_availability_prop$time <- factor(manhattan_availability_prop$time, levels = unique(manhattan_availability_prop$time), ordered = TRUE)

# Plot
manhattan_avail_plot <- ggplot(manhattan_availability_prop, aes(x = time, y = availability_proportion * 100, group = 1)) +
  geom_line(color = "darkred", size = 1.2) +
  geom_point(color = "firebrick", size = 2.5) +
  labs(
    title = "Availability Proportion in Manhattan (2018–2024)",
    subtitle = "Quarterly percentage of available office space",
    x = "Quarter",
    y = "Availability (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

################################ Occupancy ##################################
manhattan_occupancy <- market_occupancy %>%
  filter(market == "Manhattan") %>%
  mutate(time = paste(year, "Q", quarter, sep = "")) %>%
  arrange(year, quarter)

# Make sure 'time' is ordered for proper plotting
manhattan_occupancy$time <- factor(manhattan_occupancy$time, levels = unique(manhattan_occupancy$time), ordered = TRUE)

# Plot the line graph
manhattan_occupancy_plot <- ggplot(manhattan_occupancy, aes(x = time, y = avg_occupancy_proportion * 100, group = 1)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "navy", size = 2.5) +
  labs(
    title = "Average Occupancy Proportion in Manhattan (2018–2024)",
    x = "Quarter",
    y = "Occupancy (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )


############################## Save Graphs ###################################
ggsave("Plots/manhattan_rent_trend.png", plot = manhattan_rent_plot, width = 8, height = 5, dpi = 300)
ggsave("Plots/manhattan_availability_trend.png", plot = manhattan_avail_plot, width = 8, height = 5, dpi = 300)
ggsave("Plots/manhattan_occupancy_trend.png", plot = manhattan_occupancy_plot, width = 8, height = 5, dpi = 300)



