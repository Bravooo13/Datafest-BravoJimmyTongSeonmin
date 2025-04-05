library(dplyr)
library(ggplot2)

houston_data <- lease_data %>%
  filter(market == "Houston")

# Create a time variable for better x-axis labeling
houston_data <- houston_data %>%
  mutate(time = paste(year, "Q", quarter, sep = ""))

################################### Rent Trend ##############################
# Group by time and calculate average rent per quarter
houston_rent_trend <- houston_data %>%
  group_by(time) %>%
  summarize(avg_rent = mean(overall_rent, na.rm = TRUE)) %>%
  arrange(time)

# Plot the line graph
houston_rent_plot <- ggplot(houston_rent_trend, aes(x = time, y = avg_rent, group = 1)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "darkblue") +
  labs(title = "Overall Rent Trend in Houston (2018 Q1 – 2024 Q4)",
       x = "Quarter",
       y = "Average Overall Rent (USD)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

############################ Availability proportion #########################
# Group by quarter and keep only one availability_proportion per quarter
houston_availability_prop <- lease_data %>%
  filter(market == "Houston") %>%
  mutate(time = paste(year, "Q", quarter, sep = "")) %>%
  group_by(time) %>%
  summarize(availability_proportion = first(availability_proportion)) %>%
  arrange(time)

# Convert to ordered factor to ensure correct x-axis order
houston_availability_prop$time <- factor(houston_availability_prop$time, levels = unique(houston_availability_prop$time), ordered = TRUE)

# Plot
houston_avail_plot <- ggplot(houston_availability_prop, aes(x = time, y = availability_proportion * 100, group = 1)) +
  geom_line(color = "darkred", size = 1.2) +
  geom_point(color = "firebrick", size = 2.5) +
  labs(
    title = "Availability Proportion in Houston (2018–2024)",
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
houston_occupancy <- market_occupancy %>%
  filter(market == "Houston") %>%
  mutate(time = paste(year, "Q", quarter, sep = "")) %>%
  arrange(year, quarter)

# Make sure 'time' is ordered for proper plotting
houston_occupancy$time <- factor(houston_occupancy$time, levels = unique(houston_occupancy$time), ordered = TRUE)

# Plot the line graph
houston_occupancy_plot <- ggplot(houston_occupancy, aes(x = time, y = avg_occupancy_proportion * 100, group = 1)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "navy", size = 2.5) +
  labs(
    title = "Average Occupancy Proportion in Houston (2018–2024)",
    x = "Quarter",
    y = "Occupancy (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )


############################## Save Graphs ###################################
ggsave("Plots/houston_rent_trend.png", plot = houston_rent_plot, width = 8, height = 5, dpi = 300)
ggsave("Plots/houston_availability_trend.png", plot = houston_avail_plot, width = 8, height = 5, dpi = 300)
ggsave("Plots/houston_occupancy_trend.png", plot = houston_occupancy_plot, width = 8, height = 5, dpi = 300)




