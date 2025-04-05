library(dplyr)
library(ggplot2)

la_data <- lease_data %>%
  filter(market == "Los Angeles")

# Create a time variable for better x-axis labeling
la_data <- la_data %>%
  mutate(time = paste(year, "Q", quarter, sep = ""))

################################### Rent Trend ##############################
# Group by time and calculate average rent per quarter
la_rent_trend <- la_data %>%
  group_by(time) %>%
  summarize(avg_rent = mean(overall_rent, na.rm = TRUE)) %>%
  arrange(time)

# Plot the line graph
ggplot(la_rent_trend, aes(x = time, y = avg_rent, group = 1)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "darkblue") +
  labs(title = "Overall Rent Trend in Los Angeles (2018 Q1 – 2024 Q4)",
       x = "Quarter",
       y = "Average Overall Rent (USD)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

############################ Availability proportion #########################
# Group by quarter and keep only one availability_proportion per quarter
la_availability_prop <- lease_data %>%
  filter(market == "Los Angeles") %>%
  mutate(time = paste(year, "Q", quarter, sep = "")) %>%
  group_by(time) %>%
  summarize(availability_proportion = first(availability_proportion)) %>%
  arrange(time)

# Convert to ordered factor to ensure correct x-axis order
la_availability_prop$time <- factor(la_availability_prop$time, levels = unique(la_availability_prop$time), ordered = TRUE)

# Plot
ggplot(la_availability_prop, aes(x = time, y = availability_proportion * 100, group = 1)) +
  geom_line(color = "darkred", size = 1.2) +
  geom_point(color = "firebrick", size = 2.5) +
  labs(
    title = "Availability Proportion in Los Angeles (2018–2024)",
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
# Filter for Los Angeles only
la_occupancy <- market_occupancy %>%
  filter(market == "Los Angeles") %>%
  mutate(time = paste(year, "Q", quarter, sep = "")) %>%
  arrange(year, quarter)

# Make sure 'time' is ordered for proper plotting
la_occupancy$time <- factor(la_occupancy$time, levels = unique(la_occupancy$time), ordered = TRUE)

# Plot the line graph
ggplot(la_occupancy, aes(x = time, y = avg_occupancy_proportion * 100, group = 1)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "navy", size = 2.5) +
  labs(
    title = "Average Occupancy Proportion in Los Angeles (2018–2024)",
    x = "Quarter",
    y = "Occupancy (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )





