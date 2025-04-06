library(dplyr)
library(ggplot2)

# Combine rent data for all 3 cities
combined_rent_data <- lease_data %>%
  filter(market %in% c("Manhattan", "Los Angeles", "Houston")) %>%
  mutate(time = paste(year, ".", quarter, sep = "")) %>%
  group_by(market, time) %>%
  summarize(avg_rent = mean(overall_rent, na.rm = TRUE)) %>%
  arrange(market, time)

# Convert 'time' to ordered factor
combined_rent_data$time <- factor(combined_rent_data$time, levels = unique(combined_rent_data$time), ordered = TRUE)


############################# Rent #########################################
combined_rent_plot <- ggplot(combined_rent_data, aes(x = time, y = avg_rent, color = market, group = market)) +
  geom_line(size = 1.2) +
  geom_point() +
  geom_vline(xintercept = which(levels(combined_rent_data$time) == "2020.Q1"), 
             linetype = "dashed", color = "gray40", size = 1) +
  annotate("text", x = which(levels(combined_rent_data$time) == "2020.Q1"), y = Inf, 
           label = "COVID Start", vjust = -0.5, angle = 90, size = 3, color = "gray40") +
  labs(
    title = "Overall Rent Trends (2018–2024)",
    subtitle = "Comparison across Manhattan, Los Angeles, and Houston",
    x = "Quarter",
    y = "Average Rent (USD per sq ft/year)",
    color = "Market"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


############################## Availability ################################
combined_avail_data <- lease_data %>%
  filter(market %in% c("Manhattan", "Los Angeles", "Houston")) %>%
  mutate(time = paste(year, ".", quarter, sep = "")) %>%
  group_by(market, time) %>%
  summarize(availability_proportion = first(availability_proportion)) %>%
  arrange(market, time)

combined_avail_data$time <- factor(combined_avail_data$time, levels = unique(combined_avail_data$time), ordered = TRUE)

combined_avail_plot <- ggplot(combined_avail_data, aes(x = time, y = availability_proportion * 100, color = market, group = market)) +
  geom_line(size = 1.2) +
  geom_point() +
  geom_vline(xintercept = which(levels(combined_avail_data$time) == "2020.Q1"),
             linetype = "dashed", color = "gray40", size = 1) +
  annotate("text", x = which(levels(combined_avail_data$time) == "2020.Q1"), y = Inf,
           label = "COVID Start", vjust = -0.5, angle = 90, size = 3, color = "gray40") +
  labs(
    title = "Availability Proportion Trends (2018–2024)",
    subtitle = "Comparison across Manhattan, Los Angeles, and Houston",
    x = "Quarter",
    y = "Availability (%)",
    color = "Market"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


############################## Occupancy ####################################
combined_occupancy_data <- market_occupancy %>%
  filter(market %in% c("Manhattan", "Los Angeles", "Houston")) %>%
  mutate(time = paste(year, ".", quarter, sep = "")) %>%
  arrange(market, year, quarter)

combined_occupancy_data$time <- factor(combined_occupancy_data$time, levels = unique(combined_occupancy_data$time), ordered = TRUE)

combined_occupancy_plot <- ggplot(combined_occupancy_data, aes(x = time, y = avg_occupancy_proportion * 100, color = market, group = market)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(
    title = "Occupancy Proportion Trends (2018–2024)",
    subtitle = "Comparison across Manhattan, Los Angeles, and Houston",
    x = "Quarter",
    y = "Occupancy (%)",
    color = "Market"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

############################# Save Plots ###################################
ggsave("Plots/combined_rent_trend.png", plot = combined_rent_plot, width = 8, height = 5, dpi = 300)
ggsave("Plots/combined_availability_trend.png", plot = combined_avail_plot, width = 8, height = 5, dpi = 300)
ggsave("Plots/combined_occupancy_trend.png", plot = combined_occupancy_plot, width = 8, height = 5, dpi = 300)






