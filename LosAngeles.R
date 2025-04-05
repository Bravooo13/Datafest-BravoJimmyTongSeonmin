library(dplyr)
library(ggplot2)

la_data <- lease_data %>%
  filter(market == "Los Angeles")

# Optional: Create a time variable for better x-axis labeling
la_data <- la_data %>%
  mutate(time = paste(year, "Q", quarter, sep = ""))

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

# Calculate summary statistics
mean_rent <- mean(la_data$overall_rent, na.rm = TRUE)
median_rent <- median(la_data$overall_rent, na.rm = TRUE)
range_rent <- range(la_data$overall_rent, na.rm = TRUE)
rent_range_value <- diff(range_rent)  # This gives you max - min

# Print results
cat("Mean Overall Rent in LA:", round(mean_rent, 2), "\n")
cat("Median Overall Rent in LA:", round(median_rent, 2), "\n")
cat("Rent Range in LA:", round(range_rent[1], 2), "to", round(range_rent[2], 2), "\n")
cat("Total Range (Max - Min):", round(rent_range_value, 2), "\n")