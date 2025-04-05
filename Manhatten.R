library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)

lease_data_Manhattan <- lease_data %>%
  filter(market == "Manhattan")

price_availability_data_Manhattan <- price_availability_data %>%
  filter(market == "Manhattan")

# Ensure quarter column is ordered properly
price_availability_data_Manhattan_A <- price_availability_data %>%
  filter(market == "Manhattan" & internal_class == "A") %>%
  arrange(year, quarter)  # Ensure data is sorted

# Create a new variable combining year and quarter
price_availability_data_Manhattan_A <- price_availability_data_Manhattan_A %>%
  mutate(year_quarter = paste(year, quarter))

# Turn it into an ordered factor so ggplot respects order
price_availability_data_Manhattan_A$year_quarter <- factor(
  price_availability_data_Manhattan_A$year_quarter,
  levels = unique(price_availability_data_Manhattan_A$year_quarter)
)

# Plot
ggplot(price_availability_data_Manhattan_A, aes(x = year_quarter, y = overall_rent, group = 1)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  labs(title = "Overall Rent Over Time (by Quarter)",
       x = "Year-Quarter",
       y = "Overall Rent") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x labels