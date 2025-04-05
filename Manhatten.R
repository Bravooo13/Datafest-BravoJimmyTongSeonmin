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


manhatten_unemployment <- unemployment %>%
  filter(state == "NY") %>%
  group_by(year, month) %>%
  ungroup() %>%
  mutate(year_month = paste(year,month, sep = "-")) %>%
  mutate(t = 1:84)

#plot for unemployment rate in NY
ggplot(manhatten_unemployment, aes(x = year_month, y = unemployment_rate, group = 1)) +
  geom_line(color = "blue", linewidth = 1) +
  scale_x_discrete(limits = c("2018-1", "2018-2", "2018-3", "2018-4","2018-5", "2018-6", "2018-7", "2018-8", "2018-9", "2018-10",
                              "2018-11", "2018-12", 
                              "2019-1", "2019-2", "2019-3", "2019-4","2019-5", "2019-6", "2019-7",
                              "2019-8", "2019-9", "2019-10", "2019-11", "2019-12",
                              "2020-1", "2020-2", "2020-3", "2020-4", "2020-5", "2020-6", "2020-7", "2020-8", "2020-9",
                              "2020-10", "2020-11", "2020-12", 
                              "2021-1", "2021-2", "2021-3", "2021-4","2021-5", "2021-6", "2021-7", "2021-8", "2021-9",
                              "2021-10", "2021-11", "2021-12",
                              "2022-1", "2022-2", "2022-3", "2022-4","2022-5", "2022-6", "2022-7", "2022-8","2022-9",
                              "2022-10", "2022-11", "2022-12",
                              "2023-1", "2023-2", "2023-3", "2023-4", "2023-5","2023-6","2023-7","2023-8","2023-9","2023-10","2023-11","2023-12",
                              "2024-1", "2024-2", "2024-3", "2024-4","2024-5","2024-6","2024-7","2024-8","2024-9","2024-10","2024-11","2024-12")) +
  labs(title = "Unemployment Rate in NY Over Time",
       x = "Year-Month",
       y = "Unemployment") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1))