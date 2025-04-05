library(tidycensus)
library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(openxlsx)
library(stringr)
library(tidytext)

lease_data <- read.csv("Leases.csv") %>%
  rename(month = "monthsigned")
price_availability_data <- read.csv("Price and Availability Data.csv")
unemployment <- read.csv("Unemployment.csv")
market_occupancy <- read.csv("Major Market Occupancy Data-revised.csv")

#Houston Overall Rent Change over Time
houston <- lease_data %>%
  filter(market == "Houston") %>%
  group_by(year, quarter) %>%
  summarize(avg_rent = mean(overall_rent)) %>%
  ungroup() %>%
  mutate(year_quarter = paste(year,quarter, sep = "-")) %>%
  mutate(t = 1:28)

ggplot(houston, aes(x = t, y = avg_rent)) + 
  geom_line(color = "blue", linewidth = 1) +
  scale_x_discrete(limits = c("2018-Q1", "2018-Q2", "2018-Q3", "2018-Q4",
                              "2019-Q1", "2019-Q2", "2019-Q3", "2019-Q4",
                              "2020-Q1", "2020-Q2", "2020-Q3", "2020-Q4",
                              "2021-Q1", "2021-Q2", "2021-Q3", "2021-Q4",
                              "2022-Q1", "2022-Q2", "2022-Q3", "2022-Q4",
                              "2023-Q1", "2023-Q2", "2023-Q3", "2023-Q4",
                              "2024-Q1", "2024-Q2", "2024-Q3", "2024-Q4")) +
  labs(x = "Year and Quarter", y = "average rent") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

TX_unemployment <- unemployment %>%
  filter(state == "TX") %>%
  group_by(year, month) %>%
  ungroup() %>%
  mutate(year_month = paste(year,month, sep = "-"))

#plot for unemployment rate in TX
ggplot(TX_unemployment, aes(x = year_month, y = unemployment_rate, group = 1)) +
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
  labs(title = "Unemployment Rate in TX Over Time",
       x = "Year-Month",
       y = "Unemployment") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1))