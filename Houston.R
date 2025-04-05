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