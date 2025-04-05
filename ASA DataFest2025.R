library(tidycensus)
library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(openxlsx)
library(stringr)
library(tidytext)

#setwd("/Users/seonminjeong/Desktop/ASA DataFest 2025/2025 ASA DataFest-update2-2025-03-19/Data")

lease_data <- read.csv("Leases.csv") %>%
  rename(month = "monthsigned")
price_availability_data <- read.csv("Price and Availability Data.csv")
unemployment <- read.csv("Unemployment.csv")
market_occupancy <- read.csv("Major Market Occupancy Data-revised.csv")

summary_quality <- lease_data %>%
  filter(state == "TX") %>%
  group_by(year, internal_class) %>%
  summarize(count = n())

sum_industry <- lease_data %>%
  filter(is.na(internal_industry) == 0 & internal_industry != "Unclassifiable" & internal_industry != "TBD" & state == c("TX", "CA", "NY")) %>%
  group_by(state,year, internal_industry) %>%
  summarize(count = n()) %>%
  mutate(prp = count/sum(count)) %>%
  arrange(state)

merged_lease_unemployment <- merge(lease_data, unemployment, by = c("year", "month", "state"))

ls <- lm(internal_class_rent~unemployment_rate, data = merged_lease_unemployment)

distribution