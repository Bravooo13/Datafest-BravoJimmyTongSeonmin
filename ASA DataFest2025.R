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

#ummary_lease <- lease_data %>%
#  group_by(internal_class) %>%
#  summarize(count = n())

merged_lease_unemployment <- merge(lease_data, unemployment, by = c("year", "month", "state"))

ls <- lm(leasing~unemployment_rate, data = merged_lease_unemployment)