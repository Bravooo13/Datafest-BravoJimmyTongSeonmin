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
