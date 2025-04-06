library(tidycensus)
library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(openxlsx)
library(stringr)
library(tidytext)

#adding interaction term 
DiD_data <- merged_lease_unemployment %>%
  select(year, month, quarter.x, market, availability_proportion, overall_rent, unemployment_rate) %>%
  filter(market == c("Manhattan", "Los Angeles"))

# Calculate the mean of unemployment_rate
mean_unemp <- mean(DiD_data$unemployment_rate, na.rm = TRUE)

# Center the variable
DiD_data$unemployment_centered <- DiD_data$unemployment_rate - mean_unemp

# Center only (scale = FALSE)
DiD_data$unemployment_centered <- scale(DiD_data$unemployment_rate, center = TRUE, scale = FALSE)

mean(DiD_data$unemployment_centered, na.rm = TRUE)  # Should return ~0

DiD_data$post <- ifelse(as.numeric(DiD_data$year) > 2019, 1, 0)
DiD_data$manh <- ifelse(DiD_data$market == "Manhattan", 1, 0)
DiD_data$interaction <- DiD_data$post*DiD_data$manh
DiD <- lm(formula = overall_rent ~ manh + post + interaction, data = DiD_data)

tDiD <- lm(formula = overall_rent~ manh + post + +unemployment_rate + interaction + post*unemployment_rate + 
             manh*unemployment_rate + interaction*unemployment_rate, data = DiD_data)

tDiD_centered <- lm(
  overall_rent ~ manh + post + unemployment_centered +
    manh:post + post:unemployment_centered + manh:unemployment_centered +
    manh:post:unemployment_centered,
  data = DiD_data
)
library(interactions)
interact_plot(tDiD, pred = "unemployment_rate", modx = "manh", mod2 = "post")