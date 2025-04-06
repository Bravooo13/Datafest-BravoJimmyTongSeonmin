library(tidyverse)

# === Step 1: Load Leases Data ===
leases <- read_csv("Leases.csv")

# === Step 2: Create CBD Flag ===
leases <- leases %>%
  mutate(
    cbd_flag = ifelse(CBD_suburban == "CBD", 1, 0)
  )

# === Step 3: Summarize Per Market (2022–2024) ===
market_summary <- leases %>%
  filter(year >= 2022, !is.na(overall_rent), overall_rent > 0,
         !is.na(availability_proportion)) %>%
  group_by(market) %>%
  summarise(
    avg_rent = mean(overall_rent, na.rm = TRUE),
    avg_availability = mean(availability_proportion, na.rm = TRUE),
    cbd_ratio = mean(cbd_flag, na.rm = TRUE)
  ) %>%
  drop_na()

# === Step 4: Normalize All Features ===
scaled_data <- market_summary %>%
  column_to_rownames("market") %>%
  scale() %>%
  as.data.frame()

# === Step 5: Similarity Function ===
find_similar_cities <- function(input_city, top_n = 5) {
  if (!(input_city %in% rownames(scaled_data))) {
    stop("City not found in data.")
  }
  
  input_vector <- scaled_data[input_city, , drop = FALSE]
  distances <- apply(scaled_data, 1, function(row) sqrt(sum((row - input_vector)^2)))
  similar_cities <- sort(distances)[2:(top_n + 1)]  # Skip input city itself
  return(data.frame(city = names(similar_cities), similarity_score = similar_cities))
}

# === Step 6: Example Use ===
find_similar_cities("Houston", top_n = 3)
find_similar_cities("Manhattan", top_n = 3)
find_similar_cities("Los Angeles", top_n = 3)

