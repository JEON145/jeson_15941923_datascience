# =============================================================
# PART 2: HOUSE PRICE EDA
# =============================================================

library(tidyverse)

data_dir <- "C:/Users/Acer/OneDrive/Desktop/datasci/cleaned_data"

# ---- District -> County helper ----
norfolk_districts <- c("Breckland", "Broadland", "Great Yarmouth",
                        "King's Lynn and West Norfolk", "North Norfolk",
                        "Norwich", "South Norfolk")
suffolk_districts <- c("Babergh", "East Suffolk", "Ipswich",
                        "Mid Suffolk", "West Suffolk")

get_county <- function(district_name) {
  d <- str_to_title(district_name)
  case_when(
    d %in% norfolk_districts ~ "Norfolk",
    d %in% suffolk_districts ~ "Suffolk",
    TRUE ~ NA_character_
  )
}

# ---- Load house price data ----
house_price <- read_csv(file.path(data_dir, "house_prices_clean.csv"), show_col_types = FALSE) %>%
  mutate(county = get_county(district),
         district_title = str_to_title(district))

cat("Rows with unmatched county:", sum(is.na(house_price$county)), "\n")

# =============================================================
# 1. BOXPLOT: avg house price by district, chosen year (2024)
# =============================================================
year_choice <- 2024

hp_year <- house_price %>% filter(year == year_choice)

p1 <- ggplot(hp_year, aes(x = reorder(district_title, price, FUN = median),
                          y = price, fill = county)) +
  geom_boxplot(outlier.alpha = 0.3) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = paste("House Price Distribution by District,", year_choice),
       x = "District", y = "Price (£)", fill = "County") +
  theme_minimal()

print(p1)
ggsave(file.path(data_dir, "..", "graphs", "house_price_boxplot_2024.png"),
       p1, width = 9, height = 6, dpi = 300)

# =============================================================
# 2. BAR CHART: avg house price by district, same year
# =============================================================
hp_summary <- hp_year %>%
  group_by(district_title, county) %>%
  summarise(avg_price = mean(price, na.rm = TRUE), .groups = "drop")

p2 <- ggplot(hp_summary, aes(x = reorder(district_title, avg_price),
                             y = avg_price, fill = county)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = paste("Average House Price by District,", year_choice),
       x = "District", y = "Average Price (£)", fill = "County") +
  theme_minimal()

print(p2)
ggsave(file.path(data_dir, "..", "graphs", "house_price_barchart_2024.png"),
       p2, width = 9, height = 6, dpi = 300)

# =============================================================
# 3. LINE CHART: avg house price 2021-2025, both counties, one chart
# =============================================================
hp_trend <- house_price %>%
  filter(year >= 2021, year <= 2025, !is.na(county)) %>%
  group_by(year, county) %>%
  summarise(avg_price = mean(price, na.rm = TRUE), .groups = "drop")

p3 <- ggplot(hp_trend, aes(x = year, y = avg_price, colour = county)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = 2021:2025) +
  labs(title = "Average House Price Trend, 2021–2025",
       x = "Year", y = "Average Price (£)", colour = "County") +
  theme_minimal()

print(p3)
ggsave(file.path(data_dir, "..", "graphs", "house_price_linechart_2021_2025.png"),
       p3, width = 9, height = 6, dpi = 300)