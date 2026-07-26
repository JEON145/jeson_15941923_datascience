# =============================================================
# PART 6: EXTRA/CUSTOM EDA
# =============================================================

library(tidyverse)

if (!("reshape2" %in% rownames(installed.packages()))) install.packages("reshape2")
library(reshape2)

data_dir = "C:/Users/Acer/OneDrive/Desktop/datasci/cleaned_data"
graphs_dir = file.path(data_dir, "..", "graphs")
dir.create(graphs_dir, showWarnings = FALSE)

norfolk_districts = c("Breckland", "Broadland", "Great Yarmouth",
                        "King's Lynn and West Norfolk", "North Norfolk",
                        "Norwich", "South Norfolk")
suffolk_districts = c("Babergh", "East Suffolk", "Ipswich",
                        "Mid Suffolk", "West Suffolk")

get_county = function(district_name) {
  d = str_to_title(district_name)
  case_when(
    d %in% norfolk_districts ~ "Norfolk",
    d %in% suffolk_districts ~ "Suffolk",
    TRUE ~ NA_character_
  )
}

# ---- Load all 4 core datasets + population ----
house_price = read_csv(file.path(data_dir, "house_prices_clean.csv"), show_col_types = FALSE) %>%
  mutate(county = get_county(district), district_title = str_to_title(district))

broadband = read_csv(file.path(data_dir, "broadband_clean.csv"), show_col_types = FALSE) %>%
  mutate(county = get_county(district_name))

crime = read_csv(file.path(data_dir, "crime_clean.csv"), show_col_types = FALSE) %>%
  mutate(county = get_county(district_name))

school = read_csv(file.path(data_dir, "schools_clean.csv"), show_col_types = FALSE) %>%
  mutate(county = get_county(district_name))

population = read_csv(file.path(data_dir, "population_clean.csv"), show_col_types = FALSE) %>%
  mutate(county = get_county(district_name))

district_population = population %>%
  group_by(district_name, county) %>%
  summarise(district_pop = sum(population, na.rm = TRUE), .groups = "drop")

# ---- 1. House price summary (2024) ----
hp_summary = house_price %>%
  filter(year == 2024, !is.na(county)) %>%
  group_by(district_title, county) %>%
  summarise(avg_price = mean(price, na.rm = TRUE), .groups = "drop") %>%
  rename(district_name = district_title)

# ---- 2. Broadband summary (2018, only year available) ----
bb_summary = broadband %>%
  filter(!is.na(county)) %>%
  group_by(district_name, county) %>%
  summarise(avg_download = mean(avg_download, na.rm = TRUE), .groups = "drop")

# ---- 3. Crime summary: drug offence rate per 10,000, 2024 ----
crime_summary = crime %>%
  filter(year == 2024, crime_type == "Drugs", !is.na(county)) %>%
  count(district_name, county, name = "drug_offences") %>%
  right_join(district_population, by = c("district_name", "county")) %>%
  mutate(drug_offences = replace_na(drug_offences, 0),
         drug_rate_per_10k = (drug_offences / district_pop) * 10000) %>%
  select(district_name, county, drug_rate_per_10k)

# ---- 4. School summary: Att8, 2023-2024 ----
school_summary = school %>%
  filter(academic_year == "2023-2024", !is.na(county)) %>%
  group_by(district_name, county) %>%
  summarise(avg_att8 = mean(att8_score, na.rm = TRUE), .groups = "drop")

# ---- Combine into master district-level table ----
district_master = hp_summary %>%
  inner_join(bb_summary, by = c("district_name", "county")) %>%
  inner_join(crime_summary, by = c("district_name", "county")) %>%
  inner_join(school_summary, by = c("district_name", "county"))

cat("Master district table (n =", nrow(district_master), "districts):\n")
print(district_master)

# =============================================================
# EXTRA EDA 1: CORRELATION HEATMAP
# =============================================================
cor_data = district_master %>%
  select(avg_price, avg_download, drug_rate_per_10k, avg_att8)

cor_matrix = cor(cor_data, use = "complete.obs")
cor_melt = melt(cor_matrix)

p_heatmap = ggplot(cor_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = round(value, 2)), colour = "black", size = 4) +
  scale_fill_gradient2(low = "firebrick", mid = "white", high = "steelblue",
                       midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Correlation Heatmap: House Price, Broadband, Crime, Attainment 8",
       x = "", y = "", fill = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_heatmap)
ggsave(file.path(graphs_dir, "extra_correlation_heatmap.png"), p_heatmap, width = 7, height = 6, dpi = 300)

# =============================================================
# EXTRA EDA 2: BUBBLE CHART
# House price (x) vs Broadband speed (y), size = Att8, colour = county
# =============================================================
p_bubble = ggplot(district_master, aes(x = avg_price, y = avg_download,
                                         size = avg_att8, colour = county)) +
  geom_point(alpha = 0.7) +
  ggrepel::geom_text_repel(aes(label = district_name), size = 3, show.legend = FALSE) +
  scale_x_continuous(labels = scales::comma) +
  scale_size_continuous(range = c(3, 12)) +
  labs(title = "District Comparison: House Price vs Broadband Speed vs Attainment 8",
       x = "Average House Price (£, 2024)", y = "Average Download Speed (Mbps, 2018)",
       size = "Att8 Score", colour = "County") +
  theme_minimal()

print(p_bubble)
ggsave(file.path(graphs_dir, "extra_bubble_chart.png"), p_bubble, width = 9, height = 7, dpi = 300)

# ---- Save district_master for reuse in linear models + recommendation system ----
write_csv(district_master, file.path(data_dir, "district_master.csv"))
cat("\nSaved district_master.csv for use in Part 7 (linear models) and Part 8 (recommender).\n")