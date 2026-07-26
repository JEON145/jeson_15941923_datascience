# =============================================================
# PART 6: EXTRA/CUSTOM EDA (with checkpoints)
# =============================================================

library(tidyverse)

if (!("reshape2" %in% rownames(installed.packages()))) install.packages("reshape2")
if (!("ggrepel" %in% rownames(installed.packages()))) install.packages("ggrepel")
library(reshape2)
library(ggrepel)

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

cat("STEP 1: Loading datasets...\n")

house_price = read_csv(file.path(data_dir, "house_prices_clean.csv"), show_col_types = FALSE)
house_price = house_price %>% mutate(county = get_county(district), district_title = str_to_title(district))
cat("house_price loaded, rows:", nrow(house_price), "\n")

broadband = read_csv(file.path(data_dir, "broadband_clean.csv"), show_col_types = FALSE)
broadband = broadband %>% mutate(county = get_county(district_name))
cat("broadband loaded, rows:", nrow(broadband), "\n")

crime = read_csv(file.path(data_dir, "crime_clean.csv"), show_col_types = FALSE)
crime = crime %>% mutate(county = get_county(district_name))
cat("crime loaded, rows:", nrow(crime), "\n")

school = read_csv(file.path(data_dir, "schools_clean.csv"), show_col_types = FALSE)
school = school %>% mutate(county = get_county(district_name))
cat("school loaded, rows:", nrow(school), "\n")

population = read_csv(file.path(data_dir, "population_clean.csv"), show_col_types = FALSE)
population = population %>% mutate(county = get_county(district_name))
cat("population loaded, rows:", nrow(population), "\n")

cat("\nSTEP 2: Aggregating population by district...\n")
district_population = population %>%
  group_by(district_name, county) %>%
  summarise(district_pop = sum(population, na.rm = TRUE), .groups = "drop")
print(district_population)

cat("\nSTEP 3: House price summary (2024)...\n")
hp_summary = house_price %>%
  filter(year == 2024, !is.na(county)) %>%
  group_by(district_title, county) %>%
  summarise(avg_price = mean(price, na.rm = TRUE), .groups = "drop") %>%
  rename(district_name = district_title)
print(hp_summary)

cat("\nSTEP 4: Broadband summary...\n")
bb_summary = broadband %>%
  filter(!is.na(county)) %>%
  group_by(district_name, county) %>%
  summarise(avg_download = mean(avg_download, na.rm = TRUE), .groups = "drop")
print(bb_summary)

cat("\nSTEP 5: Crime summary (drug offences, 2024)...\n")
crime_summary = crime %>%
  filter(year == 2024, crime_type == "Drugs", !is.na(county)) %>%
  count(district_name, county, name = "drug_offences") %>%
  right_join(district_population, by = c("district_name", "county")) %>%
  mutate(drug_offences = replace_na(drug_offences, 0),
         drug_rate_per_10k = (drug_offences / district_pop) * 10000) %>%
  select(district_name, county, drug_rate_per_10k)
print(crime_summary)

cat("\nSTEP 6: School summary (Att8, 2023-2024)...\n")
school_summary = school %>%
  filter(academic_year == "2023-2024", !is.na(county)) %>%
  group_by(district_name, county) %>%
  summarise(avg_att8 = mean(att8_score, na.rm = TRUE), .groups = "drop")
print(school_summary)

cat("\nSTEP 7: Joining into district_master...\n")
district_master = hp_summary %>%
  inner_join(bb_summary, by = c("district_name", "county")) %>%
  inner_join(crime_summary, by = c("district_name", "county")) %>%
  inner_join(school_summary, by = c("district_name", "county"))

cat("district_master rows:", nrow(district_master), "\n")
print(district_master)

if (nrow(district_master) == 0) {
  stop("district_master has 0 rows - a join failed. Check district_name spelling matches across the tables printed above.")
}

cat("\nSTEP 8: Saving district_master.csv...\n")
write_csv(district_master, file.path(data_dir, "district_master.csv"))

cat("\nSTEP 9: Verifying file was written...\n")
cat("File exists:", file.exists(file.path(data_dir, "district_master.csv")), "\n")

cat("\n>>> If TRUE printed above, district_master.csv is saved. <<<\n")

# =============================================================
# PLOTS
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

p_bubble = ggplot(district_master, aes(x = avg_price, y = avg_download,
                                         size = avg_att8, colour = county)) +
  geom_point(alpha = 0.7) +
  geom_text_repel(aes(label = district_name), size = 3, show.legend = FALSE) +
  scale_x_continuous(labels = scales::comma) +
  scale_size_continuous(range = c(3, 12)) +
  labs(title = "District Comparison: House Price vs Broadband Speed vs Attainment 8",
       x = "Average House Price (£, 2024)", y = "Average Download Speed (Mbps, 2018)",
       size = "Att8 Score", colour = "County") +
  theme_minimal()

print(p_bubble)
ggsave(file.path(graphs_dir, "extra_bubble_chart.png"), p_bubble, width = 9, height = 7, dpi = 300)