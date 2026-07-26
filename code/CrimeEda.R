# =============================================================
# PART 4: CRIME EDA
# =============================================================

library(tidyverse)

if (!("fmsb" %in% rownames(installed.packages()))) {
  install.packages("fmsb")
}
library(fmsb)

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

crime = read_csv(file.path(data_dir, "crime_clean.csv"), show_col_types = FALSE)
crime = crime %>% mutate(county = get_county(district_name))

population = read_csv(file.path(data_dir, "population_clean.csv"), show_col_types = FALSE)
population = population %>% mutate(county = get_county(district_name))

cat("Unique crime types:\n")
print(unique(crime$crime_type))

district_population = population %>%
  group_by(district_name, county) %>%
  summarise(district_pop = sum(population, na.rm = TRUE), .groups = "drop")

lsoa_population = population %>%
  select(lsoa_code, lsoa_population = population)

year_choice = 2024
month_choice = 6

drug_lsoa_rate = crime %>%
  filter(year == year_choice, crime_type == "Drugs") %>%
  count(lsoa_code, district_name, county, name = "drug_offences") %>%
  left_join(lsoa_population, by = "lsoa_code") %>%
  filter(!is.na(lsoa_population), lsoa_population > 0) %>%
  mutate(rate_per_10k = (drug_offences / lsoa_population) * 10000)

make_crime_boxplot = function(df, county_name) {
  ggplot(df %>% filter(county == county_name),
         aes(x = reorder(district_name, rate_per_10k, FUN = median),
             y = rate_per_10k, fill = district_name)) +
    geom_boxplot(show.legend = FALSE, outlier.alpha = 0.3) +
    coord_flip() +
    labs(title = paste("Drug Offence Rate by District,", county_name, year_choice),
         x = "District", y = "Drug Offences per 10,000 Population") +
    theme_minimal()
}

p1_norfolk = make_crime_boxplot(drug_lsoa_rate, "Norfolk")
p1_suffolk = make_crime_boxplot(drug_lsoa_rate, "Suffolk")

print(p1_norfolk)
print(p1_suffolk)

ggsave(file.path(graphs_dir, "crime_drug_boxplot_norfolk_2024.png"), p1_norfolk, width = 8, height = 6, dpi = 300)
ggsave(file.path(graphs_dir, "crime_drug_boxplot_suffolk_2024.png"), p1_suffolk, width = 8, height = 6, dpi = 300)

vehicle_district_rate = crime %>%
  filter(year == year_choice, month_num == month_choice, crime_type == "Vehicle crime") %>%
  count(district_name, county, name = "vehicle_offences") %>%
  right_join(district_population, by = c("district_name", "county")) %>%
  mutate(vehicle_offences = replace_na(vehicle_offences, 0),
         rate_per_100k = (vehicle_offences / district_pop) * 100000)

make_radar = function(df, county_name) {
  df_c = df %>% filter(county == county_name) %>% arrange(district_name)
  values = df_c$rate_per_100k
  radar_data = rbind(
    rep(max(values) * 1.1, length(values)),
    rep(0, length(values)),
    values
  )
  colnames(radar_data) = df_c$district_name
  radar_data = as.data.frame(radar_data)
  radarchart(radar_data,
             axistype = 1,
             pcol = "steelblue", pfcol = scales::alpha("steelblue", 0.4), plwd = 2,
             cglcol = "grey70", cglty = 1, axislabcol = "grey40",
             vlcex = 0.8,
             title = paste("Vehicle Crime Rate per 100k,", county_name,
                            month.name[month_choice], year_choice))
}

png(file.path(graphs_dir, "crime_vehicle_radar_norfolk.png"), width = 800, height = 800, res = 120)
make_radar(vehicle_district_rate, "Norfolk")
dev.off()

png(file.path(graphs_dir, "crime_vehicle_radar_suffolk.png"), width = 800, height = 800, res = 120)
make_radar(vehicle_district_rate, "Suffolk")
dev.off()

make_radar(vehicle_district_rate, "Norfolk")
make_radar(vehicle_district_rate, "Suffolk")

county_population = district_population %>%
  group_by(county) %>%
  summarise(county_pop = sum(district_pop, na.rm = TRUE), .groups = "drop")

drug_monthly_rate = crime %>%
  filter(crime_type == "Drugs", !is.na(county)) %>%
  count(month, year, month_num, county, name = "drug_offences") %>%
  left_join(county_population, by = "county") %>%
  mutate(rate_per_10k = (drug_offences / county_pop) * 10000,
         month_date = as.Date(paste0(month, "-01")))

p3 = ggplot(drug_monthly_rate, aes(x = month_date, y = rate_per_10k, colour = county)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.5) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  labs(title = "Monthly Drug Offence Rate per 10,000 Population (May 2023 - Dec 2025)",
       x = "Month", y = "Drug Offences per 10,000 Population", colour = "County") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p3)
ggsave(file.path(graphs_dir, "crime_drug_monthly_linechart.png"), p3, width = 10, height = 6, dpi = 300)