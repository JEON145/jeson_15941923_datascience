# =============================================================
# PART 5: SCHOOL EDA
# =============================================================

library(tidyverse)

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

school = read_csv(file.path(data_dir, "schools_clean.csv"), show_col_types = FALSE)
school = school %>% mutate(county_check = get_county(district_name))

cat("Rows where county column mismatches derived county:",
    sum(school$county != school$county_check, na.rm = TRUE), "\n")
cat("Academic years available:\n")
print(unique(school$academic_year))

year_choice = "2023-2024"

# =============================================================
# 1. VIOLIN PLOT: Att8 scores by district, chosen academic year (2 graphs)
# =============================================================
school_year = school %>% filter(academic_year == year_choice)

make_violin = function(df, county_name) {
  ggplot(df %>% filter(county == county_name),
         aes(x = reorder(district_name, att8_score, FUN = median),
             y = att8_score, fill = district_name)) +
    geom_violin(trim = FALSE, show.legend = FALSE, alpha = 0.7) +
    geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
    coord_flip() +
    labs(title = paste("Attainment 8 Score Distribution by District,", county_name, year_choice),
         x = "District", y = "Attainment 8 Score") +
    theme_minimal()
}

p1_norfolk = make_violin(school_year, "Norfolk")
p1_suffolk = make_violin(school_year, "Suffolk")

print(p1_norfolk)
print(p1_suffolk)

ggsave(file.path(graphs_dir, "school_att8_violin_norfolk.png"), p1_norfolk, width = 8, height = 6, dpi = 300)
ggsave(file.path(graphs_dir, "school_att8_violin_suffolk.png"), p1_suffolk, width = 8, height = 6, dpi = 300)

# =============================================================
# 2. LINE CHARTS: Att8 score 2021-2025, 2 graphs (Norfolk, Suffolk)
# =============================================================
school_trend = school %>%
  filter(!is.na(county)) %>%
  group_by(academic_year, county, district_name) %>%
  summarise(avg_att8 = mean(att8_score, na.rm = TRUE), .groups = "drop")

make_trend_line = function(df, county_name) {
  ggplot(df %>% filter(county == county_name),
         aes(x = academic_year, y = avg_att8, colour = district_name, group = district_name)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    labs(title = paste("Attainment 8 Score Trend by District,", county_name),
         x = "Academic Year", y = "Average Attainment 8 Score", colour = "District") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

p2_norfolk = make_trend_line(school_trend, "Norfolk")
p2_suffolk = make_trend_line(school_trend, "Suffolk")

print(p2_norfolk)
print(p2_suffolk)

ggsave(file.path(graphs_dir, "school_att8_trend_norfolk.png"), p2_norfolk, width = 9, height = 6, dpi = 300)
ggsave(file.path(graphs_dir, "school_att8_trend_suffolk.png"), p2_suffolk, width = 9, height = 6, dpi = 300)