# =============================================================
# PART 3: BROADBAND EDA
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

# ---- Load broadband data (2018 snapshot, single year) ----
broadband = read_csv(file.path(data_dir, "broadband_clean.csv"), show_col_types = FALSE) %>%
  mutate(county = get_county(district_name))

cat("Rows with unmatched county:", sum(is.na(broadband$county)), "\n")

# =============================================================
# 1. BOXPLOTS: avg download speed by district (2 separate graphs: Norfolk, Suffolk)
# =============================================================
make_boxplot = function(df, county_name) {
  ggplot(df %>% filter(county == county_name),
         aes(x = reorder(district_name, avg_download, FUN = median),
             y = avg_download, fill = district_name)) +
    geom_boxplot(show.legend = FALSE, outlier.alpha = 0.3) +
    coord_flip() +
    labs(title = paste("Average Download Speed by District,", county_name, "(2018)"),
         x = "District", y = "Average Download Speed (Mbps)") +
    theme_minimal()
}

p1_norfolk = make_boxplot(broadband, "Norfolk")
p1_suffolk = make_boxplot(broadband, "Suffolk")

print(p1_norfolk)
print(p1_suffolk)

ggsave(file.path(graphs_dir, "broadband_boxplot_norfolk_2018.png"), p1_norfolk, width = 8, height = 6, dpi = 300)
ggsave(file.path(graphs_dir, "broadband_boxplot_suffolk_2018.png"), p1_suffolk, width = 8, height = 6, dpi = 300)

# =============================================================
# 2. STACKED BAR CHARTS: avg vs max download speed by district (2 graphs)
# =============================================================
broadband_summary = broadband %>%
  group_by(district_name, county) %>%
  summarise(avg_download = mean(avg_download, na.rm = TRUE),
            max_download = mean(max_download, na.rm = TRUE),
            .groups = "drop") %>%
  pivot_longer(cols = c(avg_download, max_download),
               names_to = "speed_type", values_to = "speed") %>%
  mutate(speed_type = recode(speed_type,
                             avg_download = "Average Download",
                             max_download = "Max Download"))

make_stacked_bar = function(df, county_name) {
  ggplot(df %>% filter(county == county_name),
         aes(x = reorder(district_name, speed), y = speed, fill = speed_type)) +
    geom_col(position = "stack") +
    coord_flip() +
    labs(title = paste("Average vs Max Download Speed by District,", county_name, "(2018)"),
         x = "District", y = "Download Speed (Mbps)", fill = "Speed Type") +
    theme_minimal()
}

p2_norfolk = make_stacked_bar(broadband_summary, "Norfolk")
p2_suffolk = make_stacked_bar(broadband_summary, "Suffolk")

print(p2_norfolk)
print(p2_suffolk)

ggsave(file.path(graphs_dir, "broadband_stackedbar_norfolk_2018.png"), p2_norfolk, width = 8, height = 6, dpi = 300)
ggsave(file.path(graphs_dir, "broadband_stackedbar_suffolk_2018.png"), p2_suffolk, width = 8, height = 6, dpi = 300)