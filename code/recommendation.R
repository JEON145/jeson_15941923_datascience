# =============================================================
# PART 8: RECOMMENDATION SYSTEM
# =============================================================

library(tidyverse)

data_dir = "C:/Users/Acer/OneDrive/Desktop/datasci/cleaned_data"
graphs_dir = file.path(data_dir, "..", "graphs")
dir.create(graphs_dir, showWarnings = FALSE)

district_master = read_csv(file.path(data_dir, "district_master.csv"), show_col_types = FALSE)

cat("Input data:\n")
print(district_master)

# =============================================================
# 1. NORMALISE each variable to a 0-10 scale
# =============================================================
# For "higher is better" variables (price we invert - lower price = better for affordability;
# broadband and att8 - higher is better), we min-max scale to 0-10.
# For drug_rate_per_10k, LOWER is better (safer), so we invert the scale.

normalise_0_10 = function(x, higher_is_better = TRUE) {
  rng = range(x, na.rm = TRUE)
  if (rng[1] == rng[2]) return(rep(5, length(x)))  # avoid div by zero if all equal
  scaled = (x - rng[1]) / (rng[2] - rng[1]) * 10
  if (!higher_is_better) scaled = 10 - scaled
  return(scaled)
}

district_scores = district_master %>%
  mutate(
    # Affordability: LOWER price = better (their stated top priority)
    score_affordability = normalise_0_10(avg_price, higher_is_better = FALSE),
    # Broadband: HIGHER speed = better
    score_broadband      = normalise_0_10(avg_download, higher_is_better = TRUE),
    # Safety: LOWER drug offence rate = better
    score_safety          = normalise_0_10(drug_rate_per_10k, higher_is_better = FALSE),
    # Education (justification below): HIGHER Att8 = better
    score_education        = normalise_0_10(avg_att8, higher_is_better = TRUE)
  )

# =============================================================
# 2. WEIGHTED COMBINED SCORE
# =============================================================
# Weights reflect stated priorities in the brief:
# - Affordability = TOP priority        -> 0.40
# - Safety (crime) = "very important"   -> 0.25
# - Broadband = "essential"             -> 0.20
# - Education (proxy for amenities/community/family suitability) -> 0.15

weights = c(affordability = 0.40, broadband = 0.20, safety = 0.25, education = 0.15)

district_scores = district_scores %>%
  mutate(
    total_score = score_affordability * weights["affordability"] +
      score_broadband     * weights["broadband"] +
      score_safety        * weights["safety"] +
      score_education     * weights["education"]
  ) %>%
  arrange(desc(total_score))

cat("\nFull ranked district table:\n")
print(district_scores %>%
        select(district_name, county, score_affordability, score_broadband,
               score_safety, score_education, total_score))

# =============================================================
# 3. TOP 3 DISTRICTS
# =============================================================
top3 = district_scores %>% slice_head(n = 3)

cat("\n\n================================================\n")
cat("TOP 3 RECOMMENDED DISTRICTS\n")
cat("================================================\n")
print(top3 %>% select(district_name, county, total_score))

write_csv(district_scores, file.path(data_dir, "district_recommendation_scores.csv"))
cat("\nSaved district_recommendation_scores.csv\n")

# =============================================================
# 4. VISUALISE: horizontal bar chart of total scores, top 3 highlighted
# =============================================================
district_scores = district_scores %>%
  mutate(rank = row_number(),
         highlight = ifelse(rank <= 3, "Top 3", "Other"))

p_rank = ggplot(district_scores, aes(x = reorder(district_name, total_score),
                                      y = total_score, fill = highlight)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Top 3" = "steelblue", "Other" = "grey70")) +
  labs(title = "District Recommendation Scores (0-10 scale)",
       subtitle = "Weighted: Affordability 40%, Safety 25%, Broadband 20%, Education 15%",
       x = "District", y = "Total Weighted Score", fill = "") +
  theme_minimal()

print(p_rank)
ggsave(file.path(graphs_dir, "recommendation_ranking.png"), p_rank, width = 8, height = 6, dpi = 300)

# =============================================================
# 5. TABLE COMPARISON of top 3 districts across 4 criteria
# =============================================================
top3_table = top3 %>%
  transmute(
    District = district_name,
    County = county,
    Affordability = round(score_affordability, 2),
    Broadband = round(score_broadband, 2),
    Safety = round(score_safety, 2),
    Education = round(score_education, 2),
    Total_Score = round(total_score, 2)
  )

write_csv(top3_table, file.path(data_dir, "recommendation_top3_districts.csv"))
cat("\nSaved recommendation_top3_districts.csv\n")
print(top3_table)

table_plot = ggplot(
  top3_table %>%
    pivot_longer(cols = c(Affordability, Broadband, Safety, Education),
                 names_to = "Criterion", values_to = "Score"),
  aes(x = Criterion, y = District, fill = Score)
) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.1f", Score)), color = "black", size = 3.5) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Top 3 Recommended Districts: Score Breakdown",
       subtitle = "Rounded scores on a 0-10 scale",
       x = "", y = "", fill = "Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(table_plot)
ggsave(file.path(graphs_dir, "recommendation_top3_table.png"), table_plot, width = 8, height = 4, dpi = 300)