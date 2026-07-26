library(ggplot2)
library(dplyr)

# Build the scores data frame
# Replace these with your exact model_comparison / recommendation table values
# before submission - these are read approximately off Figure 32's axis
df_scores = data.frame(
  district = c("Ipswich", "Great Yarmouth", "Broadland", "South Norfolk",
               "Norwich", "Mid Suffolk", "East Suffolk", "Breckland",
               "North Norfolk", "Babergh", "West Suffolk"),
  total_score = c(6.3, 6.2, 5.6, 5.0, 4.8, 3.8, 3.7, 3.6, 3.3, 3.2, 2.9)
)

# Order and flag top 3
df_scores = df_scores %>%
  arrange(desc(total_score)) %>%
  mutate(
    district = factor(district, levels = district),
    highlight = ifelse(district %in% c("Ipswich", "Great Yarmouth", "Broadland"),
                       "Top 3", "Other")
  )

# Figure 33: Overall Recommendation Score by District (lollipop chart)
ggplot(df_scores, aes(x = total_score, y = district, color = highlight)) +
  geom_segment(aes(x = 0, xend = total_score, y = district, yend = district),
               linewidth = 1) +
  geom_point(size = 4) +
  scale_color_manual(values = c("Top 3" = "#2C77BF", "Other" = "grey60")) +
  labs(
    title = "Overall Recommendation Score by District",
    subtitle = "Weighted: Affordability 40%, Safety 25%, Broadband 20%, Education 15%",
    x = "Total Weighted Score", y = "District", color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

ggsave("fig33_overall_score.png", width = 8, height = 6, dpi = 300)