# =============================================================
# CORRELATION HEATMAP (part of Part 6: Extra/Custom EDA)
# =============================================================

library(tidyverse)

if (!("reshape2" %in% rownames(installed.packages()))) install.packages("reshape2")
library(reshape2)

data_dir = "C:/Users/Acer/OneDrive/Desktop/datasci/cleaned_data"
graphs_dir = file.path(data_dir, "..", "graphs")
dir.create(graphs_dir, showWarnings = FALSE)

district_master = read_csv(file.path(data_dir, "district_master.csv"), show_col_types = FALSE)

# ---- Correlation matrix across all 4 core variables ----
cor_data = district_master %>%
  select(avg_price, avg_download, drug_rate_per_10k, avg_att8)

cor_matrix = cor(cor_data, use = "complete.obs")

cat("Correlation Matrix:\n")
print(round(cor_matrix, 3))

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