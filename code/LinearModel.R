# =============================================================
# PART 7: LINEAR MODELS
# Norfolk & Suffolk Property Investment Analysis
# =============================================================

library(tidyverse)
library(ggrepel)

data_dir = "C:/Users/Acer/OneDrive/Desktop/datasci/cleaned_data"
graphs_dir = file.path(data_dir, "..", "graphs")
dir.create(graphs_dir, showWarnings = FALSE)

district_master = read_csv(file.path(data_dir, "district_master.csv"), show_col_types = FALSE)

cat("District master table:\n")
print(district_master)

# =============================================================
# Helper function: fit model, plot scatter+line, plot diagnostics, print summary
# =============================================================
run_lm = function(data, x_var, y_var, x_label, y_label, model_name) {
  
  formula_str = paste(y_var, "~", x_var)
  model = lm(as.formula(formula_str), data = data)
  
  cat("\n\n================================================\n")
  cat("MODEL:", model_name, "\n")
  cat("================================================\n")
  print(summary(model))
  
  p_scatter = ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_point(size = 3, colour = "steelblue") +
    geom_smooth(method = "lm", se = TRUE, colour = "firebrick") +
    geom_text_repel(aes(label = district_name), size = 3) +
    labs(title = paste("Scatterplot:", model_name),
         subtitle = paste0("R2 = ", round(summary(model)$r.squared, 3),
                           ", p = ", round(summary(model)$coefficients[2,4], 4)),
         x = x_label, y = y_label) +
    theme_minimal()
  
  print(p_scatter)
  fname_scatter = paste0("lm_", gsub("[^a-zA-Z0-9]", "_", model_name), "_scatter.png")
  ggsave(file.path(graphs_dir, fname_scatter), p_scatter, width = 8, height = 6, dpi = 300)
  
  fname_diag = paste0("lm_", gsub("[^a-zA-Z0-9]", "_", model_name), "_diagnostics.png")
  png(file.path(graphs_dir, fname_diag), width = 900, height = 900, res = 120)
  par(mfrow = c(2, 2))
  plot(model)
  dev.off()
  
  par(mfrow = c(2, 2))
  plot(model)
  par(mfrow = c(1, 1))
  
  return(model)
}

# 1. House price vs Broadband download speed
m1 = run_lm(district_master, "avg_download", "avg_price",
             "Average Download Speed (Mbps, 2018)", "Average House Price (GBP, 2024)",
             "House Price vs Broadband Speed")

# 2. House price vs Drug offence rate
m2 = run_lm(district_master, "drug_rate_per_10k", "avg_price",
             "Drug Offence Rate per 10,000 (2024)", "Average House Price (GBP, 2024)",
             "House Price vs Drug Offence Rate")

# 3. House price vs Attainment 8 score
m3 = run_lm(district_master, "avg_att8", "avg_price",
             "Average Attainment 8 Score (2023-2024)", "Average House Price (GBP, 2024)",
             "House Price vs Attainment 8 Score")

# 4. Drug crime rate vs Attainment 8 score
m4 = run_lm(district_master, "avg_att8", "drug_rate_per_10k",
             "Average Attainment 8 Score (2023-2024)", "Drug Offence Rate per 10,000 (2024)",
             "Drug Offence Rate vs Attainment 8 Score")

# 5. Broadband speed vs Drug offence rate
m5 = run_lm(district_master, "drug_rate_per_10k", "avg_download",
             "Drug Offence Rate per 10,000 (2024)", "Average Download Speed (Mbps, 2018)",
             "Broadband Speed vs Drug Offence Rate")

# 6. Broadband speed vs Attainment 8 score
m6 = run_lm(district_master, "avg_att8", "avg_download",
             "Average Attainment 8 Score (2023-2024)", "Average Download Speed (Mbps, 2018)",
             "Broadband Speed vs Attainment 8 Score")

# =============================================================
# SUMMARY COMPARISON TABLE ACROSS ALL 6 MODELS
# =============================================================
model_list = list(
  "House Price ~ Broadband"        = m1,
  "House Price ~ Drug Offence"     = m2,
  "House Price ~ Attainment 8"     = m3,
  "Drug Offence ~ Attainment 8"    = m4,
  "Broadband ~ Drug Offence"       = m5,
  "Broadband ~ Attainment 8"       = m6
)

model_comparison = map_df(names(model_list), function(nm) {
  mod = model_list[[nm]]
  s = summary(mod)
  tibble(
    model         = nm,
    intercept     = coef(mod)[1],
    slope         = coef(mod)[2],
    r_squared     = s$r.squared,
    adj_r_squared = s$adj.r.squared,
    p_value       = s$coefficients[2, 4],
    residual_se   = s$sigma,
    significant   = ifelse(s$coefficients[2, 4] < 0.05, "Yes", "No")
  )
})

cat("\n\n================================================\n")
cat("MODEL COMPARISON TABLE\n")
cat("================================================\n")
print(model_comparison, width = Inf)

write_csv(model_comparison, file.path(data_dir, "model_comparison_summary.csv"))
cat("\nSaved model_comparison_summary.csv\n")