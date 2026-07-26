# =============================================================
# PART 0: SETUP & DATA INSPECTION
# Norfolk & Suffolk Property Investment Analysis
# =============================================================

required_packages <- c("tidyverse", "readr", "readxl")
installed <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!(pkg %in% installed)) install.packages(pkg)
}
lapply(required_packages, library, character.only = TRUE)

# ---- List all CSV files in current project folder ----
all_files <- list.files(".", pattern = "\\.csv$", full.names = TRUE)
cat("Files found:\n")
print(all_files)

# ---- Helper function: load a file and print structure ----
inspect_file <- function(filepath) {
  cat("\n\n================================================\n")
  cat("FILE:", filepath, "\n")
  cat("================================================\n")
  
  df <- tryCatch({
    read_csv(filepath, show_col_types = FALSE)
  }, error = function(e) {
    cat("ERROR reading file:", conditionMessage(e), "\n")
    return(NULL)
  })
  
  if (!is.null(df)) {
    cat("\n--- Column names ---\n")
    print(names(df))
    
    cat("\n--- Structure (str) ---\n")
    str(df)
    
    cat("\n--- First 5 rows ---\n")
    print(head(df, 5))
    
    cat("\n--- Dimensions (rows, cols) ---\n")
    print(dim(df))
    
    cat("\n--- Unique values per column ---\n")
    print(sapply(df, function(x) length(unique(x))))
  }
  
  return(df)
}

# ---- Inspect each of the 6 datasets by exact name ----
file_map <- list(
  broadband   = "broadband_clean.csv",
  crime       = "crime_clean.csv",
  house_price = "house_prices_clean.csv",
  population  = "population_clean.csv",
  postcode    = "postcode_clean.csv",
  school      = "schools_clean.csv"
)

loaded_data <- list()

for (name in names(file_map)) {
  fpath <- file_map[[name]]
  if (file.exists(fpath)) {
    loaded_data[[name]] <- inspect_file(fpath)
  } else {
    cat("\n!! File not found:", fpath, "\n")
  }
}