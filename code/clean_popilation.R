

library(tidyverse)
library(readxl)

project_dir = "C:/Users/Acer/OneDrive/Desktop/datasci"
clean_data_dir = file.path(project_dir, "cleaned_data")
dir.create(clean_data_dir, recursive = TRUE, showWarnings = FALSE)

population_raw = read_excel(
  "C:/Users/Acer/OneDrive/Documents/DATA/pupulation.xlsx",
  sheet = 5,
  skip = 3
)

head(population_raw, 10)

# Load postcode lookup to get Norfolk & Suffolk LSOA codes
postcode_clean = read_csv(
  file.path(clean_data_dir, "postcode_lookup_clean.csv"),
  show_col_types = FALSE
)

norfolk_suffolk_lsoas = postcode_clean %>%
  distinct(lsoa_code, district_name)

population_clean = population_raw %>%
  select(
    lsoa_code = `LSOA 2021 Code`,
    lsoa_name = `LSOA 2021 Name`,
    population = Total
  ) %>%
  # Filter to Norfolk & Suffolk LSOAs only
  inner_join(norfolk_suffolk_lsoas, by = "lsoa_code")


nrow(population_clean)
glimpse(population_clean)