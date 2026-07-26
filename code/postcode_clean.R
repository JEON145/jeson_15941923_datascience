library(tidyverse)

project_dir = "C:/Users/Acer/OneDrive/Desktop/datasci"
data_dir = "C:/Users/Acer/OneDrive/Documents/DATA"
clean_data_dir = file.path(project_dir, "cleaned_data")
dir.create(clean_data_dir, recursive = TRUE, showWarnings = FALSE)

postcode_raw = read_csv(
  file.path(data_dir, "PCD_OA21_LSOA21_MSOA21_LAD_MAY26_UK_LU.csv"),
  show_col_types = FALSE
)

glimpse(postcode_raw)

postcode_clean = postcode_raw %>%
  select(
    postcode = pcds,
    lsoa_code = lsoa21cd,
    lsoa_name = lsoa21nm,
    district_code = ladcd,
    district_name = ladnm
  ) %>%
  mutate(postcode = gsub(" ", "", postcode)) %>%
  filter(district_name %in% c(
    "Breckland", "Broadland", "Great Yarmouth",
    "King's Lynn and West Norfolk", "North Norfolk",
    "Norwich", "South Norfolk",
    "Babergh", "East Suffolk", "Ipswich",
    "Mid Suffolk", "West Suffolk"
  ))

nrow(postcode_clean)
distinct(postcode_clean, district_name)

write_csv(postcode_clean, file.path(clean_data_dir, "postcode_lookup_clean.csv"))
