
library(tidyverse)

project_dir = "C:/Users/Acer/OneDrive/Desktop/datasci"
data_dir = "C:/Users/Acer/OneDrive/Documents/DATA"
clean_data_dir = file.path(project_dir, "cleaned_data")
dir.create(clean_data_dir, recursive = TRUE, showWarnings = FALSE)

crime_files = list.files(
  path = file.path(data_dir, "Crime"),
  pattern = "*.csv",
  full.names = TRUE,
  recursive = TRUE
)

crime_raw = crime_files %>%
  map_dfr(~ read_csv(.x, show_col_types = FALSE))

crime_clean = crime_raw %>%
  rename(
    crime_id = `Crime ID`,
    month = Month,
    reported_by = `Reported by`,
    falls_within = `Falls within`,
    longitude = Longitude,
    latitude = Latitude,
    location = Location,
    lsoa_code = `LSOA code`,
    lsoa_name = `LSOA name`,
    crime_type = `Crime type`,
    last_outcome = `Last outcome category`
  ) %>%
  select(-Context) %>%
  filter(!is.na(lsoa_code)) %>%
  mutate(
    year = as.integer(substr(month, 1, 4)),
    month_num = as.integer(substr(month, 6, 7))
  )

crime_clean %>%
  count(crime_type, sort = TRUE)

postcode_clean = read_csv(
  file.path(clean_data_dir, "postcode_lookup_clean.csv"),
  show_col_types = FALSE
)

crime_clean = crime_clean %>%
  inner_join(
    postcode_clean %>% distinct(lsoa_code, district_name),
    by = "lsoa_code"
  )

crime_clean %>%
  distinct(district_name) %>%
  arrange(district_name)

nrow(crime_clean)

write_csv(crime_clean, file.path(clean_data_dir, "crime_clean.csv"))