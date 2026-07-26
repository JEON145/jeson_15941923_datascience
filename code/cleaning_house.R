library(tidyverse)

project_dir = "C:/Users/Acer/OneDrive/Desktop/datasci"
data_dir = "C:/Users/Acer/OneDrive/Documents/DATA"
clean_data_dir = file.path(project_dir, "cleaned_data")
dir.create(clean_data_dir, recursive = TRUE, showWarnings = FALSE)

col_names = c(
  "transaction_id",
  "price",
  "date",
  "postcode",
  "property_type",
  "old_new",
  "duration",
  "paon",
  "saon",
  "street",
  "locality",
  "town",
  "district",
  "county",
  "ppd_type",
  "record_status"
)

files = list.files(
  path = file.path(data_dir, "House"),
  pattern = "pp-20(21|22|23|24|25)\\.csv",
  full.names = TRUE
)

house_raw = files %>%
  map_dfr(~ read_csv(.x, col_names = col_names, show_col_types = FALSE))

house_filtered = house_raw %>%
  filter(county %in% c("NORFOLK", "SUFFOLK"))

house_clean = house_filtered %>%
  filter(!is.na(postcode), !is.na(price)) %>%
  distinct(transaction_id, .keep_all = TRUE) %>%
  mutate(
    year = year(date),
    month = month(date)
  ) %>%
  filter(price >= 1000, price <= 10000000) %>%
  mutate(postcode = gsub(" ", "", postcode))

nrow(house_clean)
glimpse(house_clean)

write_csv(house_clean, file.path(clean_data_dir, "house_prices_clean.csv"))

