
library(tidyverse)

project_dir = "C:/Users/Acer/OneDrive/Desktop/datasci"
data_dir = "C:/Users/Acer/OneDrive/Documents/DATA"
clean_data_dir = file.path(project_dir, "cleaned_data")
dir.create(clean_data_dir, recursive = TRUE, showWarnings = FALSE)

read_ks4 = function(path, county, year) {
  read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    mutate(
      county = county,
      academic_year = year
    )
}

nor_2122 = read_ks4(file.path(data_dir, "school", "Nor", "2021-2022", "926_ks4final.csv"), "Norfolk", "2021-2022")
nor_2223 = read_ks4(file.path(data_dir, "school", "Nor", "2022-2023", "926_ks4final.csv"), "Norfolk", "2022-2023")
nor_2324 = read_ks4(file.path(data_dir, "school", "Nor", "2023-2024", "926_ks4final.csv"), "Norfolk", "2023-2024")
nor_2425 = read_ks4(file.path(data_dir, "school", "Nor", "2024-2025", "926_ks4final.csv"), "Norfolk", "2024-2025")

suf_2122 = read_ks4(file.path(data_dir, "school", "suf", "2021-2022", "935_ks4final.csv"), "Suffolk", "2021-2022")
suf_2223 = read_ks4(file.path(data_dir, "school", "suf", "2022-2023", "935_ks4final.csv"), "Suffolk", "2022-2023")
suf_2324 = read_ks4(file.path(data_dir, "school", "suf", "2023-2024", "935_ks4final.csv"), "Suffolk", "2023-2024")
suf_2425 = read_ks4(file.path(data_dir, "school", "suf", "2024-2025", "935_ks4final.csv"), "Suffolk", "2024-2025")

schools_raw = bind_rows(
  nor_2122, nor_2223, nor_2324, nor_2425,
  suf_2122, suf_2223, suf_2324, suf_2425
)

glimpse(schools_raw)
nrow(schools_raw)

names(schools_raw)[grepl("ATT8", names(schools_raw), ignore.case = TRUE)]

schools_clean = schools_raw %>%
  select(
    urn = URN,
    school_name = SCHNAME,
    postcode = PCODE,
    lea_code = LEA,
    county = county,
    academic_year = academic_year,
    total_pupils = TOTPUPS,
    att8_score = ATT8SCR
  ) %>%
  mutate(
    att8_score = suppressWarnings(as.numeric(att8_score)),
    total_pupils = suppressWarnings(as.numeric(total_pupils)),
    postcode = gsub(" ", "", postcode)
  ) %>%
  filter(!is.na(att8_score))

postcode_clean = read_csv(
  file.path(clean_data_dir, "postcode_lookup_clean.csv"),
  show_col_types = FALSE
)

schools_clean = schools_clean %>%
  select(-starts_with("district_name")) %>%
  left_join(
    postcode_clean %>% distinct(postcode, district_name),
    by = "postcode"
  ) %>%
  filter(!is.na(district_name))

write_csv(schools_clean, file.path(clean_data_dir, "schools_clean.csv"))
