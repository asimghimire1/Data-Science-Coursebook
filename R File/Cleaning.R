# ST5014CEM Data Science for Developers
# Asim Ghimire (240330)


library(tidyverse)
library(lubridate)
library(DBI)
library(RSQLite)


setwd("C:/Users/asimg/Downloads/Data Science Codes Files")

dir.create("cleaned", showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)

# "nr13ab" -> "NR1 3AB"
std_pc <- function(x) {
  x <- toupper(gsub("\\s+", "", x))
  ifelse(nchar(x) >= 5,
         paste0(substr(x, 1, nchar(x) - 3), " ", substr(x, nchar(x) - 2, 
                                                        nchar(x))),
         x)
}

# "NR1 3AB" -> "NR1 3"  (the postcode sector)
pc_sector <- function(pc) {
  sub("^(\\S+\\s\\d).*$", "\\1", pc)
}

# Price Paid stores County and District in upper case, but the crime data derives
# them in title case from the LSOA names. The two must agree or every join between
# housing and crime returns zero rows.
title_uk <- function(x) {
  x <- tolower(x)
  x <- gsub("(^|[ -])([a-z])", "\\1\\U\\2", x, perl = TRUE)
  gsub("\\bAnd\\b", "and", x)
}


# 1. Housing Price  (HM Land Registry Price Paid)
# The raw Price Paid files have NO header row, so the column names are supplied here.
pp_cols <- c("Transaction_ID", "Price", "Date_of_Transfer", "Postcode",
             "Property_Type", "New_Build", "Freehold", "PAON",
             "SAON", "Street", "Locality", "Town",
             "District", "County", "Category_Type", "Record_Status")

house_prices_2021 <- read_csv("data/pp-2021.csv", col_names = pp_cols, 
                              show_col_types = FALSE)
house_prices_2022 <- read_csv("data/pp-2022.csv", col_names = pp_cols, 
                              show_col_types = FALSE)
house_prices_2023 <- read_csv("data/pp-2023.csv", col_names = pp_cols, 
                              show_col_types = FALSE)
house_prices_2024 <- read_csv("data/pp-2024.csv", col_names = pp_cols, 
                              show_col_types = FALSE)
house_prices_2025 <- read_csv("data/pp-2025.csv", col_names = pp_cols, 
                              show_col_types = FALSE)

merged_house_prices <- bind_rows(house_prices_2021,
                                 house_prices_2022,
                                 house_prices_2023,
                                 house_prices_2024,
                                 house_prices_2025)

# Dates arrive as "2021-06-09 00:00", so only the first 10 characters are parsed.
cleaned_house_prices <- merged_house_prices %>%
  filter(toupper(County) %in% c("NORFOLK", "SUFFOLK")) %>%
  mutate(Date_of_Transfer = as.Date(substr(Date_of_Transfer, 1, 10)),
         Postcode         = std_pc(Postcode),
         Price            = as.numeric(Price),
         Town             = str_to_title(Town),
         County           = title_uk(County),
         District         = title_uk(District),
         Year             = year(Date_of_Transfer)) %>%
  filter(!is.na(Price), Price > 1000, !is.na(Postcode)) %>%
  distinct()

write.csv(cleaned_house_prices,
          "cleaned/cleaned_house_prices.csv",
          row.names = FALSE)

cat("Housing   :", nrow(cleaned_house_prices), "sales |",
    n_distinct(cleaned_house_prices$District), "districts\n")

# 2. Broadband  (Ofcom fixed postcode performance)


broadband_data <- read_csv("data/201805_fixed_pc_performance_r03.csv",
                           show_col_types = FALSE)


broadband_clean_data <- broadband_data %>%
  transmute(Postcode = std_pc(postcode_space),
            `Average download speed (Mbit/s)` = as.numeric
            (`Average download speed (Mbit/s)`),
            `Maximum download speed (Mbit/s)` = as.numeric
            (`Maximum download speed (Mbit/s)`)) %>%
  filter(!is.na(`Average download speed (Mbit/s)`)) %>%
  semi_join(cleaned_house_prices %>% distinct(Postcode), by = "Postcode")

write.csv(broadband_clean_data,
          "cleaned/broadband_cleaned_data.csv",
          row.names = FALSE)

cat("Broadband :", nrow(broadband_clean_data), "study-area postcodes\n")


# 3. Crime  (data.police.uk monthly street-level files)


crime_files <- list.files("data/crime",
                          pattern    = "\\.csv$",
                          recursive  = TRUE,
                          full.names = TRUE)

merged_crime <- map_dfr(crime_files, \(f) read_csv(f, show_col_types = FALSE))


# Each name must stay on ONE line: a string broken across two lines keeps the
# newline and the indent inside it, and would then match no LSOA name at all.
districts <- c("Breckland", "Broadland", "Great Yarmouth",
               "King's Lynn and West Norfolk",
               "North Norfolk", "Norwich", "South Norfolk",
               "Babergh", "East Suffolk", "Ipswich", "Mid Suffolk",
               "West Suffolk",
               "Forest Heath", "St Edmundsbury", "Suffolk Coastal",
               "Waveney")

suffolk_districts <- c("Babergh", "East Suffolk", "Ipswich", "Mid Suffolk",
                       "West Suffolk")

crime_cleaned_data <- merged_crime %>%
  select(Month, `Crime type`, `LSOA code`, `LSOA name`) %>%
  filter(!is.na(`LSOA code`),
         str_detect(`LSOA name`,
                    paste0("^(", paste(districts, collapse = "|"), ") "))) %>%
  mutate(District = str_remove(`LSOA name`, " \\d+[A-Z]?$")) %>%
  # The 2019 boundary review merged four Suffolk districts into two.
  mutate(District = case_when(
           District %in% c("Forest Heath", "St Edmundsbury")  ~ "West Suffolk",
           District %in% c("Suffolk Coastal", "Waveney")      ~ "East Suffolk",
           TRUE                                               ~ District)) %>%
  mutate(County = if_else(District %in% suffolk_districts,
                          "Suffolk", "Norfolk"))

write.csv(crime_cleaned_data,
          "cleaned/crime_clean_data.csv",
          row.names = FALSE)

cat("Crime     :", nrow(crime_cleaned_data), "kept of", nrow(merged_crime),
    "|", nrow(merged_crime) - nrow(crime_cleaned_data), "dropped\n")



# 4. Population  (2011 census, postcode-sector level)


# Population values are quoted and comma-separated, e.g. "12,345".
population_clean <- read_csv("data/Population2011_1656567141570.csv",
                             show_col_types = FALSE) %>%
  transmute(Sector     = str_squish(toupper(Postcode)),
            Population = as.numeric(gsub(",", "", Population))) %>%
  filter(!is.na(Population))

write.csv(population_clean,
          "cleaned/cleaned_population_data.csv",
          row.names = FALSE)

cat("Population:", nrow(population_clean), "postcode sectors\n")



# 5. School  (KS4 Attainment 8)


ks4_files <- list.files("data/school",
                        pattern    = "ks4final\\.csv$",
                        recursive  = TRUE,
                        full.names = TRUE)

info_files <- list.files("data/school",
                         pattern    = "school_information\\.csv$",
                         recursive  = TRUE,
                         full.names = TRUE)


read_ks4 <- function(f) {
  academic_year <- str_extract(f, "20\\d{2}-20\\d{2}")

  read_csv(f, col_types = cols(.default = "c")) %>%
    transmute(URN,
              Academic_Year = academic_year,
              ATT8SCR       = suppressWarnings(as.numeric(ATT8SCR)))
}

school_scores <- map_dfr(ks4_files, read_ks4) %>%
  filter(!is.na(ATT8SCR), !is.na(URN))

school_info <- map_dfr(info_files, \(f)
    read_csv(f, col_types = cols(.default = "c")) %>%
      transmute(URN, Postcode = std_pc(POSTCODE))) %>%
  distinct(URN, .keep_all = TRUE)


sector_town <- cleaned_house_prices %>%
  mutate(Sector = pc_sector(Postcode)) %>%
  count(Sector, Town, County) %>%
  group_by(Sector) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(Sector, Town, County)

school_cleaned_data <- school_scores %>%
  inner_join(school_info, by = "URN") %>%
  mutate(Sector = pc_sector(Postcode)) %>%
  inner_join(sector_town, by = "Sector") %>%
  group_by(Town, County, Academic_Year) %>%
  summarise(average_att8scr = mean(ATT8SCR), .groups = "drop")

write.csv(school_cleaned_data,
          "cleaned/school_clean_data.csv",
          row.names = FALSE)

cat("School    :", nrow(school_cleaned_data), "town-year rows |",
    n_distinct(school_cleaned_data$Town), "towns\n")



# 6. Normalised (3NF) SQLite database


library(DBI); library(RSQLite)
con <- dbConnect(SQLite(), "outputs/property_analysis.sqlite")

districts_tbl <- cleaned_house_prices %>%
  distinct(District, County) %>%
  arrange(County, District) %>%
  mutate(district_id = row_number())

# A town qualifies for the study once it has at least 300 sales across 2021-2025.
# Each town is assigned the district in which most of its sales occurred.
towns_tbl <- cleaned_house_prices %>%
  count(Town, District) %>%
  group_by(Town) %>%
  mutate(total = sum(n)) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  filter(total >= 300) %>%
  #inner join applied here
  inner_join(districts_tbl, by = "District") %>%
  transmute(town_id = row_number(), Town, district_id)

postcodes_tbl <- cleaned_house_prices %>%
  distinct(Postcode, Town) %>%
  inner_join(towns_tbl, by = "Town") %>%
  transmute(Postcode, town_id, Sector = pc_sector(Postcode))

house_sales_tbl <- cleaned_house_prices %>% mutate(sale_id  = row_number())
crimes_tbl      <- crime_cleaned_data  %>% mutate(crime_id = row_number())

dbWriteTable(con, "districts",   districts_tbl,        overwrite = TRUE)
dbWriteTable(con, "towns",       towns_tbl,            overwrite = TRUE)
dbWriteTable(con, "postcodes",   postcodes_tbl,        overwrite = TRUE)
dbWriteTable(con, "house_sales", house_sales_tbl,      overwrite = TRUE)
dbWriteTable(con, "broadband",   broadband_clean_data, overwrite = TRUE)
dbWriteTable(con, "crimes",      crimes_tbl,           overwrite = TRUE)
dbWriteTable(con, "population",  population_clean,     overwrite = TRUE)
dbWriteTable(con, "schools",     school_cleaned_data,  overwrite = TRUE)

dbListTables(con)

dbDisconnect(con)
