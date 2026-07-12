# ST5014CEM Data Science for Developers - Asim Ghimire (240330)
# Project structure (relative paths - set the working directory to the project root):
#   data/pp-2021.csv ... pp-2025.csv, data/201805_fixed_pc_performance_r03.csv,
#   data/crime/<month folders>, data/school/<Nor|suf>/<year>/, 
#   data/Population2011_1656567141570.csv, data/PCD_OA21_LSOA21_MSOA21_LAD_MAY26_UK_LU.csv
# Outputs are written to cleaned/, graphs/ and outputs/.
library(tidyverse)
library(lubridate)

std_pc <- function(x){ x <- toupper(gsub("\\s+","",x))
  ifelse(nchar(x)>=5, paste0(substr(x,1,nchar(x)-3)," ",substr(x,nchar(x)-2,nchar(x))), x)}
pc_sector <- function(pc) sub("^(\\S+\\s\\d).*$", "\\1", pc)

# Price Paid stores County/District upper case; the crime cleaner derives them in title
# case from LSOA names. Both must agree or every house-crime join returns zero rows.
# "and" stays lower case so "King's Lynn and West Norfolk" matches the LSOA spelling.
title_uk <- function(x){ x <- tolower(x)
  x <- gsub("(^|[ -])([a-z])", "\\1\\U\\2", x, perl = TRUE)
  gsub("\\bAnd\\b", "and", x)}

dir.create("cleaned", showWarnings = FALSE)

# ---------------- Housing Price ----------------
pp_cols = c("Transaction_ID","Price","Date_of_Transfer","Postcode","Property_Type",
  "New_Build","Freehold","PAON","SAON","Street","Locality","Town",
  "District","County","Category_Type","Record_Status")

merged_house_prices = map_dfr(2021:2025, \(y)
  read_csv(sprintf("data/pp-%d.csv", y), col_names = pp_cols, show_col_types = FALSE))

cleaned_house_prices = merged_house_prices %>%
  filter(toupper(County) %in% c("NORFOLK","SUFFOLK")) %>%
  mutate(Date_of_Transfer = as.Date(substr(Date_of_Transfer,1,10)),
         Postcode = std_pc(Postcode), Price = as.numeric(Price),
         Town = str_to_title(Town), County = title_uk(County),
         District = title_uk(District), Year = year(Date_of_Transfer)) %>%
  filter(!is.na(Price), Price > 1000, !is.na(Postcode)) %>% distinct()
write.csv(cleaned_house_prices, "cleaned/cleaned_house_prices.csv", row.names = FALSE)

# ---------------- Broadband ----------------
broadband_data = read_csv("data/201805_fixed_pc_performance_r03.csv", show_col_types = FALSE)
broadband_clean_data = broadband_data %>%
  transmute(Postcode = std_pc(postcode_space),
    `Average download speed (Mbit/s)` = as.numeric(`Average download speed (Mbit/s)`),
    `Maximum download speed (Mbit/s)` = as.numeric(`Maximum download speed (Mbit/s)`)) %>%
  filter(!is.na(`Average download speed (Mbit/s)`)) %>%
  semi_join(cleaned_house_prices %>% distinct(Postcode), by = "Postcode")
write.csv(broadband_clean_data, "cleaned/broadband_cleaned_data.csv", row.names = FALSE)

# ---------------- Crime ----------------
crime_files = list.files("data/crime", pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
districts = c("Breckland","Broadland","Great Yarmouth","King's Lynn and West Norfolk",
  "North Norfolk","Norwich","South Norfolk","Babergh","East Suffolk","Ipswich",
  "Mid Suffolk","West Suffolk","Forest Heath","St Edmundsbury","Suffolk Coastal","Waveney")

crime_cleaned_data = map_dfr(crime_files, \(f) read_csv(f, show_col_types = FALSE)) %>%
  select(Month, `Crime type`, `LSOA code`, `LSOA name`) %>%
  filter(!is.na(`LSOA code`),
         str_detect(`LSOA name`, paste0("^(", paste(districts, collapse="|"), ") "))) %>%
  mutate(District = str_remove(`LSOA name`, " \\d+[A-Z]?$"),
         District = case_when(
           District %in% c("Forest Heath","St Edmundsbury") ~ "West Suffolk",
           District %in% c("Suffolk Coastal","Waveney") ~ "East Suffolk",
           TRUE ~ District),
         County = if_else(District %in% c("Babergh","East Suffolk","Ipswich",
                  "Mid Suffolk","West Suffolk"), "Suffolk", "Norfolk"))
write.csv(crime_cleaned_data, "cleaned/crime_clean_data.csv", row.names = FALSE)

# ---------------- Population (postcode-sector level) ----------------
population_clean = read_csv("data/Population2011_1656567141570.csv", show_col_types = FALSE) %>%
  transmute(Sector = str_squish(toupper(Postcode)),
            Population = as.numeric(gsub(",", "", Population))) %>%
  filter(!is.na(Population))
write.csv(population_clean, "cleaned/cleaned_population_data.csv", row.names = FALSE)

# ---------------- School (KS4 Attainment 8) ----------------
ks4_files  = list.files("data/school", pattern = "ks4final\\.csv$",
                        recursive = TRUE, full.names = TRUE)
info_files = list.files("data/school", pattern = "school_information\\.csv$",
                        recursive = TRUE, full.names = TRUE)
read_ks4 = function(f){ yr = str_extract(f, "20\\d{2}-20\\d{2}")
  read_csv(f, col_types = cols(.default = "c")) %>%
    transmute(URN, Academic_Year = yr, ATT8SCR = suppressWarnings(as.numeric(ATT8SCR))) }
school_info = map_dfr(info_files, \(f) read_csv(f, col_types = cols(.default="c")) %>%
    transmute(URN, Postcode = std_pc(POSTCODE))) %>% distinct(URN, .keep_all = TRUE)

# A school's own postcode is rarely one that has had a house sale, so joining schools to
# towns on the exact postcode matches only ~40% of them and silently drops 16 towns.
# Match on postcode sector instead - the level the population data already works at -
# taking each sector's dominant town. This matches all 146 schools.
sector_town = cleaned_house_prices %>%
  mutate(Sector = pc_sector(Postcode)) %>%
  count(Sector, Town, County) %>% group_by(Sector) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>% ungroup() %>%
  select(Sector, Town, County)

school_cleaned_data = map_dfr(ks4_files, read_ks4) %>%
  filter(!is.na(ATT8SCR), !is.na(URN)) %>%
  inner_join(school_info, by = "URN") %>%
  mutate(Sector = pc_sector(Postcode)) %>%
  inner_join(sector_town, by = "Sector") %>%
  group_by(Town, County, Academic_Year) %>%
  summarise(average_att8scr = mean(ATT8SCR), .groups = "drop")
write.csv(school_cleaned_data, "cleaned/school_clean_data.csv", row.names = FALSE)

# ---------------- 3NF database (2026 brief requirement) ----------------
library(DBI); library(RSQLite)
con = dbConnect(SQLite(), "outputs/property_analysis.sqlite")
districts_tbl = cleaned_house_prices %>% distinct(District, County) %>%
  arrange(County, District) %>% mutate(district_id = row_number())
towns_tbl = cleaned_house_prices %>% count(Town, District) %>%
  group_by(Town) %>% mutate(total = sum(n)) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>% ungroup() %>%
  filter(total >= 300) %>%
  inner_join(districts_tbl, by = "District") %>%
  transmute(town_id = row_number(), Town, district_id)
postcodes_tbl = cleaned_house_prices %>% distinct(Postcode, Town) %>%
  inner_join(towns_tbl, by = "Town") %>%
  transmute(Postcode, town_id, Sector = pc_sector(Postcode))

dbWriteTable(con, "districts",  districts_tbl,        overwrite = TRUE)
dbWriteTable(con, "towns",      towns_tbl,            overwrite = TRUE)
dbWriteTable(con, "postcodes",  postcodes_tbl,        overwrite = TRUE)
dbWriteTable(con, "house_sales", cleaned_house_prices %>% mutate(sale_id = row_number()), overwrite = TRUE)
dbWriteTable(con, "broadband",  broadband_clean_data, overwrite = TRUE)
dbWriteTable(con, "crimes",     crime_cleaned_data %>% mutate(crime_id = row_number()), overwrite = TRUE)
dbWriteTable(con, "population", population_clean,     overwrite = TRUE)
dbWriteTable(con, "schools",    school_cleaned_data,  overwrite = TRUE)
dbListTables(con)
dbDisconnect(con)
