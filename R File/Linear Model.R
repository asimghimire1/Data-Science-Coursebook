# Linear Model.R - statistical models (run after Cleaning.R)
library(tidyverse)

house_data      = read_csv("cleaned/cleaned_house_prices.csv", show_col_types = FALSE)
broadband_data  = read_csv("cleaned/broadband_cleaned_data.csv", show_col_types = FALSE)
crime_data      = read_csv("cleaned/crime_clean_data.csv", show_col_types = FALSE)
population_data = read_csv("cleaned/cleaned_population_data.csv", show_col_types = FALSE)
school_data     = read_csv("cleaned/school_clean_data.csv", show_col_types = FALSE)

# ---- town-level dataset: towns with >= 300 sales, dominant county/district ----
town_tab = house_data %>% count(Town, County, District) %>%
  group_by(Town) %>% mutate(total = sum(n)) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>% ungroup() %>% filter(total >= 300)

town_price = house_data %>% select(-County, -District) %>%
  inner_join(town_tab %>% select(Town, County, District), by = "Town") %>%
  group_by(Town, County, District) %>%
  summarise(average_house_price = mean(Price), .groups = "drop")

town_speed = broadband_data %>%
  inner_join(house_data %>% distinct(Postcode, Town), by = "Postcode") %>%
  group_by(Town) %>%
  summarise(avg_download_speed = mean(`Average download speed (Mbit/s)`), .groups="drop")

# district population from postcode sectors (sector -> dominant district)
sector_district = house_data %>%
  mutate(Sector = sub("^(\\S+\\s\\d).*$","\\1",Postcode)) %>%
  count(Sector, District) %>% group_by(Sector) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>% ungroup() %>% select(Sector, District)
district_pop = sector_district %>% inner_join(population_data, by = "Sector") %>%
  group_by(District) %>% summarise(Population = sum(Population), .groups="drop")

district_crime = crime_data %>% count(District, name = "crimes") %>%
  left_join(crime_data %>% filter(`Crime type`=="Drugs") %>% count(District, name="drugs"),
            by = "District") %>%
  inner_join(district_pop, by = "District") %>%
  mutate(crime_rate_per_1000 = crimes/Population*1000,
         drugs_rate_per_10000 = drugs/Population*10000)

town_school = school_data %>% group_by(Town) %>%
  summarise(average_att8scr = mean(average_att8scr), .groups = "drop")

town_data = town_price %>%
  left_join(town_speed, by = "Town") %>%
  left_join(district_crime %>% select(District, crime_rate_per_1000, drugs_rate_per_10000),
            by = "District") %>%
  left_join(town_school, by = "Town") %>%
  na.omit()
write.csv(town_data, "cleaned/town_data.csv", row.names = FALSE)

sc = function(x, y, xl, yl, ti){
  ggplot(town_data, aes(.data[[x]], .data[[y]], colour = County)) +
    geom_point(size = 1.6) + geom_smooth(method = "lm", se = FALSE, linewidth = .5) +
    scale_y_continuous(labels = scales::comma) + labs(title = ti, x = xl, y = yl)}

# 1. house price ~ download speed
house_broadband_model = lm(average_house_price ~ avg_download_speed, data = town_data)
summary(house_broadband_model)
sc("avg_download_speed","average_house_price","Average Download Speed (Mbps)",
   "Average House Price","Average House Price vs Average Download Speed")
ggsave("graphs/house_price_vs_download_speed.png", width=8, height=4.3)

# 2. house price ~ drug rate
house_drug_model = lm(average_house_price ~ drugs_rate_per_10000, data = town_data)
summary(house_drug_model)
sc("drugs_rate_per_10000","average_house_price","Drug Offence Rate/10000",
   "Average House Price","Average House Price vs Drug Offence Rate/10000")
ggsave("graphs/house_price_vs_drug_rate.png", width=8, height=4.3)

# 3. house price ~ attainment 8
school_house_model = lm(average_house_price ~ average_att8scr, data = town_data)
summary(school_house_model)
sc("average_att8scr","average_house_price","Average Attainment 8 Score",
   "Average House Price","Average House Price vs Average Attainment 8 Score")
ggsave("graphs/house_price_vs_att8scr.png", width=8, height=4.3)

# 4. attainment 8 ~ drug rate
drug_school_model = lm(average_att8scr ~ drugs_rate_per_10000, data = town_data)
summary(drug_school_model)
sc("drugs_rate_per_10000","average_att8scr","Drug Offence Rate per 10,000",
   "Average Attainment 8 Score","Average Attainment 8 Score vs Drug Offence Rate")
ggsave("graphs/att8scr_vs_drug_rate.png", width=8, height=4.3)

# 5. download speed ~ drug rate
drug_broadband_model = lm(avg_download_speed ~ drugs_rate_per_10000, data = town_data)
summary(drug_broadband_model)
sc("drugs_rate_per_10000","avg_download_speed","Drugs Offence Rate per 10000",
   "Average download speed (Mbit/s)","Drugs Offence Rate vs Average Download Speed")
ggsave("graphs/drug_rate_vs_download_speed.png", width=8, height=4.3)

# 6. download speed ~ attainment 8
broadband_school_model = lm(avg_download_speed ~ average_att8scr, data = town_data)
summary(broadband_school_model)
sc("average_att8scr","avg_download_speed","Average Attainment 8 Score",
   "Average download speed (Mbit/s)","Average Download Speed vs Average Attainment 8 Score")
ggsave("graphs/download_speed_vs_att8scr.png", width=8, height=4.3)
