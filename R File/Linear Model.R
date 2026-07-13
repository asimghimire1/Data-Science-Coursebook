# ST5014CEM Data Science for Developers
# Asim Ghimire (240330)
#


library(tidyverse)
setwd("C:/Users/asimg/Downloads/Data Science Codes Files")

# Colour palette

CORAL <- "#F97068"
TEAL  <- "#00BFC9"
county_colours <- c(Norfolk = CORAL, Suffolk = TEAL)


# Load the cleaned datasets


house_data      <- read_csv("cleaned/cleaned_house_prices.csv", show_col_types = FALSE)
broadband_data  <- read_csv("cleaned/broadband_cleaned_data.csv", show_col_types = FALSE)
crime_data      <- read_csv("cleaned/crime_clean_data.csv", show_col_types = FALSE)
population_data <- read_csv("cleaned/cleaned_population_data.csv", show_col_types = FALSE)
school_data     <- read_csv("cleaned/school_clean_data.csv", show_col_types = FALSE)



# Town-level dataset
# A town qualifies once it has at least 300 sales across 2021-2025, and is assigned
# the county and district in which most of its sales occurred.
town_tab <- house_data %>%
  count(Town, County, District) %>%
  group_by(Town) %>%
  mutate(total = sum(n)) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  filter(total >= 300)

town_price <- house_data %>%
  select(-County, -District) %>%
  inner_join(town_tab %>% select(Town, County, District), by = "Town") %>%
  group_by(Town, County, District) %>%
  summarise(average_house_price = mean(Price), .groups = "drop")

town_speed <- broadband_data %>%
  inner_join(house_data %>% distinct(Postcode, Town), by = "Postcode") %>%
  group_by(Town) %>%
  summarise(avg_download_speed = mean(`Average download speed (Mbit/s)`), .groups = "drop")

# Population is published per postcode sector, so each sector is mapped to the
# district that most of its sales fall in, then summed to give district population.
sector_district <- house_data %>%
  mutate(Sector = sub("^(\\S+\\s\\d).*$", "\\1", Postcode)) %>%
  count(Sector, District) %>%
  group_by(Sector) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(Sector, District)

district_pop <- sector_district %>%
  inner_join(population_data, by = "Sector") %>%
  group_by(District) %>%
  summarise(Population = sum(Population), .groups = "drop")

# Crime is only comparable between districts once it is expressed as a rate.
district_drugs <- crime_data %>%
  filter(`Crime type` == "Drugs") %>%
  count(District, name = "drugs")

district_crime <- crime_data %>%
  count(District, name = "crimes") %>%
  left_join(district_drugs, by = "District") %>%
  inner_join(district_pop, by = "District") %>%
  mutate(crime_rate_per_1000   = crimes / Population * 1000,
         drugs_rate_per_10000  = drugs  / Population * 10000)

town_school <- school_data %>%
  group_by(Town) %>%
  summarise(average_att8scr = mean(average_att8scr), .groups = "drop")

# na.omit() keeps only the towns that carry a complete set of measures.
town_data <- town_price %>%
  left_join(town_speed, by = "Town") %>%
  left_join(district_crime %>% select(District, crime_rate_per_1000, drugs_rate_per_10000),
            by = "District") %>%
  left_join(town_school, by = "Town") %>%
  na.omit()

write.csv(town_data, "cleaned/town_data.csv", row.names = FALSE)

cat("town_data:", nrow(town_data), "towns\n\n")



# For Scatter plot 

scatter <- function(x, y, x_label, y_label, title) {
  ggplot(town_data, aes(.data[[x]], .data[[y]], colour = County)) +
    geom_point(size = 2.4) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.7) +
    scale_colour_manual(values = county_colours) +
    scale_y_continuous(labels = scales::comma) +
    labs(title = title, x = x_label, y = y_label)
}



# Model 1. Average house price ~ average download speed


house_broadband_model <- lm(average_house_price ~ avg_download_speed, data = town_data)
summary(house_broadband_model)

scatter("avg_download_speed", "average_house_price",
        "Average Download Speed (Mbps)",
        "Average House Price",
        "Average House Price vs Average Download Speed")

ggsave("graphs/house_price_vs_download_speed.png", width = 8, height = 4.3)


# ---------------------------------------------------------------------------
# Model 2. Average house price ~ drug offence rate
# ---------------------------------------------------------------------------

house_drug_model <- lm(average_house_price ~ drugs_rate_per_10000, data = town_data)
summary(house_drug_model)

scatter("drugs_rate_per_10000", "average_house_price",
        "Drug Offence Rate/10000",
        "Average House Price",
        "Average House Price vs Drug Offence Rate/10000")

ggsave("graphs/house_price_vs_drug_rate.png", width = 8, height = 4.3)


# ---------------------------------------------------------------------------
# Model 3. Average house price ~ Attainment 8 score
# ---------------------------------------------------------------------------

school_house_model <- lm(average_house_price ~ average_att8scr, data = town_data)
summary(school_house_model)

scatter("average_att8scr", "average_house_price",
        "Average Attainment 8 Score",
        "Average House Price",
        "Average House Price vs Average Attainment 8 Score")

ggsave("graphs/house_price_vs_att8scr.png", width = 8, height = 4.3)


# ---------------------------------------------------------------------------
# Model 4. Attainment 8 score ~ drug offence rate
# ---------------------------------------------------------------------------

drug_school_model <- lm(average_att8scr ~ drugs_rate_per_10000, data = town_data)
summary(drug_school_model)

scatter("drugs_rate_per_10000", "average_att8scr",
        "Drug Offence Rate per 10,000",
        "Average Attainment 8 Score",
        "Average Attainment 8 Score vs Drug Offence Rate")

ggsave("graphs/att8scr_vs_drug_rate.png", width = 8, height = 4.3)


# ---------------------------------------------------------------------------
# Model 5. Average download speed ~ drug offence rate
# ---------------------------------------------------------------------------

drug_broadband_model <- lm(avg_download_speed ~ drugs_rate_per_10000, data = town_data)
summary(drug_broadband_model)

scatter("drugs_rate_per_10000", "avg_download_speed",
        "Drugs Offence Rate per 10000",
        "Average download speed (Mbit/s)",
        "Drugs Offence Rate vs Average Download Speed")

ggsave("graphs/drug_rate_vs_download_speed.png", width = 8, height = 4.3)


# ---------------------------------------------------------------------------
# Model 6. Average download speed ~ Attainment 8 score
# ---------------------------------------------------------------------------

broadband_school_model <- lm(avg_download_speed ~ average_att8scr, data = town_data)
summary(broadband_school_model)

scatter("average_att8scr", "avg_download_speed",
        "Average Attainment 8 Score",
        "Average download speed (Mbit/s)",
        "Average Download Speed vs Average Attainment 8 Score")

ggsave("graphs/download_speed_vs_att8scr.png", width = 8, height = 4.3)
