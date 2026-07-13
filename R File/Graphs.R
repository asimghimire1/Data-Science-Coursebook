# ST5014CEM Data Science for Developers
# Asim Ghimire (240330)

library(tidyverse)
library(lubridate)
library(scales)

options(scipen = 1000)


setwd("C:/Users/asimg/Downloads/Data Science Codes Files")

dir.create("graphs", showWarnings = FALSE)

# Colour palette

CORAL <- "#F97068"
TEAL  <- "#00BFC9"

# Deeper siblings of the same two hues, used only for outlines and never as a fill.
CORAL_DEEP <- "#C0453C"
TEAL_DEEP  <- "#0094A6"

county_fill    <- c(Norfolk = CORAL,      Suffolk = TEAL)
county_outline <- c(Norfolk = CORAL_DEEP, Suffolk = TEAL_DEEP)


# Loading the cleaned datasets

cleaned_house_price_data <- read_csv("cleaned/cleaned_house_prices.csv", 
                                     show_col_types = FALSE)
broadband_data           <- read_csv("cleaned/broadband_cleaned_data.csv", 
                                     show_col_types = FALSE)
crime_data               <- read_csv("cleaned/crime_clean_data.csv", 
                                     show_col_types = FALSE)
population_data          <- read_csv("cleaned/cleaned_population_data.csv", 
                                     show_col_types = FALSE)
school_data              <- read_csv("cleaned/school_clean_data.csv", 
                                     show_col_types = FALSE)


# A town qualifies once it has at least 300 sales, and is assigned the county
# in which most of its sales occurred.
town_tab <- cleaned_house_price_data %>%
  count(Town, County) %>%
  group_by(Town) %>%
  mutate(total = sum(n)) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  filter(total >= 300)

price_town <- cleaned_house_price_data %>%
  select(-County) %>%
  inner_join(town_tab %>% select(Town, County), by = "Town")

# Town names are long, so the x axis labels are angled on every town-level chart.
angled_x <- theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))



# 1-2. Box plots of 2024 house prices by town  (log scale)

for (county in c("Norfolk", "Suffolk")) {

  plot <- price_town %>%
    filter(County == county, Year == 2024) %>%
    ggplot(aes(Town, Price)) +
    geom_boxplot(fill         = county_fill[county],
                 colour       = county_outline[county],
                 outlier.size = 0.4) +
    scale_y_log10(labels = comma) +
    angled_x +
    labs(title = sprintf("Box Plot of House Prices Across Towns in %s for the Year 2024", county),
         x     = "Town",
         y     = "Price (£)")

  ggsave(sprintf("graphs/%s_house_prices_boxplot.png", tolower(county)),
         plot, width = 8.6, height = 4.6)
}



# 3-4. Average 2024 house price by town  (bar chart)


for (county in c("Norfolk", "Suffolk")) {

  plot <- price_town %>%
    filter(County == county, Year == 2024) %>%
    group_by(Town) %>%
    summarise(average_price = mean(Price)) %>%
    ggplot(aes(Town, average_price)) +
    geom_col(fill = county_fill[county]) +
    scale_y_continuous(labels = comma) +
    angled_x +
    labs(title = sprintf("Average House Prices by Town in %s (2024)", county),
         x     = "Town",
         y     = "Average Price (£)")

  ggsave(sprintf("graphs/%s_avg_price_barchart.png", tolower(county)),
         plot, width = 8.6, height = 4.4)
}



# 5. Monthly average house price by county

plot <- cleaned_house_price_data %>%
  mutate(Month_Start = floor_date(Date_of_Transfer, "month")) %>%
  group_by(County, Month_Start) %>%
  summarise(average_price = mean(Price), .groups = "drop") %>%
  ggplot(aes(Month_Start, average_price, colour = County)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.2) +
  scale_colour_manual(values = county_fill) +
  scale_y_continuous(labels = comma) +
  labs(title = "Monthly Average House Prices in Norfolk and Suffolk",
       x     = "Month",
       y     = "Average Price (£)")

ggsave("graphs/avg_price_line_chart.png", plot, width = 8.6, height = 4.4)


# 6-7. Average and maximum download speed by town


broadband_town <- broadband_data %>%
  inner_join(cleaned_house_price_data %>% distinct(Postcode, Town), by = 
               "Postcode") %>%
  inner_join(town_tab %>% select(Town, County), by = "Town")

for (county in c("Norfolk", "Suffolk")) {

  plot <- broadband_town %>%
    filter(County == county) %>%
    group_by(Town) %>%
    summarise(average = mean(`Average download speed (Mbit/s)`),
              maximum = mean(`Maximum download speed (Mbit/s)`)) %>%
    pivot_longer(-Town, names_to = "Metric") %>%
    ggplot(aes(Town, value, fill = Metric)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = c(average = CORAL, maximum = TEAL)) +
    angled_x +
    labs(title = sprintf("Average and Maximum Download Speed in %s", county),
         x     = "Town",
         y     = "Download Speed (Mbit/s)")

  ggsave(sprintf("graphs/%s_download_speed_barchart.png", tolower(county)),
         plot, width = 8.6, height = 4.5)
}



# 8. Monthly drug crimes in 2024, by county

plot <- crime_data %>%
  filter(`Crime type` == "Drugs", str_starts(Month, "2024")) %>%
  count(County, Month) %>%
  ggplot(aes(n, County, fill = County)) +
  geom_boxplot(alpha = 0.85, show.legend = FALSE) +
  scale_fill_manual(values = county_fill) +
  labs(title = "Boxplot of Monthly Drug Crimes in 2024 by County",
       x     = "Count of Drug Crimes",
       y     = "County")

ggsave("graphs/drug_boxplot_2024.png", plot, width = 7.6, height = 3.9)


# 9. Vehicle crime radar - the main town of each district, April 2024

# The main town of a district is the one with the most sales. Only Town is kept:
# carrying the count column through would collide with the crime count below.
main_towns <- cleaned_house_price_data %>%
  count(District, Town) %>%
  group_by(District) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(District, Town)

vehicle_crime <- crime_data %>%
  filter(`Crime type` == "Vehicle crime", Month == "2024-04") %>%
  count(District) %>%
  inner_join(main_towns, by = "District") %>%
  group_by(Town) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  mutate(pct = n / max(n) * 100) %>%
  arrange(Town)

# Repeat the first row so the polygon closes back on itself.
radar_points <- rbind(vehicle_crime, vehicle_crime[1, ])
angles       <- seq(0, 2 * pi, length.out = nrow(radar_points))
spoke_angles <- angles[-length(angles)]

png("graphs/vehicle_crime_radar.png", width = 1150, height = 900, res = 150)
par(mar = c(1, 1, 3, 1))

plot(NA,
     xlim = c(-1.35, 1.35), ylim = c(-1.3, 1.35),
     axes = FALSE, xlab = "", ylab = "",
     main = "Vehicle Crime in April 2024")

# Concentric rings at 25%, 50%, 75% and 100%. The web is drawn in teal so that it
# stays recessive behind the coral data polygon.
for (r in c(0.25, 0.5, 0.75, 1)) {
  lines(r * cos(angles), r * sin(angles), lty = 3, col = TEAL)
  text(0.06, r + 0.03, paste0(r * 100, " (%)"), col = TEAL_DEEP, cex = 0.7)
}

segments(0, 0, cos(spoke_angles), sin(spoke_angles), lty = 3, col = TEAL)

polygon(radar_points$pct / 100 * cos(angles),
        radar_points$pct / 100 * sin(angles),
        col = adjustcolor(CORAL, 0.55), border = CORAL_DEEP, lwd = 2)

text(1.18 * cos(spoke_angles), 1.18 * sin(spoke_angles), vehicle_crime$Town, cex = 0.55)

dev.off()


# 10. Robbery share by county, April 2024


robbery <- crime_data %>%
  filter(`Crime type` == "Robbery", Month == "2024-04") %>%
  count(County) %>%
  mutate(label = paste0(County, " (", round(n / sum(n) * 100, 1), "%)"))

plot <- ggplot(robbery, aes("", n, fill = County)) +
  geom_col(width = 1, colour = "white", linewidth = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = county_fill) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3.4) +
  labs(title = "Robbery Rate by County - April 2024") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

ggsave("graphs/robbery_pie_chart.png", plot, width = 6.4, height = 4.4)



# 11. Drug offence rate per 10,000 people, monthly trend

# Population is published per postcode sector, so it is rolled up to the county.
sector_county <- cleaned_house_price_data %>%
  mutate(Sector = sub("^(\\S+\\s\\d).*$", "\\1", Postcode)) %>%
  distinct(Sector, County)

county_pop <- sector_county %>%
  inner_join(population_data, by = "Sector") %>%
  distinct(Sector, County, Population) %>%
  group_by(County) %>%
  summarise(Population = sum(Population))

plot <- crime_data %>%
  filter(`Crime type` == "Drugs") %>%
  count(County, Month) %>%
  inner_join(county_pop, by = "County") %>%
  mutate(rate       = n / Population * 10000,
         Month_Date = ym(Month)) %>%
  ggplot(aes(Month_Date, rate, colour = County)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.2) +
  scale_colour_manual(values = county_fill) +
  labs(title = "Drug Offence Rate per 10,000 People",
       x     = "Month",
       y     = "Crime Rate (per 10,000)")

ggsave("graphs/drug_offence_rate_line_chart.png", plot, width = 8.6, height = 4.3)



# 12-15. Attainment 8 by town - box plot and line chart per county

latest_year <- max(school_data$Academic_Year)

for (county in c("Norfolk", "Suffolk")) {

  plot <- school_data %>%
    filter(County == county, Academic_Year == latest_year) %>%
    ggplot(aes(Town, average_att8scr)) +
    geom_boxplot(fill = county_fill[county], colour = county_outline[county]) +
    angled_x +
    labs(title = sprintf("Box-Plot for average attainment 8 score of academic year (%s) in %s",
                         latest_year, county),
         x     = "Town",
         y     = "Average Attainment 8 Score")

  ggsave(sprintf("graphs/%s_att8scr_boxplot.png", tolower(county)),
         plot, width = 8.6, height = 4.3)

  plot <- school_data %>%
    filter(County == county) %>%
    group_by(Town) %>%
    summarise(average_att8scr = mean(average_att8scr)) %>%
    ggplot(aes(Town, average_att8scr, group = 1)) +
    geom_line(colour = TEAL, linewidth = 0.7) +
    geom_point(colour = CORAL, size = 1.8) +
    angled_x +
    labs(title = sprintf("Line Chart of Average Attainment 8 Score by Town in %s", county),
         x     = "Town",
         y     = "Average Attainment 8 Score")

  ggsave(sprintf("graphs/%s_att8scr_line_chart.png", tolower(county)),
         plot, width = 8.6, height = 4.3)
}
