# Graphs.R - EDA visualisations (run after Cleaning.R)
library(tidyverse); library(lubridate); library(scales)
options(scipen = 1000)
dir.create("graphs", showWarnings = FALSE)

cleaned_house_price_data = read_csv("cleaned/cleaned_house_prices.csv", show_col_types = FALSE)
broadband_data = read_csv("cleaned/broadband_cleaned_data.csv", show_col_types = FALSE)
crime_data = read_csv("cleaned/crime_clean_data.csv", show_col_types = FALSE)
population_data = read_csv("cleaned/cleaned_population_data.csv", show_col_types = FALSE)
school_data = read_csv("cleaned/school_clean_data.csv", show_col_types = FALSE)

# towns with >= 300 sales, dominant county each
town_tab = cleaned_house_price_data %>% count(Town, County) %>%
  group_by(Town) %>% mutate(total = sum(n)) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>% ungroup() %>% filter(total >= 300)
pt = cleaned_house_price_data %>% select(-County) %>%
  inner_join(town_tab %>% select(Town, County), by = "Town")

th_x = theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))

# 1-2. Box plots of 2024 prices per county (log scale)
for (co in c("Norfolk","Suffolk")) {
  p = pt %>% filter(County == co, Year == 2024) %>%
    ggplot(aes(Town, Price)) +
    geom_boxplot(fill = ifelse(co=="Norfolk","lightblue","lightgreen"),
                 colour = ifelse(co=="Norfolk","navy","darkgreen"), outlier.size=.4) +
    scale_y_log10(labels = comma) + th_x +
    labs(title = sprintf("Box Plot of House Prices Across Towns in %s for the Year 2024", co),
         x = "Town", y = "Price (\u00a3)")
  ggsave(sprintf("graphs/%s_house_prices_boxplot.png", tolower(co)), p, width=8.6, height=4.6)
}

# 3-4. Average price bar charts
for (co in c("Norfolk","Suffolk")) {
  p = pt %>% filter(County == co, Year == 2024) %>% group_by(Town) %>%
    summarise(m = mean(Price)) %>%
    ggplot(aes(Town, m)) + geom_col(fill = ifelse(co=="Norfolk","blue","red")) + th_x +
    scale_y_continuous(labels = comma) +
    labs(title = sprintf("Average House Prices by Town in %s (2024)", co),
         x = "Town", y = "Average Price (\u00a3)")
  ggsave(sprintf("graphs/%s_avg_price_barchart.png", tolower(co)), p, width=8.6, height=4.4)
}

# 5. Monthly price trend by county
p = cleaned_house_price_data %>% mutate(MonthD = floor_date(Date_of_Transfer, "month")) %>%
  group_by(County, MonthD) %>% summarise(m = mean(Price), .groups="drop") %>%
  ggplot(aes(MonthD, m, colour = County)) + geom_line(linewidth=.5) + geom_point(size=1) +
  scale_y_continuous(labels = comma) +
  labs(title = "Monthly Average House Prices in Norfolk and Suffolk",
       x = "Month", y = "Average Price (\u00a3)")
ggsave("graphs/avg_price_line_chart.png", p, width=8.6, height=4.4)

# 6-7. Broadband avg/max dodged bars per county
bbt = broadband_data %>%
  inner_join(cleaned_house_price_data %>% distinct(Postcode, Town), by = "Postcode") %>%
  inner_join(town_tab %>% select(Town, County), by = "Town")
for (co in c("Norfolk","Suffolk")) {
  p = bbt %>% filter(County == co) %>% group_by(Town) %>%
    summarise(average = mean(`Average download speed (Mbit/s)`),
              maximum = mean(`Maximum download speed (Mbit/s)`)) %>%
    pivot_longer(-Town, names_to = "Metric") %>%
    ggplot(aes(Town, value, fill = Metric)) + geom_col(position = "dodge") + th_x +
    labs(title = sprintf("Average and Maximum Download Speed in %s", co),
         x = "Town", y = "Download Speed (Mbit/s)")
  ggsave(sprintf("graphs/%s_download_speed_barchart.png", tolower(co)), p, width=8.6, height=4.5)
}

# 8. Drug crimes 2024 boxplot by county
p = crime_data %>% filter(`Crime type` == "Drugs", str_starts(Month, "2024")) %>%
  count(County, Month) %>%
  ggplot(aes(n, County, fill = County)) + geom_boxplot(alpha=.6, show.legend = FALSE) +
  labs(title = "Boxplot of Monthly Drug Crimes in 2024 by County",
       x = "Count of Drug Crimes", y = "County")
ggsave("graphs/drug_boxplot_2024.png", p, width=7.6, height=3.9)

# 9. Vehicle crime radar - main town of each district, April 2024
main12 = cleaned_house_price_data %>% count(District, Town) %>% group_by(District) %>%
  slice_max(n, n=1, with_ties=FALSE) %>% ungroup() %>% select(District, Town)
vr = crime_data %>% filter(`Crime type`=="Vehicle crime", Month=="2024-04") %>%
  count(District) %>% inner_join(main12, by = "District") %>%
  group_by(Town) %>% summarise(n = sum(n), .groups="drop") %>%
  mutate(pct = n/max(n)*100) %>% arrange(Town)
vr2 = rbind(vr, vr[1,]); ang = seq(0, 2*pi, length.out = nrow(vr2))
png("graphs/vehicle_crime_radar.png", width=1150, height=900, res=150)
par(mar=c(1,1,3,1))
plot(NA, xlim=c(-1.35,1.35), ylim=c(-1.3,1.35), axes=FALSE, xlab="", ylab="",
     main="Vehicle Crime in April 2024")
for (r in c(.25,.5,.75,1)) { lines(r*cos(ang), r*sin(ang), lty=3, col="blue")
  text(0.06, r+0.03, paste0(r*100," (%)"), col="blue", cex=.7)}
segments(0,0,cos(ang[-length(ang)]),sin(ang[-length(ang)]),lty=3,col="blue")
polygon(vr2$pct/100*cos(ang), vr2$pct/100*sin(ang),
        col=adjustcolor("lightblue",.5), border="blue")
text(1.18*cos(ang[-length(ang)]), 1.18*sin(ang[-length(ang)]), vr$Town, cex=.55)
dev.off()

# 10. Robbery pie, April 2024
rb = crime_data %>% filter(`Crime type`=="Robbery", Month=="2024-04") %>% count(County) %>%
  mutate(lab = paste0(County, " (", round(n/sum(n)*100,1), "%)"))
p = ggplot(rb, aes("", n, fill = County)) + geom_col(width=1, colour="white") +
  coord_polar(theta="y") +
  geom_text(aes(label = lab), position = position_stack(vjust=.5), size=3.4) +
  labs(title = "Robbery Rate by County - April 2024") + theme_void() +
  theme(plot.title = element_text(face="bold", hjust=.5))
ggsave("graphs/robbery_pie_chart.png", p, width=6.4, height=4.4)

# 11. Drug offence rate per 10,000 trend (sector population -> county)
sector_county = cleaned_house_price_data %>%
  mutate(Sector = sub("^(\\S+\\s\\d).*$","\\1",Postcode)) %>% distinct(Sector, County)
county_pop = sector_county %>% inner_join(population_data, by = "Sector") %>%
  distinct(Sector, County, Population) %>% group_by(County) %>%
  summarise(Population = sum(Population))
p = crime_data %>% filter(`Crime type`=="Drugs") %>% count(County, Month) %>%
  inner_join(county_pop, by = "County") %>%
  mutate(rate = n/Population*10000, m = ym(Month)) %>%
  ggplot(aes(m, rate, colour = County)) + geom_line(linewidth=.5) + geom_point(size=1) +
  labs(title = "Drug Offence Rate per 10,000 People (2023-2026)",
       x = "Month", y = "Crime Rate (per 10,000)")
ggsave("graphs/drug_offence_rate_line_chart.png", p, width=8.6, height=4.3)

# 12-15. School Attainment 8: box + line per county (latest year)
latest = max(school_data$Academic_Year)
for (co in c("Norfolk","Suffolk")) {
  p = school_data %>% filter(County == co, Academic_Year == latest) %>%
    ggplot(aes(Town, average_att8scr)) + geom_boxplot(colour = "darkblue") + th_x +
    labs(title = sprintf("Box-Plot for average attainment 8 score of academic year (%s) in %s", latest, co),
         x = "Town", y = "Average Attainment 8 Score")
  ggsave(sprintf("graphs/%s_att8scr_boxplot.png", tolower(co)), p, width=8.6, height=4.3)
  p = school_data %>% filter(County == co) %>% group_by(Town) %>%
    summarise(m = mean(average_att8scr)) %>%
    ggplot(aes(Town, m, group = 1)) + geom_line(colour="blue") +
    geom_point(colour="red", size=1.2) + th_x +
    labs(title = sprintf("Line Chart of Average Attainment 8 Score by Town in %s", co),
         x = "Town", y = "Average Attainment 8 Score")
  ggsave(sprintf("graphs/%s_att8scr_line_chart.png", tolower(co)), p, width=8.6, height=4.3)
}
