# Recommendation System.R - weighted 0-10 ratings and top towns (run after Linear Model.R)
library(tidyverse)

town_data = read_csv("cleaned/town_data.csv", show_col_types = FALSE)

# The 2026 brief requires each characteristic scored in the range 0-10.
scale10 = function(x, invert = FALSE){
  s = (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * 10
  if (invert) 10 - s else s }

# Weights mirror the investors' stated priorities (affordability first).
weights = c(afford = 0.35, safety = 0.25, speed = 0.25, school = 0.15)

town_rating = town_data %>% mutate(
  `Affordability Rating`       = scale10(average_house_price,  invert = TRUE),
  `Connectivity Rating`        = scale10(avg_download_speed),
  `Safety and Security Rating` = scale10(crime_rate_per_1000,  invert = TRUE),
  `School Academic Rating`     = scale10(average_att8scr),
  `Overall Rating` = weights["afford"] * `Affordability Rating` +
                     weights["safety"] * `Safety and Security Rating` +
                     weights["speed"]  * `Connectivity Rating` +
                     weights["school"] * `School Academic Rating`) %>%
  select(Town, County, ends_with("Rating")) %>%
  arrange(desc(`Overall Rating`))

dir.create("Recommendation System", showWarnings = FALSE)
write.csv(town_rating, "Recommendation System/town_rating.csv", row.names = FALSE)

top_10_towns = town_rating %>% slice_head(n = 10)
print(top_10_towns)
cat("Top 3 towns:", paste(head(town_rating$Town, 3), collapse = ", "), "\n")

towns_long = top_10_towns %>%
  pivot_longer(cols = ends_with("Rating"), names_to = "Rating Type", values_to = "Rating")

ggplot(towns_long, aes(Town, Rating, fill = `Rating Type`)) +
  geom_col(position = "dodge") +
  labs(title = "Rating Comparison Across Top 10 Towns", x = "Town", y = "Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Recommendation System/rating_comparison_top10.png", width = 8.8, height = 4.6)
