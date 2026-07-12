# ST5014CEM Data Science for Developers
# Asim Ghimire (240330)
#
# Rates every town 0-10 on four characteristics, combines them into a weighted
# overall score, and ranks the towns to recommend where to invest.
#
# Run after Linear Model.R.

library(tidyverse)


setwd("C:/Users/asimg/Downloads/Data Science Codes Files")

dir.create("Recommendation System", showWarnings = FALSE)


# The same two brand colours used in Graphs.R and Linear Model.R.
CORAL <- "#F97068"
TEAL  <- "#00BFC9"


town_data <- read_csv("cleaned/town_data.csv", show_col_types = FALSE)


# ---------------------------------------------------------------------------
# Rating scale
# ---------------------------------------------------------------------------

# The brief requires every characteristic to be scored on a 0-10 scale.
# invert = TRUE is used where a LOWER raw value is better, so that a cheap town
# and a safe town both score highly.
scale10 <- function(x, invert = FALSE) {
  scaled <- (x - min(x, na.rm = TRUE)) /
            (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * 10

  if (invert) 10 - scaled else scaled
}

# The weights mirror the investors' stated priorities, affordability first.
weights <- c(afford = 0.35,
             safety = 0.25,
             speed  = 0.25,
             school = 0.15)


# ---------------------------------------------------------------------------
# Rate and rank the towns
# ---------------------------------------------------------------------------

town_rating <- town_data %>%
  mutate(`Affordability Rating`       = scale10(average_house_price, invert = TRUE),
         `Connectivity Rating`        = scale10(avg_download_speed),
         `Safety and Security Rating` = scale10(crime_rate_per_1000,  invert = TRUE),
         `School Academic Rating`     = scale10(average_att8scr),

         `Overall Rating` = weights["afford"] * `Affordability Rating` +
                            weights["safety"] * `Safety and Security Rating` +
                            weights["speed"]  * `Connectivity Rating` +
                            weights["school"] * `School Academic Rating`) %>%
  select(Town, County, ends_with("Rating")) %>%
  arrange(desc(`Overall Rating`))

write.csv(town_rating,
          "Recommendation System/town_rating.csv",
          row.names = FALSE)


# ---------------------------------------------------------------------------
# The recommendation
# ---------------------------------------------------------------------------

top_10_towns <- town_rating %>% slice_head(n = 10)

print(top_10_towns)

cat("\nTop 3 towns:", paste(head(town_rating$Town, 3), collapse = ", "), "\n")


# ---------------------------------------------------------------------------
# Rating comparison across the top 10 towns
# ---------------------------------------------------------------------------

# Drawn as small multiples - one panel per rating - rather than fifty dodged bars.
# The panel title carries the identity of each rating, so colour is freed from that
# job and only the two brand colours are needed: the overall score in teal, and the
# four components it is built from in coral.
rating_levels <- c("Overall Rating",
                   "Affordability Rating",
                   "Connectivity Rating",
                   "Safety and Security Rating",
                   "School Academic Rating")

towns_long <- top_10_towns %>%
  pivot_longer(cols      = ends_with("Rating"),
               names_to  = "Rating Type",
               values_to = "Rating") %>%
  mutate(
    # Highest overall rating at the top of every panel.
    Town          = factor(Town, levels = rev(top_10_towns$Town)),
    `Rating Type` = factor(`Rating Type`, levels = rating_levels),
    Role          = if_else(`Rating Type` == "Overall Rating", "Overall", "Component"))

plot <- ggplot(towns_long, aes(Rating, Town, fill = Role)) +
  geom_col(width = 0.72, show.legend = FALSE) +
  facet_wrap(~ `Rating Type`, ncol = 5) +
  scale_fill_manual(values = c(Component = CORAL, Overall = TEAL)) +
  scale_x_continuous(limits = c(0, 10),
                     breaks = c(0, 5, 10),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title    = "Rating Comparison Across Top 10 Towns",
       subtitle = "Towns ordered by overall rating. The overall score (teal) is the weighted combination of the four components (coral).",
       x        = "Rating (0-10)",
       y        = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        strip.text         = element_text(face = "bold", size = 8.5),
        plot.title         = element_text(face = "bold"),
        plot.subtitle      = element_text(size = 8.5, colour = "grey30"))

ggsave("Recommendation System/rating_comparison_top10.png",
       plot, width = 11.5, height = 4.6, dpi = 150)
