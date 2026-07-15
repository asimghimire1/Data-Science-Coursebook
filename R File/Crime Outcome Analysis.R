# Crime Outcome Analysis.R
# ST5014CEM - Asim Ghimire (240330)
#
# How likely is a crime to be solved, town by town? Buckets each concluded crime
# by its last recorded outcome and charts the solved share across the 37 towns.

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

setwd("C:/Users/asimg/Downloads/Data Science Codes Files")

lookup_path <- "data/PCD_OA21_LSOA21_MSOA21_LAD_MAY26_UK_LU.csv"



crime_files <- list.files("data/crime", pattern = "\\.csv$",
                          recursive = TRUE, full.names = TRUE)

crime <- read_csv(crime_files,
                  col_select = c(Month, `LSOA code`, `Crime type`,
                                 `Last outcome category`),
                  show_col_types = FALSE)

cat("Loaded", nrow(crime), "rows\n")



# STEP 2 - FILTERS (in order, reporting rows remaining after each)


# 2a) Keep mature cases only (>= 12 months old).
crime <- crime %>% filter(Month <= "2025-05")
cat("After 2a (Month <= 2025-05):", nrow(crime), "rows\n")

# 2b) Drop anti-social behaviour (it has no case outcome).
crime <- crime %>% filter(`Crime type` != "Anti-social behaviour")
cat("After 2b (drop Anti-social behaviour):", nrow(crime), "rows\n")

# 2c) Map each LSOA to a town, then keep only the 37 study towns.
#     The town of a postcode is its Price Paid post town; the town of an LSOA is
#     the most frequent town among that LSOA's postcodes (ties: alphabetically
#     first). Postcodes are normalised identically on both sides before joining.

# The 37 towns already used in Recommendation System.R.
towns_37 <- read_csv("cleaned/town_data.csv", show_col_types = FALSE)$Town

# Postcode -> town, one dominant town per postcode.
pc_town <- read_csv("cleaned/cleaned_house_prices.csv",
                    col_select = c(Postcode, Town), show_col_types = FALSE) %>%
  count(Postcode, Town) %>%
  group_by(Postcode) %>%
  arrange(desc(n), Town) %>%
  slice(1) %>%
  ungroup() %>%
  select(Postcode, Town)

# LSOA -> town, via the ONS lookup's postcodes.
lsoa_town <- read_csv(lookup_path,
                      col_select = c(pcds, lsoa21cd), show_col_types = FALSE) %>%
  mutate(pc       = str_remove_all(str_to_upper(pcds), "\\s"),
         Postcode = if_else(str_length(pc) >= 5,
                            str_c(str_sub(pc, 1, str_length(pc) - 3), " ",
                                  str_sub(pc, -3)),
                            pc)) %>%
  inner_join(pc_town, by = "Postcode") %>%
  count(lsoa21cd, Town) %>%
  group_by(lsoa21cd) %>%
  arrange(desc(n), Town) %>%
  slice(1) %>%
  ungroup() %>%
  select(lsoa21cd, Town)

crime <- crime %>%
  inner_join(lsoa_town, by = c("LSOA code" = "lsoa21cd")) %>%
  filter(Town %in% towns_37)
cat("After 2c (mapped to one of the 37 towns):", nrow(crime), "rows\n")



# STEP 3 - BUCKET the last outcome category (exact string match only)

bucket_map <- c(
  "Investigation complete; no suspect identified"       = "No suspect identified",
  "Unable to prosecute suspect"                         = "Suspect known, no action",
  "Formal action is not in the public interest"         = "Suspect known, no action",
  "Further action is not in the public interest"        = "Suspect known, no action",
  "Further investigation is not in the public interest" = "Suspect known, no action",
  "Local resolution"                                    = "Resolved out of court",
  "Offender given a caution"                            = "Resolved out of court",
  "Offender given a drugs possession warning"           = "Resolved out of court",
  "Offender given penalty notice"                       = "Resolved out of court",
  "Action to be taken by another organisation"          = "Resolved out of court",
  "Court result unavailable"                            = "Charged / court",
  "Awaiting court outcome"                              = "Charged / court",
  "Suspect charged as part of another case"             = "Charged / court",
  "__NA__"                                              = "Unknown/ongoing",
  "Under investigation"                                 = "Unknown/ongoing",
  "Status update unavailable"                           = "Unknown/ongoing")

crime <- crime %>%
  mutate(outcome_key = if_else(is.na(`Last outcome category`), "__NA__",
                               `Last outcome category`),
         bucket      = unname(bucket_map[outcome_key]))

# Any outcome value outside the mapping stops the script.
unmatched <- crime %>%
  filter(is.na(bucket)) %>%
  distinct(`Last outcome category`) %>%
  pull(`Last outcome category`)
if (length(unmatched) > 0) {
  stop("Unmatched 'Last outcome category' values:\n  ",
       paste(unmatched, collapse = "\n  "))
}



# STEP 4 - TOWN TABLE of concluded-crime outcome shares

bucket_levels <- c("Charged / court", "Resolved out of court",
                   "Suspect known, no action", "No suspect identified")

concluded <- crime %>% filter(bucket != "Unknown/ongoing")

town_table <- concluded %>%
  count(Town, bucket) %>%
  group_by(Town) %>%
  mutate(total = sum(n),
         pct   = round(n / total * 100, 1)) %>%
  ungroup() %>%
  select(Town, total, bucket, pct) %>%
  pivot_wider(names_from = bucket, values_from = pct, values_fill = 0) %>%
  mutate(solved_pct = `Charged / court` + `Resolved out of court`) %>%
  select(Town, total, all_of(bucket_levels), solved_pct) %>%
  arrange(desc(solved_pct))

print(town_table, n = 40)

overall_solved <- round(
  mean(concluded$bucket %in% c("Charged / court", "Resolved out of court")) * 100, 1)
cat("Overall two-county solved rate:", overall_solved, "%\n")



# STEP 5 - CHART: horizontal 100% stacked bar of outcomes by town

bucket_colours <- c("Charged / court"          = "#1b7837",
                    "Resolved out of court"    = "#7fbf7b",
                    "Suspect known, no action" = "#fdb863",
                    "No suspect identified"    = "#d7191c")

plot_data <- concluded %>%
  count(Town, bucket) %>%
  mutate(Town   = factor(Town, levels = rev(town_table$Town)),
         bucket = factor(bucket, levels = bucket_levels))

ggplot(plot_data, aes(x = n, y = Town, fill = bucket)) +
  geom_col(position = position_fill(reverse = TRUE)) +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = c("0", "25", "50", "75", "100")) +
  scale_fill_manual(values = bucket_colours) +
  labs(title = "Crime case outcomes by town (Jun 2023 - May 2025)",
       x = "% of concluded crimes", y = NULL, fill = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom")

ggsave("crime_outcomes_by_town.png", width = 6.25, height = 6.8,
       units = "in", dpi = 300)
