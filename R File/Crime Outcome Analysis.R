# Crime Outcome Analysis.R
# ST5014CEM - Asim Ghimire (240330)
#
# How likely is a recorded crime to be solved? Buckets each concluded crime by
# its last recorded outcome and compares the solved share across the twelve
# districts. Districts come straight from the LSOA name, the same rule used in
# Cleaning.R, so no extra lookup file is needed.

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

setwd("C:/Users/asimg/Downloads/Data Science Codes Files")

# STEP 1 - LOAD the street-level crime files
crime_files <- list.files("data/crime", pattern = "\\.csv$",
                          recursive = TRUE, full.names = TRUE)
crime <- read_csv(crime_files,
                  col_select = c(Month, `LSOA name`, `Crime type`,
                                 `Last outcome category`),
                  show_col_types = FALSE)
cat("Loaded", nrow(crime), "rows\n")

# STEP 2 - FILTERS (in order, reporting rows remaining after each)

# 2a) Mature cases only (>= 12 months old), so outcomes have had time to land.
crime <- crime %>% filter(Month <= "2025-05")
cat("After 2a (mature window):", nrow(crime), "rows\n")

# 2b) Drop anti-social behaviour - the police never record outcomes for it.
crime <- crime %>% filter(`Crime type` != "Anti-social behaviour")
cat("After 2b (drop ASB):", nrow(crime), "rows\n")

# 2c) District from the LSOA name (as in Cleaning.R); keep the study area only.
study_districts <- c("Breckland", "Broadland", "Great Yarmouth",
                     "King's Lynn and West Norfolk", "North Norfolk",
                     "Norwich", "South Norfolk",
                     "Babergh", "East Suffolk", "Ipswich",
                     "Mid Suffolk", "West Suffolk")
crime <- crime %>%
  mutate(District = str_remove(`LSOA name`, " \\d+[A-Z]?$")) %>%
  filter(District %in% study_districts)
cat("After 2c (12 study districts):", nrow(crime), "rows\n")

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

unmatched <- crime %>% filter(is.na(bucket)) %>%
  distinct(`Last outcome category`) %>% pull()
if (length(unmatched) > 0) {
  stop("Unmatched 'Last outcome category' values:\n  ",
       paste(unmatched, collapse = "\n  "))
}

# STEP 4 - DISTRICT TABLE of concluded-crime outcome shares
bucket_levels <- c("Charged / court", "Resolved out of court",
                   "Suspect known, no action", "No suspect identified")

concluded <- crime %>% filter(bucket != "Unknown/ongoing")
cat("Concluded cases:", nrow(concluded), "\n\n")

district_table <- concluded %>%
  count(District, bucket) %>%
  group_by(District) %>%
  mutate(total = sum(n),
         pct   = round(n / total * 100, 1)) %>%
  ungroup() %>%
  select(District, total, bucket, pct) %>%
  pivot_wider(names_from = bucket, values_from = pct, values_fill = 0) %>%
  mutate(solved_pct = `Charged / court` + `Resolved out of court`) %>%
  select(District, total, all_of(bucket_levels), solved_pct) %>%
  arrange(desc(solved_pct))

print(district_table, n = 15)
cat("\nOverall two-county solved rate:",
    round(mean(concluded$bucket %in% bucket_levels[1:2]) * 100, 1), "%\n")

# STEP 5 - CHART: horizontal 100% stacked bar of outcomes by district
bucket_colours <- c("Charged / court"          = "#1b7837",
                    "Resolved out of court"    = "#7fbf7b",
                    "Suspect known, no action" = "#fdb863",
                    "No suspect identified"    = "#d7191c")

plot_data <- concluded %>%
  count(District, bucket) %>%
  mutate(District = factor(District, levels = rev(district_table$District)),
         bucket   = factor(bucket, levels = bucket_levels))

ggplot(plot_data, aes(x = n, y = District, fill = bucket)) +
  geom_col(position = position_fill(reverse = TRUE)) +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = c("0", "25", "50", "75", "100")) +
  scale_fill_manual(values = bucket_colours) +
  labs(title = "Crime case outcomes by district (Jun 2023 - May 2025)",
       x = "% of concluded crimes", y = NULL, fill = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom")

ggsave("graphs/crime_outcomes_by_district.png", width = 6.25, height = 4.2,
       units = "in", dpi = 300)