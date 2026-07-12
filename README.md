# ST5014CEM Data Science Coursework - Asim Ghimire (240330)
Norfolk & Suffolk town recommendation system (ST5014CEM, Softwarica / Coventry).

## Repository structure (mirrors the module's expected layout)
- `R File/Cleaning.R` - cleans all five datasets and builds the 3NF SQLite database
- `R File/Graphs.R` - all EDA visualisations (saved to `graphs/`)
- `R File/Linear Model.R` - builds the town-level dataset and fits the six regressions
- `R File/Recommendation System.R` - 0-10 weighted ratings, top-10 table, top-3 towns
- `ST5014CEM_Report_AsimGhimire.docx` / final PDF - the report

## How to run
1. Place raw data under `data/` as described at the top of `Cleaning.R`
   (Price Paid 2021-2025, Ofcom broadband, monthly crime folders, school KS4 folders,
   sector population CSV, ONS postcode lookup).
2. Run the four scripts in order: Cleaning -> Graphs -> Linear Model -> Recommendation System.
3. Outputs: `cleaned/` CSVs, `outputs/property_analysis.sqlite`, `graphs/` PNGs,
   `Recommendation System/` ratings and chart.

Do NOT commit the raw `data/` folder (hundreds of MB); commit code + outputs only.

## Data coverage
- **Price Paid**: 2021-2025, all five years complete (2025 covers January-December).
- **Crime**: 32 monthly releases, 2023-05 to 2025-12. police.uk serves a rolling archive,
  so months before 2023-05 are no longer downloadable.
- **School (KS4)**: academic years 2021-22, 2022-23 and 2023-24 for both counties. Suffolk
  also publishes 2024-25, but it is excluded so the county comparison is like-for-like.
- Schools are matched to towns by **postcode sector**, not exact postcode: a school's own
  postcode is rarely one that has had a house sale, so an exact join matched only 40% of
  schools and silently dropped 16 towns.

## Headline results
- 161,481 cleaned sales across 2 counties and 12 districts; 40 towns with >= 300 sales.
- 37 towns carry a complete set of price, broadband, crime and school measures.
- Only 2 of the 6 regressions are significant at the 5% level: house price ~ drug rate
  (R2 = 0.145, p = 0.020) and download speed ~ drug rate (R2 = 0.118, p = 0.037).
- **Top 3 towns: Norwich, Newmarket, Felixstowe.**
