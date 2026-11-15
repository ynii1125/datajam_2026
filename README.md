# DataJam 2026 – Airbnb Vancouver: $10M Short-Term Rental Portfolio

R analysis estimating revenue, costs and net yield for Vancouver short-term rental
segments (neighbourhood × property type × bedrooms), then building three $10M
portfolio options with occupancy and fee sensitivity tests.

## How to run
1. Install R packages:
   `install.packages(c("tidyverse", "lubridate", "sf", "VancouvR"))`
2. Download `listings.csv` and `calendar.csv` (Vancouver, 15 June 2026) from
   https://insideairbnb.com/get-the-data/ and put them in `data/raw/`.
3. Open `DataJam_2026.Rproj` in RStudio and run `datajam2026.R` (click **Source**).
   City of Vancouver property tax and address data download automatically on first run.
4. Results are written to `output/`.

## Key outputs
- `portfolio_options.csv`: the three portfolio options compared
- `portfolio_holdings.csv`: properties bought in each option
- `segments.csv`: revenue, cost and yield for every segment
- `sensitivity.csv`, `platform_fee_sensitivity.csv`: stress tests
- `assumptions.csv`, `cost_sources.csv`: all assumptions and sources

## Data
Inside Airbnb (CC BY 4.0); City of Vancouver Open Data.
Simulation only: assumes the case waives the principal-residence rule.
