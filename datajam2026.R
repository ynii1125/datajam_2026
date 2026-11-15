library(tidyverse)
library(lubridate)
library(sf)
library(VancouvR)

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)
options(timeout = 900)


A <- list(
  budget            = 10e6,
  reserve_months    = 3,      # Change to 6 and rerun for the six-month scenario
  snapshot_date     = ymd("2026-06-15"),
  review_rate       = 0.50,  
  avg_stay_nights   = 3,     
  max_occupancy     = 0.70,   
  platform_fee      = 0.155, # S7: single-fee base case; hold nightly prices fixed
  management_fee    = 0.20,   
  strata_fee_month  = 500,   
  utilities_month   = 250,    
  insurance_condo   = 1500,  
  insurance_house   = 3000,  
  maintenance_condo = 0.005,  
  maintenance_house = 0.010,
  city_licence      = 1108,   # S5: current published fee; 2027 proxy, not guaranteed
  provincial_fee    = 450,    # S6: non-principal-residence simulation category
  registry_service  = 1.50,   # S6
  application_fee   = 77,     # S5: one-time municipal application, per unit  
  min_listings      = 8,      
  max_per_segment   = 3     
)

# Simulation-only costing: principal-residence restriction is waived by the case.
# Fee budgets do NOT establish real-world eligibility for a corporate portfolio.
A$licence_fees <- A$city_licence + A$provincial_fee + A$registry_service

save_cov <- function(x, path) {            
  if (inherits(x, "sf")) x <- st_drop_geometry(x)
  x <- as.data.frame(x)
  x <- x[, !sapply(x, is.list), drop = FALSE]
  write.csv(x, path, row.names = FALSE)
}


need <- c("listings.csv", "calendar.csv")
missing <- need[!file.exists(file.path("data/raw", need))]
if (length(missing) > 0) stop("Missing in data/raw/: ", paste(missing, collapse = ", "),
                              "\nDownload from https://insideairbnb.com/get-the-data/ (Vancouver, 15 June 2026).")


if (!file.exists("data/raw/property_tax_2026.csv")) {
  message("Downloading property tax report (a few minutes) ...")
  tax_dl <- get_cov_data("property-tax-report", where = "tax_assessment_year = '2026'")
  save_cov(tax_dl, "data/raw/property_tax_2026.csv")
}


if (!file.exists("data/raw/property_addresses.csv")) {
  message("Downloading property addresses (a few minutes) ...")
  addr_dl <- get_cov_data("property-addresses")
  save_cov(addr_dl, "data/raw/property_addresses.csv")
}


listings_raw <- read_csv("data/raw/listings.csv", show_col_types = FALSE,
                         guess_max = 10000)

listings <- listings_raw %>%
  mutate(price       = parse_number(as.character(price)),   # "$1,250.00" -> 1250
         last_review = as_date(last_review),
         bedrooms    = as.numeric(bedrooms))


key_cols <- c("price", "bedrooms", "room_type", "property_type",
              "neighbourhood_cleansed", "minimum_nights",
              "number_of_reviews_ltm", "last_review", "license")
data_quality <- listings %>%
  summarise(across(any_of(key_cols), ~ round(mean(is.na(.)), 3))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "share_missing")
write_csv(data_quality, "output/data_quality.csv")


step1 <- listings %>% filter(minimum_nights < 30)
step2 <- step1 %>% filter(!is.na(last_review),
                          last_review >= A$snapshot_date - years(1))
step3 <- step2 %>% filter(!is.na(price), price > 0)
bounds <- quantile(step3$price, c(0.01, 0.99))
step4 <- step3 %>% filter(between(price, bounds[1], bounds[2]))

active <- step4 %>%
  filter(room_type == "Entire home/apt") %>%
  mutate(unit_type = case_when(
    str_detect(property_type, regex("^Entire (home|villa|bungalow|cottage)", TRUE)) ~ "House",
    str_detect(property_type, regex("^Entire (condo|rental unit|loft|serviced apartment|townhouse)", TRUE)) ~ "Condo/Apt",
    TRUE ~ NA_character_)) %>%
  filter(!is.na(unit_type), !is.na(bedrooms)) %>%
  mutate(beds = case_when(
    bedrooms == 0 ~ "Studio",
    bedrooms == 1 ~ "1BR",
    bedrooms == 2 ~ "2BR",
    bedrooms >= 3 ~ "3BR+",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(beds))

stage <- function(name, d) tibble(stage = name, n = nrow(d),
                                  median_price = median(d$price, na.rm = TRUE),
                                  median_reviews_ltm = median(d$number_of_reviews_ltm, na.rm = TRUE))
filter_impact <- bind_rows(
  stage("0 All listings", listings),
  stage("1 Short-term only (<30 nights)", step1),
  stage("2 Reviewed in last 12 months", step2),
  stage("3 Has a price", step3),
  stage("4 Price outliers removed (1%-99%)", step4),
  stage("5 Entire homes in modelled categories", active))
write_csv(filter_impact, "output/decision1_filter_impact.csv")
print(filter_impact)


active <- active %>%
  mutate(nights  = pmin(number_of_reviews_ltm / A$review_rate *
                          pmax(A$avg_stay_nights, minimum_nights),
                        365 * A$max_occupancy),
         revenue = nights * price)


validation <- tibble(check = "Our median revenue per listing",
                     value = median(active$revenue))
if ("estimated_revenue_l365d" %in% names(active)) {
  ia <- parse_number(as.character(active$estimated_revenue_l365d))
  validation <- bind_rows(validation,
                          tibble(check = "Inside Airbnb median estimated revenue", value = median(ia, na.rm = TRUE)),
                          tibble(check = "Correlation between the two", value = cor(active$revenue, ia, use = "complete.obs")))
}


calendar <- read_csv("data/raw/calendar.csv", show_col_types = FALSE,
                     col_select = c(listing_id, date, available)) %>%
  filter(listing_id %in% active$id) %>%
  mutate(date = as_date(date),
         unavailable = as.character(available) %in% c("f", "FALSE"))

validation <- bind_rows(validation,
                        tibble(check = "Review-based occupancy (median)", value = median(active$nights) / 365),
                        tibble(check = "Calendar share unavailable (upper bound)", value = mean(calendar$unavailable)))
write_csv(validation, "output/validation_revenue.csv")
print(validation)

rm(calendar)


norm <- function(x) gsub("[^a-z]", "", tolower(x))       # "Dunbar-Southlands" = "Dunbar Southlands"
key  <- function(x) str_remove(as.character(x), "^0+")   # same ID format in both files

tax  <- read_csv("data/raw/property_tax_2026.csv", show_col_types = FALSE, guess_max = 50000)
addr <- read_csv("data/raw/property_addresses.csv", show_col_types = FALSE, guess_max = 50000)

pc_col   <- grep("pcoord", names(addr), value = TRUE, ignore.case = TRUE)[1]
area_col <- grep("local_area", names(addr), value = TRUE, ignore.case = TRUE)[1]
if (is.na(pc_col) || is.na(area_col) || !"land_coordinate" %in% names(tax)) {
  cat("tax columns:\n");  print(names(tax))
  cat("addr columns:\n"); print(names(addr))
  stop("Column names differ from what this script expects - send a screenshot of the lists above.")
}

area_lookup <- addr %>%
  transmute(k = key(.data[[pc_col]]), area = .data[[area_col]]) %>%
  filter(!is.na(area)) %>% distinct(k, .keep_all = TRUE)

homes <- tax %>%
  mutate(k = key(land_coordinate),
         value = current_land_value + current_improvement_value) %>%
  inner_join(area_lookup, by = "k") %>%
  filter(!is.na(value), current_improvement_value > 0,
         str_detect(zoning_classification,
                    regex("dwelling|residential|comprehensive", TRUE))) %>%
  mutate(unit_type = case_when(legal_type == "STRATA" ~ "Condo/Apt",
                               legal_type == "LAND"   ~ "House",
                               TRUE ~ NA_character_)) %>%
  filter(!is.na(unit_type),
         (unit_type == "Condo/Apt" & between(value, 250e3, 5e6)) |   # drops parking stalls, lockers
           (unit_type == "House"     & between(value, 800e3, 10e6)))   # drops towers, vacant lots
message("Tax records matched to a local area: ", nrow(homes))


price_q <- tribble(~unit_type,  ~beds,    ~q,
                   "Condo/Apt", "Studio", 0.15,
                   "Condo/Apt", "1BR",    0.35,
                   "Condo/Apt", "2BR",    0.65,
                   "Condo/Apt", "3BR+",   0.90,
                   "House",     "Studio", 0.10,
                   "House",     "1BR",    0.20,
                   "House",     "2BR",    0.40,
                   "House",     "3BR+",   0.60)

prices <- homes %>%
  mutate(area_key = norm(area)) %>%
  group_by(area_key, unit_type) %>% filter(n() >= 30) %>% ungroup() %>%
  inner_join(price_q, by = "unit_type", relationship = "many-to-many") %>%
  group_by(area_key, unit_type, beds, q) %>%
  summarise(n_assessed = n(),
            unit_price = quantile(value, q[1]),
            tax_levy   = quantile(tax_levy, q[1], na.rm = TRUE),
            .groups = "drop")


# S8: general PTT + further 2% on fully residential value above $3M.
# Purchase price proxies fair market value. No exemptions assumed.
ptt <- function(p) {
  0.01 * pmin(p, 2e5) +
    0.02 * pmax(pmin(p, 2e6) - 2e5, 0) +
    0.03 * pmax(p - 2e6, 0) +
    0.02 * pmax(p - 3e6, 0)
}

net_income <- function(rev, price, type, levy) {
  rev * (1 - A$platform_fee - A$management_fee) -
    levy - A$licence_fees - 12 * A$utilities_month -
    if_else(type == "House",
            A$insurance_house + A$maintenance_house * price,
            A$insurance_condo + A$maintenance_condo * price + 12 * A$strata_fee_month)
}

# User-supplied asking prices: midpoints are scenario assumptions, not sold prices.
asking_prices <- tribble(
  ~neighbourhood, ~unit_type, ~beds, ~asking_low, ~asking_high,
  "Strathcona", "Condo/Apt", "1BR", 499900, 599900,
  "Downtown", "Condo/Apt", "Studio", 409900, 449999,
  "Downtown", "Condo/Apt", "1BR", 599000, 619000,
  "Downtown", "Condo/Apt", "2BR", 858800, 895000,
  "West End", "Condo/Apt", "2BR", 748800, 929000,
  "Grandview-Woodland", "Condo/Apt", "Studio", 479900, 479900,
  "Grandview-Woodland", "Condo/Apt", "1BR", 528000, 669000
) %>% mutate(asking_price_base = (asking_low + asking_high) / 2)

# User bundles: furniture totals already include delivery/assembly, cookware
# and supplies; do not add those again. Rekeying is excluded.
# legal_budget: S1 cash-conveyance benchmark + variable-document allowance
# + S2 inspection-by-assumed-size, rounded upward as a budget.
# cleaning_base: S3 move-in/out STARTING prices; apply 5% GST once.
# photo_budget: S4 condo $180 / house $220 public base, rounded tax buffer.
# repair_budget: analytical assumptions, NOT contractor quotes.
startup_costs <- tribble(
  ~unit_type, ~cost_beds, ~furnishing_cost, ~legal_budget, ~cleaning_base, ~photo_budget, ~repair_budget,
  "Condo/Apt", "Studio",  869.02, 2100, 228.00, 200,  500,
  "Condo/Apt", "1BR",     869.02, 2100, 258.00, 200,  500,
  "Condo/Apt", "2BR",    1425.57, 2100, 318.50, 200,  750,
  "Condo/Apt", "3BR",    2038.11, 2100, 371.00, 200, 1000,
  "Condo/Apt", "4BR",    2594.66, 2150, 462.00, 200, 1500,
  "House",     "1BR",     869.02, 2200, 258.00, 250, 1000,
  "House",     "2BR",    1425.57, 2200, 349.00, 250, 1250,
  "House",     "3BR",    2038.11, 2200, 371.00, 250, 1500,
  "House",     "4BR",    2594.66, 2250, 462.00, 250, 2000
) %>%
  mutate(cleaning_cost = round(cleaning_base * 1.05, 2),
         one_time_cost = legal_budget + cleaning_cost + photo_budget +
           repair_budget + A$application_fee,
         setup_including_furniture = furnishing_cost + one_time_cost)
# Size assumptions: condo inspection Studio-2BR <1000 sqft, 3BR 1001-2000,
# 4BR 2001-3000; house 1BR-3BR <2000, 4BR 2001-3000.
# Cleaning: Studio <600 sqft/1 bath; 1BR 600-1199/1 bath;
# condo 2BR 600-1199/2 bath; house 2BR 1200-1999/2 bath;
# 3BR 1200-1999/2 bath; 4BR 2000-2499/3 bath. Adjust for actual property.
# Larger houses (photography >2500 sqft) and 5+ bedrooms need new quotes.
write_csv(startup_costs, "output/startup_cost_assumptions.csv")

# Preserve the original 3BR+ revenue/price segments. Within that group,
# weight setup costs by the cleaned sample's 3-bedroom / 4+-bedroom mix.
# For 5+ bedrooms, 4BR costs are a proxy and can understate actual costs.
active_costs <- active %>%
  mutate(cost_beds = case_when(
    bedrooms == 0 ~ "Studio", bedrooms == 1 ~ "1BR",
    bedrooms == 2 ~ "2BR", bedrooms == 3 ~ "3BR",
    bedrooms >= 4 ~ "4BR", TRUE ~ NA_character_
  )) %>%
  left_join(startup_costs, by = c("unit_type", "cost_beds"))
missing_costs <- active_costs %>%
  filter(is.na(furnishing_cost) | is.na(one_time_cost)) %>%
  count(unit_type, cost_beds)
if (nrow(missing_costs) > 0) {
  print(missing_costs)
  stop("Missing startup-cost assumptions. Add costs; do not substitute zero.")
}
write_csv(active_costs %>% count(unit_type, beds, cost_beds),
          "output/startup_cost_mapping.csv")
segment_costs <- active_costs %>%
  group_by(neighbourhood = neighbourhood_cleansed, unit_type, beds) %>%
  summarise(furnishing_cost = mean(furnishing_cost),
            one_time_cost = mean(one_time_cost),
            share_5plus = mean(bedrooms >= 5), .groups = "drop")

segments <- active %>%
  group_by(neighbourhood = neighbourhood_cleansed, unit_type, beds) %>%
  summarise(n_listings  = n(),
            nightly     = median(price),
            nights      = median(nights),
            rev_low     = quantile(revenue, 0.25),
            rev_mid     = median(revenue),
            rev_high    = quantile(revenue, 0.75),
            share_license_provided = mean(!is.na(license) & str_trim(license) != ""),   # ESTIMATE only
            .groups = "drop") %>%
  filter(n_listings >= A$min_listings) %>%
  mutate(area_key = norm(neighbourhood)) %>%
  left_join(prices, by = c("area_key", "unit_type", "beds")) %>%
  mutate(unit_price_original = unit_price) %>%
  left_join(asking_prices, by = c("neighbourhood", "unit_type", "beds")) %>%
  mutate(unit_price = coalesce(asking_price_base, unit_price_original),
         price_source = if_else(!is.na(asking_price_base),
                                "Asking-price midpoint", "Assessment percentile")) %>%
  left_join(segment_costs, by = c("neighbourhood", "unit_type", "beds"))

unmatched <- segments %>% filter(is.na(unit_price)) %>% distinct(neighbourhood, unit_type)
if (nrow(unmatched) > 0) {
  message("Segments with no purchase price (dropped) - name these as a limitation:")
  print(unmatched)
}

segments <- segments %>%
  filter(!is.na(unit_price)) %>%
  mutate(all_in_cost = unit_price + ptt(unit_price),
         initial_cost = all_in_cost + furnishing_cost + one_time_cost,
         # Reserve uses recurring costs already charged in NOI; do not expense
         # the reserve again. This includes planned maintenance funding.
         monthly_required = A$utilities_month +
           if_else(unit_type == "House", 0, A$strata_fee_month) +
           (tax_levy + A$licence_fees +
              if_else(unit_type == "House", A$insurance_house, A$insurance_condo) +
              unit_price * if_else(unit_type == "House",
                                   A$maintenance_house, A$maintenance_condo)) / 12,
         reserve_per_unit = monthly_required * A$reserve_months,
         budget_per_unit = initial_cost + reserve_per_unit,
         net_low   = net_income(rev_low,  unit_price, unit_type, tax_levy),
         net_mid   = net_income(rev_mid,  unit_price, unit_type, tax_levy),
         net_high  = net_income(rev_high, unit_price, unit_type, tax_levy),
         gross_yield = rev_mid / initial_cost,
         purchase_net_yield = net_mid / all_in_cost,
         net_yield   = net_mid / initial_cost,
         rank_gross_revenue = min_rank(desc(rev_mid)),
         rank_net_yield     = min_rank(desc(net_yield))) %>%
  select(-area_key) %>% arrange(desc(net_yield))
write_csv(segments, "output/segments.csv")
print(segments %>% select(neighbourhood, unit_type, beds, n_listings, rev_mid,
                          unit_price, net_mid, net_yield), n = 20)


metric_impact <- segments %>%
  select(neighbourhood, unit_type, beds, rev_mid, unit_price, net_yield,
         rank_gross_revenue, rank_net_yield) %>%
  filter(rank_gross_revenue <= 5 | rank_net_yield <= 5) %>%
  arrange(rank_net_yield)
write_csv(metric_impact, "output/decision2_metric_impact.csv")


build <- function(seg, label, budget = A$budget, cap = A$max_per_segment) {
  seg <- seg %>%
    filter(is.finite(budget_per_unit), budget_per_unit > 0,
           is.finite(net_yield)) %>%
    arrange(desc(net_yield)) %>% mutate(units = 0L)
  left <- budget
  for (i in seq_len(nrow(seg))) {
    n <- min(cap, floor(left / seg$budget_per_unit[i]))
    if (n > 0) { seg$units[i] <- as.integer(n); left <- left - n * seg$budget_per_unit[i] }
  }
  seg %>% filter(units > 0) %>% mutate(option = label, .before = 1)
}

cut_price <- median(segments$unit_price)
holdings <- bind_rows(
  build(filter(segments, unit_price <= cut_price), "A Many modest"),
  build(filter(segments, unit_price >  cut_price), "B Few high-value", cap = 2),
  build(segments,                                  "C Best yield, any size"))
write_csv(holdings %>% select(option, neighbourhood, unit_type, beds, units,
                              unit_price_original, unit_price, price_source,
                              all_in_cost, furnishing_cost, one_time_cost,
                              initial_cost, monthly_required, reserve_per_unit, budget_per_unit,
                              rev_mid, net_mid, net_yield),
          "output/portfolio_holdings.csv")

options_tbl <- holdings %>%
  group_by(option) %>%
  summarise(properties      = sum(units),
            neighbourhoods  = n_distinct(neighbourhood),
            purchase_cost   = sum(units * all_in_cost),
            furniture_total = sum(units * furnishing_cost),
            one_time_total  = sum(units * one_time_cost),
            capital_used    = sum(units * initial_cost),
            reserve_total   = sum(units * reserve_per_unit),
            net_income_low  = sum(units * net_low),
            net_income_2027 = sum(units * net_mid),
            net_income_high = sum(units * net_high),
            .groups = "drop") %>%
  mutate(cash_left      = A$budget - capital_used,
         cash_unallocated = cash_left - reserve_total,
         total_budget_committed = capital_used + reserve_total,
         reserve_months = A$reserve_months,
         yield_low      = net_income_low  / A$budget,   # on the full $10M
         yield_expected = net_income_2027 / A$budget,
         yield_high     = net_income_high / A$budget)
write_csv(options_tbl, "output/portfolio_options.csv")
print(options_tbl, width = Inf)
stopifnot(all(options_tbl$cash_unallocated >= -0.01))
# Cash left includes the reserved cash. Reserve is NOT an operating expense.
# net_yield = NOI / initial deployed capital (purchase, furniture, startup).
# Reserve is not an expense. Portfolio yields remain NOI / the full $10M.
write_csv(options_tbl, paste0("output/portfolio_options_reserve_",
                              A$reserve_months, "m.csv"))


sensitivity <- crossing(holdings, occupancy_shock = c(0, -0.10, -0.20)) %>%
  mutate(net = net_income(rev_mid * (1 + occupancy_shock), unit_price, unit_type, tax_levy)) %>%
  group_by(option, occupancy_shock) %>%
  summarise(net_income = sum(units * net), .groups = "drop") %>%
  mutate(yield_on_10M = net_income / A$budget)
write_csv(sensitivity, "output/sensitivity.csv")
print(sensitivity)

# S7: compare fee structures at fixed nightly prices and fixed holdings.
# Do not assume prices can rise enough to offset the higher service fee.
fee_sensitivity <- crossing(holdings, platform_fee_case = c(0.03, 0.155)) %>%
  mutate(net = net_mid + rev_mid * (A$platform_fee - platform_fee_case)) %>%
  group_by(option, platform_fee_case) %>%
  summarise(net_income = sum(units * net), .groups = "drop") %>%
  mutate(yield_on_10M = net_income / A$budget)
write_csv(fee_sensitivity, "output/platform_fee_sensitivity.csv")

credit <- "Data: Inside Airbnb, Vancouver, 15 June 2026, CC BY 4.0; City of Vancouver Open Data."

ggsave("output/chart_yield_by_segment.png", width = 9, height = 6, dpi = 200, plot =
         segments %>% slice_max(net_yield, n = 15) %>%
         mutate(label = fct_reorder(paste(neighbourhood, beds, unit_type, sep = " | "), net_yield)) %>%
         ggplot(aes(net_yield, label)) +
         geom_col(fill = "#2a6fb0") +
         scale_x_continuous(labels = scales::percent) +
         labs(title = "Estimated net yield by segment (top 15)",
              subtitle = "Net operating income / initial deployed capital, all-cash",
              x = "Net yield", y = NULL, caption = credit) +
         theme_minimal())

ggsave("output/chart_portfolios.png", width = 8, height = 4.5, dpi = 200, plot =
         ggplot(options_tbl, aes(option, yield_expected)) +
         geom_col(fill = "#2a6fb0", width = 0.6) +
         geom_errorbar(aes(ymin = yield_low, ymax = yield_high), width = 0.2) +
         geom_text(aes(y = 0, label = paste(properties, "properties")), vjust = -0.6, colour = "white") +
         scale_y_continuous(labels = scales::percent) +
         labs(title = "Expected 2027 net yield on $10M, by portfolio option",
              subtitle = "Bars: median-revenue case. Whiskers: 25th to 75th percentile revenue",
              x = NULL, y = "Net yield on $10M", caption = credit) +
         theme_minimal())

write_csv(enframe(A, "assumption", "value") %>%
            mutate(value = map_chr(value, as.character)) %>%
            bind_rows(price_q %>% transmute(
              assumption = paste("price percentile:", unit_type, beds),
              value = as.character(q))),
          "output/assumptions.csv")


# Sources and remaining assumptions; public fee schedules checked 2026-11-14.
cost_sources <- tribble(
  ~source_id, ~item, ~url_or_basis, ~notes,
  "S1", "Cash purchase legal fee", "https://www.tsangnotary.com/pricing-guidelines.html", "Published approximate cash-conveyance benchmark; variable disbursements extra.",
  "S2", "Inspection size tiers", "https://www.ballardinspections.ca/pricing/", "Published inspection base + 5% GST; bedroom-to-area mapping is an assumption.",
  "S3", "Initial move-in cleaning", "https://cleaninghive.ca/vancouver/move-in-out-cleaning/", "Starting prices + 5% GST; actual area/bathrooms/condition may change quote.",
  "S4", "Photography", "https://revstudio.ca/pricing/", "Public base prices; 200/250 inclusive budgets are model allowances.",
  "S5", "Municipal licence/application", "https://vancouver.ca/doing-business/short-term-rentals.aspx", "Published 1108 annual / 77 application; current fee used as 2027 proxy.",
  "S6", "Provincial host registration", "https://www2.gov.bc.ca/gov/content/housing-tenancy/short-term-rentals/registry/host-registration", "450 annual + 1.50 service for non-principal-residence simulation category.",
  "S7", "Airbnb service fee", "https://www.airbnb.ca/help/article/1857", "15.5% single-fee baseline; 3% comparison only; hold nightly prices fixed.",
  "S8", "Property transfer tax", "https://www2.gov.bc.ca/gov/content/taxes/property-taxes/property-transfer-tax", "Includes further 2% residential value above 3M; no exemptions.",
  "S9", "GST/PST rates", "https://www.canada.ca/en/revenue-agency/services/tax/businesses/topics/gst-hst-businesses/charge-collect-which-rate/calculator.html", "BC 5% GST and 7% PST; do not apply 12% indiscriminately to services.",
  "U1", "User price/furniture bundles", "User supplied in conversation", "Furniture/supplies/delivery/assembly totals preserved; asking examples have no listing URLs supplied.",
  "A1", "Unverified operating assumptions", "Model assumptions, not supplier quotes", "Management 20%; strata 500/month; utilities incl internet 250/month; condo/house insurance 1500/3000/year; maintenance 0.5%/1% of price. Confirm scope and actual quotes.",
  "A2", "Cleaning / equipment scope", "User supplied in conversation", "No duplicate supplies cost. Initial cleaning is one-time; recurring turnover cleaning/laundry assumed settled through guest cleaning charges/management. Verify net fee leakage. Existing safety equipment, appliances, lights and window coverings assumed adequate; price separately if absent.",
  "A3", "Tax and eligibility limits", "Case simulation / modelling scope", "All-cash resale purchase; non-foreign buyer; pre-income-tax NOI; no capital appreciation. Corporate/licensing feasibility not established by fee budgets. Transaction tax treatment and platform-fee taxes/recoverability require confirmation. No standalone budget for extraordinary strata levies."
) %>% mutate(checked_date = "2026-11-14")
write_csv(cost_sources, "output/cost_sources.csv")

message("\nDone. Open output/. Reserve scenario: ", A$reserve_months,
        " months. Change A$reserve_months to 6 and rerun to compare.")
