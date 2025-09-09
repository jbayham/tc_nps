#### HEADER ####

# Last updated: AE on 2025-07-24-0950

# Clear memory
rm(list = ls())

# Clear console
cat("\f")

# Clear map (required for mapview to work properly)
# See here: https://stackoverflow.com/questions/62332956/cannot-display-mapview-output-after-installing-r-4-0-0
options(viewer = NULL)

#### PACKAGES ####

# Need the pacman package to use p_load()
if (!require(pacman)) install.packages('pacman')
library(pacman)

# Efficiently install/load packages
p_load(tidyverse, sf, readxl, epiDisplay)
p_load(tidyverse, sf, readxl)

#### IMPORT ####

# Importing airports_geo (note that the crs is 4326)
# Update file path accordingly!
airports_geo <- read.csv("build/cache/airports_geo.csv")

# Ordering by IATA
airports_geo <- airports_geo %>%
  arrange(code)

# Extract the state abbreviation and create a new column
airports_geo <- airports_geo %>%
  mutate(state = str_extract(airport, "(?<=, )[A-Z]{2}(?=:)"))

# Now linking to NP region
airports_geo <- airports_geo %>%
  mutate(region = case_when(
    state %in% c("WA", "ID", "OR", "CA", "NV") ~ "PWR",
    state %in% c("MT", "WY", "UT", "CO", "AZ", "NM", "TX", "OK") ~ "IMR",
    state %in% c("ND", "SD", "NE", "KS", "MN", "IA", "MO", "AR", "WI", "IL", "MI", "IN", "OH") ~ "MWR",
    state %in% c("KY", "TN", "MS", "AL", "GA", "LA", "FL", "NC", "SC") ~ "SER",
    state %in% c("ME", "VT", "NH", "MA", "RI", "CT", "NY", "NJ", "PA", "DE", "MD", "VA", "WV") ~ "NER",
    state == "DC" ~ "NER",
    state == "PR" ~ "SER",
    state %in% c("AK", "HI") ~ "PWR",
    TRUE ~ NA_character_
  ))

# Dropping AK, HI, and PR
airports_geo <- subset(airports_geo, !(state %in% c("AK", "HI", "PR")))

#### SPATIAL DISTANCE CALCULATIONS ####

# Convert the data frame to an sf object and keep the original columns
airports_sf <- st_as_sf(airports_geo, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# Generate all possible combinations of airports
airports_combos <- expand.grid(airport1_code = airports_geo$code, airport2_code = airports_geo$code, stringsAsFactors = FALSE)

# Remove combinations where the airports are the same
airports_combos <- airports_combos %>% filter(airport1_code != airport2_code)

# Convert the sf object back to a regular data frame for renaming
airports_df <- as.data.frame(airports_sf)

# Rename columns in airports_df for the first join
airports_df1 <- airports_df %>%
  rename_with(~ paste0("airport1_", .), -code) %>%
  rename(airport1_code = code)

# Rename columns in airports_df for the second join
airports_df2 <- airports_df %>%
  rename_with(~ paste0("airport2_", .), -code) %>%
  rename(airport2_code = code)

# Merge the coordinates for both airports
airports_combos <- airports_combos %>%
  left_join(airports_df1, by = c("airport1_code" = "airport1_code")) %>%
  left_join(airports_df2, by = c("airport2_code" = "airport2_code"))

# Calculate the distances
airports_combos <- airports_combos %>%
  mutate(
    distance_meters_oneway = st_distance(airport1_geometry, airport2_geometry, by_element = TRUE)
  )

# Removing the meters units
units(airports_combos$distance_meters_oneway) <- NULL

# Adding miles
airports_combos$airport2airport_miles_oneway <- airports_combos$distance_meters_oneway * 0.000621371

# Deleting meters
airports_combos$distance_meters_oneway <- NULL

# Ordering by IATA again
airports_combos <- airports_combos %>%
  arrange(airport1_code, airport2_code)

#### FARE REGRESSION ####

# Using Table 6 from Consumer Airfare Report (from Transportation.gov)
# See here: https://data.transportation.gov/Aviation/Consumer-Airfare-Report-Table-6-Contiguous-State-C/yj5y-b2ir/about_data
# Dataset last updated July 10, 2025
# Click export, then download file as CSV

# Importing
# Update file path accordingly!
DOTfare_00 <- read.csv("build/inputs/Consumer_Airfare_Report__Table_6_-_Contiguous_State_City-Pair_Markets_That_Average_At_Least_10_Passengers_Per_Day_20250725.csv")

# Condensing
DOTfare_01 <- DOTfare_00[,c("Year", "quarter", "city1", "Geocoded_City1", "city2", "Geocoded_City2", "nsmiles", "passengers", "fare")]

# Converting year to numeric
DOTfare_01$Year <- as.numeric(DOTfare_01$Year)

# Observing the years for which there are data
sort(unique(DOTfare_01$Year))

# Narrowing down to years of interest
DOTfare_02 <- DOTfare_01[DOTfare_01$Year %in% 2019:2022, ]
# Checking year range
tab1(DOTfare_02$Year)

# Removing the parentheses parts of city names
DOTfare_02 <- DOTfare_02 %>%
  mutate(city1 = gsub("\\s*\\(.*?\\)", "", city1)) %>%           # Remove extra text in parentheses
  separate(city1, into = c("city_1", "state_1"), sep = ", ") %>% # Split the cleaned city1 into two columns
  mutate(city2 = gsub("\\s*\\(.*?\\)", "", city2)) %>%           # Remove extra text in parentheses
  separate(city2, into = c("city_2", "state_2"), sep = ", ")     # Split the cleaned city2 into two columns

# Now linking to NP region
DOTfare_02 <- DOTfare_02 %>%
  mutate(region_1 = case_when(
    state_1 %in% c("WA", "ID", "OR", "CA", "NV") ~ "PWR",
    state_1 %in% c("MT", "WY", "UT", "CO", "AZ", "NM", "TX", "OK") ~ "IMR",
    state_1 %in% c("ND", "SD", "NE", "KS", "MN", "IA", "MO", "AR", "WI", "IL", "MI", "IN", "OH") ~ "MWR",
    state_1 %in% c("KY", "TN", "MS", "AL", "GA", "LA", "FL", "NC", "SC") ~ "SER",
    state_1 %in% c("ME", "VT", "NH", "MA", "RI", "CT", "NY", "NJ", "PA", "DE", "MD", "VA", "WV") ~ "NER",
    state_1 == "DC" ~ "NER",
    state_1 == "PR" ~ "SER",
    state_1 %in% c("AK", "HI") ~ "PWR",
    TRUE ~ NA_character_
  )) %>%
  mutate(region_2 = case_when(
    state_2 %in% c("WA", "ID", "OR", "CA", "NV") ~ "PWR",
    state_2 %in% c("MT", "WY", "UT", "CO", "AZ", "NM", "TX", "OK") ~ "IMR",
    state_2 %in% c("ND", "SD", "NE", "KS", "MN", "IA", "MO", "AR", "WI", "IL", "MI", "IN", "OH") ~ "MWR",
    state_2 %in% c("KY", "TN", "MS", "AL", "GA", "LA", "FL", "NC", "SC") ~ "SER",
    state_2 %in% c("ME", "VT", "NH", "MA", "RI", "CT", "NY", "NJ", "PA", "DE", "MD", "VA", "WV") ~ "NER",
    state_2 == "DC" ~ "NER",
    state_2 == "PR" ~ "SER",
    state_2 %in% c("AK", "HI") ~ "PWR",
    TRUE ~ NA_character_
  ))

# Now creating interacted region dummies

# Starting with a list of the NPS regions
regions <- c("PWR", "IMR", "MWR", "SER", "NER")

# Initializing vector to store dummy combinations
region_combos <- c()

# Loop through and create a dummy for each pair of regions
for (i in 1:length(regions)) {
  for (j in i:length(regions)) {
    region1 <- regions[i]
    region2 <- regions[j]
    var_name <- paste0(region1, "_", region2)
    region_combos <- c(region_combos, var_name)
    DOTfare_02[[var_name]] <- ifelse((DOTfare_02$region_1 == region1 & DOTfare_02$region_2 == region2) | 
                                       (DOTfare_02$region_1 == region2 & DOTfare_02$region_2 == region1), 1, 0)
  }
}

# Now moving on to the regression

# Base formula components
eqn_base <- "fare ~ nsmiles + as.factor(Year)"

# Combine base formula with dummy variables
eqn_full <- paste(eqn_base, paste(region_combos, collapse = " + "), sep = " + ")

# Run the regression
reg_flyfare <- lm(as.formula(eqn_full), data = DOTfare_02)
summary(reg_flyfare)

#### Cross validatation ##########
# Set seed for reproducibility
set.seed(20)

p_load(caret,Metrics,flextable)

# Create 5 folds
folds <- createFolds(DOTfare_02$fare, k = 10, list = TRUE, returnTrain = FALSE)

# Store metrics
results <- data.frame(
  Fold = integer(),
  RMSE = numeric(),
  MAE = numeric(),
  MAPE = numeric(),
  R2 = numeric(),
  Correlation = numeric()
)

# Loop over folds
for (i in 1:10) {
  test_idx <- folds[[i]]
  test_data <- DOTfare_02[test_idx, ]
  train_data <- DOTfare_02[-test_idx, ]
  
  # Fit model on training data
  model_i <- lm(as.formula(eqn_full), data = train_data)
  
  # Predict on test data
  preds <- predict(model_i, newdata = test_data)
  actual <- test_data$fare
  
  # Calculate metrics
  rmse_i <- RMSE(preds, actual)
  mae_i <- MAE(preds, actual)
  mape_i <- mape(actual, preds) * 100  # percentage
  r2_i <- R2(preds, actual)
  corr_i <- cor(preds, actual)
  
  # Append to results
  results <- rbind(results, data.frame(
    Fold = i,
    RMSE = rmse_i,
    MAE = mae_i,
    MAPE = mape_i,
    R2 = r2_i,
    Correlation = corr_i
  ))
}

# Print summary
summary_results <- data.frame(
  Metric = c("RMSE", "MAE", "MAPE (%)", "R2", "Correlation"),
  Mean = sapply(results[,-1], mean),
  SD = sapply(results[,-1], sd)
)

# Prepare table
cv_table <- summary_results
colnames(cv_table) <- c("Metric", "Mean", "Std. Dev.")
cv_table$Mean <- round(cv_table$Mean, 2)
cv_table$`Std. Dev.` <- round(cv_table$`Std. Dev.`, 2)

# Create flextable
ft <- flextable(cv_table)
ft <- set_caption(ft, "Out-of-sample model fit statistics from 5-fold cross-validation")
ft <- autofit(ft)

# Print table to view and copy
ft


#### PREDICTIONS ####

# Need to start by giving airports_combos consistent column names

# Making a copy of the airports combos df
airports_combos_01 <- airports_combos

# Renaming column
airports_combos_01 <- airports_combos_01 %>%
  rename(nsmiles = airport2airport_miles_oneway)

# Loop through and create a dummy for each pair of regions
for (i in 1:length(regions)) {
  for (j in i:length(regions)) {
    region1 <- regions[i]
    region2 <- regions[j]
    var_name <- paste0(region1, "_", region2)
    airports_combos_01[[var_name]] <- ifelse((airports_combos_01$airport1_region == region1 & airports_combos_01$airport2_region == region2) | 
                                       (airports_combos_01$airport1_region == region2 & airports_combos_01$airport2_region == region1), 1, 0)
  }
}

# Initialize the predictions data frame with the original data
airfares <- airports_combos_01

# Possible years
years <- c(2019:2022)

# Looping through each year
for (year in years) {
  airfares$Year <- year # Add Year column to the df
  column_name <- paste0("flyfare_predict_", year) # Generate predictions
  airfares[[column_name]] <- predict(reg_flyfare, newdata = airfares)
}

# Dropping the unnecessary Year column
airfares$Year <- NULL

#### EXPORTING ####

# Rearranging columns
airfares <- airfares[,c("airport1_code", "airport2_code", "airport1_airport", "airport1_state", "airport1_region", "airport1_lat", "airport1_lon", "airport1_avg_originating", "airport2_airport", "airport2_state", "airport2_region", "airport2_lat", "airport2_lon", "airport2_avg_originating", "nsmiles", "PWR_PWR", "PWR_IMR", "PWR_MWR", "PWR_SER", "PWR_NER", "IMR_IMR", "IMR_MWR", "IMR_SER", "IMR_NER", "MWR_MWR", "MWR_SER", "MWR_NER", "SER_SER", "SER_NER", "NER_NER", "flyfare_predict_2019", "flyfare_predict_2020", "flyfare_predict_2021", "flyfare_predict_2022")] # "airport1_geometry", "airport2_geometry"
# From the above, removed the geometry columns (otherwise the df can't export properly as a csv below)

# Exporting
#write.csv(airfares, "C:/Users/aenriquez/Documents/Research_USGS/NPS/Mobile_Bayham/TravelCosts/airfares.csv", row.names = FALSE)

