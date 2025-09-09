# Script to get car rental prices and parking costs at the nearest major airport for each airport in the dataset

# Loading libraries
library(pacman)
p_load(tidyverse,conflicted,sf,janitor,readr)

# Resolving conflicts
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("rename", "dplyr")

################

# Pulling information on average weekly car rental price at the largest US airports (https://www.nerdwallet.com/article/travel/car-rental-pricing-statistics)
rental <- data.frame(
  airport_name = c("Miami International Airport", "Harry Reid Airport", "John F. Kennedy International Airport", "Orlando Airport",
                   "Charlotte/Douglas Airport", "Hartsfield-Jackson Atlanta Airport", "George Bush Intercontinental Airport", "Dallas-Fort Worth Airport",
                   "Phoenix Sky Harbor Airport", "Newark Liberty International Airport", "Los Angeles Airport", "San Francisco International Airport",
                   "Denver Airport", "Seattle-Tacoma Airport", "Chicago O'Hare Airport"),
  rental_price_weekly = c(388.88, 410.30, 471.47, 497, 498.48, 508, 536.62, 544.30, 584.96, 590.68, 593.04, 645.07, 651.96, 690.60, 702.76),
  airport_code = c("MIA", "LAS", "JFK", "MCO", "CLT", "ATL", "IAH", "DFW", "PHX", "EWR", "LAX", "SFO", "DEN", "SEA", "ORD")
  ) %>%
  mutate(rental_price_daily = rental_price_weekly / 7)

parking <- read_csv("build/inputs/airport_parking_costs.csv") %>%
  clean_names() %>%
  select(airport_code=iata,daily_parking_cost=x1_day_parking_cost)

airport_sf <- read_csv("build/cache/airports_geo.csv") %>%
  clean_names() %>%
  select(airport_code=code,lat,lon,avg_originating) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Merging in spatial coordinates
rental_sf <- inner_join(airport_sf, rental, by = "airport_code")
parking_sf <- inner_join(airport_sf, parking, by = "airport_code")

#plot of prices against originating passengers
parking_sf %>%
  st_drop_geometry() %>%
  ggplot(aes(y=daily_parking_cost,x=avg_originating)) +
  geom_point() +
  geom_smooth(method="lm")

#plot of rental prices against originating passengers
rental_sf %>%
  st_drop_geometry() %>%
  ggplot(aes(y=rental_price_daily,x=avg_originating,label=airport_code)) +
  geom_text() +
  geom_smooth(method="lm")

########## 
#Identifying the closest rental airport for each airport

# Finding nearest rental airport for each of the airports
nearest_airport_rental <- st_nearest_feature(airport_sf, rental_sf)
nearest_airport_parking <- st_nearest_feature(airport_sf, parking_sf)

# Merging nearest rental airport info back into the main airport dataset
rental_cwlk <- st_drop_geometry(airport_sf) %>%
  mutate(rental_price_daily = rental_sf$rental_price_daily[nearest_airport_rental]) 

parking_cwlk <- st_drop_geometry(airport_sf) %>%
  mutate(parking_price_daily = parking_sf$daily_parking_cost[nearest_airport_parking]) 


# Writing out final dataset in rds
saveRDS(rental_cwlk, "build/cache/rental_car_prices.csv")
saveRDS(parking_cwlk, "build/cache/parking_prices.csv")
