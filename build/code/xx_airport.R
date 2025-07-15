#This script builds the airport data based on the most popular airports

library(pacman)
p_load(tidyverse,conflicted,readxl,janitor)

source("project_init.R")

conflicts_prefer(lubridate::month)
conflicts_prefer(dplyr::filter)
#######################################

#Read in all excel files
flist <- list.files("build/inputs/airports",pattern = ".xlsx",full.names = T)
fl=flist[1]
orig_airport <- map(flist,function(fl){
  year=str_extract(fl,"[:digit:]{4}")
  dat <- read_excel(fl,sheet = "Originating (Dom)",skip = 4) %>%
    add_column(year=year)
})

cleaned <- orig_airport %>%
  bind_rows() %>%
  clean_names() %>%
  select(airport, code, originating_domestic_passengers, year) %>%
  filter(!is.na(airport), !is.na(originating_domestic_passengers))

years_in_data <- cleaned %>% pull(year) %>% unique()
n_years <- length(years_in_data)

# Count years per airport
airports_all_years <- cleaned %>%
  group_by(code) %>%
  summarise(n_years_present = n_distinct(year), 
            airport = first(airport),
            .groups = "drop") %>%
  filter(n_years_present == n_years)

# Keep only those airports and calculate the average
final_airports <- cleaned %>%
  filter(airport %in% airports_all_years$airport,!is.na(code)) %>%
  group_by(code) %>%
  summarise(
    avg_originating = mean(originating_domestic_passengers, na.rm = TRUE),
    airport = first(airport),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_originating))

write_csv(final_airports,"~/Downloads/airport_list.csv")

#Link back to spatial data and generate final list of airports
airport_geo <- read_csv("build/inputs/airports/NTAD_Aviation_Facilities_7163558772200366310.csv") %>%
  clean_names() %>%
  select(code=arpt_id,lat=lat_decimal,lon=long_decimal)

final_airports_geo <- final_airports %>%
  inner_join(airport_geo,by = join_by(code))


write_csv(final_airports_geo,"build/cache/airports_geo.csv")
