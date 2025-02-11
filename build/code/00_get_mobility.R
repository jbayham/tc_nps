#This script queries the mobility data from Advan database located on server datecompute-02

library(pacman)
p_load(tidyverse,janitor,arrow,readxl)

source("project_init.R")

################################# Read in park map created by Kate F. She used
#bounding boxes around the park to identify placekeys of the park. Advan no
#longer processes visits to the park itself. We grab all placekeys for which the
#NPS boundary is listed as the parent placekey

# park_placekeys <- read_excel("build/inputs/nps_placekeys.xlsx") %>%
#   clean_names() %>%
#   select(1:4) %>%
#   drop_na(park)
# 
# count(park_placekeys,park)
# 
# saveRDS(park_placekeys,"build/cache/park_placekeys.rds")

#Subset of pois with verified locations and known visitor histories
park_subset <- read_excel("build/inputs/park_ref.xlsx") %>%
  select(park:dest_lat)

saveRDS(park_subset,"build/cache/park_subset.rds")



#Connect to monthly patterns
arrow_con <- open_dataset("/data/restricted_data/dewey/advan_monthly_patterns_parquet/") 


#Extracting monthly patterns of all identified NPS park locations and sub locations in US
pp_pat <- arrow_con %>%
  filter(parent_placekey %in% na.omit(park_subset$placekey) |
           placekey %in% na.omit(park_subset$placekey)) %>%
  #date_range_start == as_date("2021-07-01")) %>%
  collect()



count(pp_pat,date_range_start) %>% 
  ggplot(aes(x=date_range_start,y=n)) +
  geom_col()

write_csv(pp_pat,"build/inputs/park_monthly_pat.csv.gz")
saveRDS(pp_pat,"build/inputs/park_monthly_pat.rds")

#Just the visit data
pp_pat %>%
  select(placekey,location_name,date_range_start,raw_visit_counts,raw_visitor_counts,visitor_home_aggregation) %>%
  saveRDS("build/inputs/visit_dat.rds")

#the polygons for each poi
pp_pat %>%
  select(placekey:wkt_area_sq_meters) %>%
  distinct() %>%
  saveRDS("build/inputs/poi_dat.rds")
  


# park_subset %>%
#   filter(is.na(dest_lon)) %>%
#   inner_join(distinct(park_monthly_pat,placekey,location_name,longitude,latitude)) %>%
#   st_as_sf(coords = c("longitude","latitude"),crs=4326) %>%
#   mapview::mapview()



########################################################
#Query home panel stats
arrow_con <- open_dataset("/data/restricted_data/dewey/advan_monthly_patterns_home_panel_parquet/") 


#Aggregate to tracts, convert date
home_panel <- arrow_con %>%
  filter(iso_country_code=="US") %>%
  collect()

tract_devices <- home_panel %>%
  group_by(tract=str_sub(census_block_group,1,11), 
           year,mon) %>%
  summarize(residing=sum(number_devices_residing,na.rm=TRUE),
            daytime=sum(number_devices_primary_daytime,na.rm=TRUE)) %>%
  ungroup()


write_csv(tract_devices,"build/inputs/tract_devices.csv")
