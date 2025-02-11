#This script reads in the advan visitation data
library(pacman)
p_load(tidyverse,sf,conflicted,deweydatar,data.table)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

source("project_init.R")

################################################
#Read in park_subset
park_subset <- readRDS("build/cache/park_subset.rds")

#Read in visits data
visits <- readRDS("build/inputs/visit_dat.rds") 

poi <- readRDS("build/inputs/poi_dat.rds") 

##############
#Check whether all locations in park subset are valid
visits %>% 
  group_by(placekey) %>%
  mutate(min_date = min(date_range_start,na.rm = T),
            max_date = max(date_range_start,na.rm = T)) %>%
  distinct(placekey,location_name) %>%
  inner_join(select(park_subset,placekey)) %>% 
  inner_join(poi,by="placekey") %>%
  st_as_sf(wkt = "polygon_wkt",crs=4326) %>%
  mapview::mapview()

############



#############################


# parent_places <- visits %>%
#   distinct(placekey,parent_location_name = location_name)
# 
# unique_places <- visits %>%
#   distinct(placekey,parent_placekey,location_name,naics_code,top_category,category_tags,polygon_wkt) %>%
#   inner_join(parent_places,by = c("parent_placekey"="placekey"))
# 
# unique_places %>%
#   st_as_sf(wkt = "polygon_wkt",crs=4326) %>%
#   mapview::mapview()
# 
# unique_places %>%
#   select(-polygon_wkt) %>%
#   write_csv("build/cache/check_sub_units.csv")

###############################

#Calculate ratio of visits to visitors to estimate visits per visitor
#Adjust for sample used in regression
visits_to_visitors <- visits %>%
  mutate(visit_ratio = raw_visit_counts/raw_visitor_counts) 

visits_to_visitors %>%
  group_by(placekey,location_name) %>%
  summarise(vr = mean(visit_ratio,na.rm=TRUE)) %>%
  saveRDS("build/cache/visit_ratio.rds")


#Explode visits by home tract
visits_by_ct <- visits %>%
  select(placekey,date_range_start,visitor_home_aggregation) %>%
  arrange(placekey,date_range_start) %>%
  as.data.table() %>%
  expand_cat_json(.,expand='visitor_home_aggregation',index='tract',by=c('placekey','date_range_start')) %>%
  rename(visits=visitor_home_aggregation) 


#cache visits by home tract
visits_by_ct %>%
  inner_join(select(park_subset,placekey)) %>%
  write_csv("build/cache/parks_home_tract_t1.rds")

visits_by_ct %>%
  anti_join(select(park_subset,placekey)) %>%
  write_csv("build/cache/parks_home_tract_t2.rds")
