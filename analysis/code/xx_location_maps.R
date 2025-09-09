#This script reads in the park visitation data and plots the locations

library(pacman)
p_load(tidyverse,sf,mapview,readxl,janitor,tmap)

source("project_init.R")

###################################
#Read in park visitation data
poi_dat <- readRDS("build/inputs/poi_dat.rds")

#Subset of parks used


park_placekeys <- readRDS("build/cache/park_placekeys.rds")

parks_home_tract <- read_csv("build/cache/parks_home_tract.csv")

census_geo <- read_csv("build/cache/census_geo.csv")


##############################
#Create example plot of locations within parks
dest = "PINN"

park_poly <- park_subset %>%
  filter(code_dest == dest) %>%
  inner_join(select(poi_dat,placekey,date_range_start,polygon_wkt),
             by=c("placekey","sample_period"="date_range_start")) %>%
  st_as_sf(wkt = "polygon_wkt",crs=4326)

park_poly <- park_subset %>%
  filter(code_dest == dest) %>%
  inner_join(select(poi_dat,placekey,polygon_wkt),
             by=c("placekey")) %>%
  distinct() %>%
  st_as_sf(wkt = "polygon_wkt",crs=4326)

park_map <- tm_basemap("OpenStreetMap.Mapnik") + #start with a basemap
  tm_shape(park_poly[3,]) +
  tm_fill(col="placekey",alpha=.31) +
  tm_borders(col = "blue")

#Display as interactive leaflet
tmap_leaflet(park_map)


check <- unique(park_subset$code_dest) %>%
  map(function(x){
    park_poly <- park_subset %>%
      filter(code_dest == x) %>%
      inner_join(select(poi_dat,placekey,polygon_wkt),
                 by=c("placekey")) %>%
      distinct(polygon_wkt,.keep_all = T) %>%
      st_as_sf(wkt = "polygon_wkt",crs=4326)
    
    contd <- st_contains(park_poly)
  })
names(check) <- unique(park_subset$code_dest)
unlist(check,recursive = F)



teton_nps <- park_monthly_pat %>%
  filter(placekey == tetons$placekey[tetons$location_name=="grand_teton_national_park"]) %>%
  select(location_name,polygon_wkt) %>%
  distinct() %>%
  st_as_sf(wkt = "polygon_wkt",crs=4326)

teton_sub <- park_monthly_pat %>%
  filter(placekey == tetons$placekey[tetons$location_name!="grand_teton_national_park"]) %>%
  group_by(placekey) %>%
  slice_max(date_range_start,n=1) %>%
  ungroup() %>%
  distinct(location_name,.keep_all = T) %>%
  select(location_name,longitude,latitude) %>%
  st_as_sf(coords = c("longitude","latitude"),crs=4326) 

teton_sub$location_name[1] <- "Grand Teton Lodge"
teton_sub$location_name[2] <- "Visitors Center"

  


teton_map <- tm_basemap("OpenStreetMap.Mapnik") + #start with a basemap
  tm_shape(teton_nps) +
  tm_borders(col="red",lwd=2) +
  tm_shape(teton_sub) +
  tm_symbols(col = "orange",border.lwd = 0) +
  tm_text("location_name",just="left",ymod = -1,xmod=1,size=1.3) 
  
  

#Display as interactive leaflet
tmap_leaflet(teton_map)

####################################
#Visitor attribution figure
teton_sub_poly <- park_monthly_pat %>%
  filter(placekey == tetons$placekey[tetons$location_name!="grand_teton_national_park"]) %>%
  group_by(placekey) %>%
  slice_max(date_range_start,n=1) %>%
  ungroup() %>%
  distinct(location_name,.keep_all = T) %>%
  select(location_name,polygon_wkt) %>%
  st_as_sf(wkt = "polygon_wkt",crs=4326)

visit_attr <- tm_basemap("OpenStreetMap.Mapnik") + #start with a basemap
  tm_shape(teton_sub_poly) +
  tm_borders(col="red",lwd=2)
  
#Display as interactive leaflet
tmap_leaflet(visit_attr)

######################################
#Map of source tracts

home_points <- parks_home_tract %>%
  filter(placekey == tetons$placekey[tetons$location_name=="grand_teton_national_park"]) %>%
  inner_join(census_geo) %>%
  distinct(tract,.keep_all = T) %>%
  filter(date_range_start %within% (as_date("2022-01-01") %--% as_date("2022-12-31"))) %>%
  st_as_sf(coords = c("orig_lon","orig_lat"),crs=4326) 

home_map <- tm_basemap("OpenStreetMap.Mapnik") + #start with a basemap
  tm_shape(home_points) +
  tm_symbols(col = "darkorange",alpha=.3,border.lwd = 0,size = .05)

#Display as interactive leaflet
tmap_leaflet(home_map)

#################################
#Map of parks

park_points <- park_monthly_pat %>%
  inner_join(select(park_subset,park,placekey))  %>%
  distinct(park,.keep_all = T) %>%
  select(park,location_name,longitude,latitude) %>%
  st_as_sf(coords = c("longitude","latitude"),crs=4326) 


park_map <- tm_basemap("OpenStreetMap.Mapnik") + #start with a basemap
  tm_shape(park_points) +
  tm_symbols(col = "orange",border.lwd = 0) +
  tm_text("park",just="left",ymod = -1,xmod=1,size=1.3,auto.placement = T,remove.overlap = T) 

#Display as interactive leaflet
tmap_leaflet(park_map)


#############################
#Create list of locations with visitation data and date range
for_review <- park_monthly_pat %>%
  drop_na(visitor_home_aggregation) %>%
  group_by(placekey,parent_placekey,location_name) %>%
  summarize(min_date = as_date(min(date_range_start,na.rm = TRUE)),
            max_date = as_date(max(date_range_start,na.rm = TRUE)),
            mean_visits = mean(raw_visitor_counts,na.rm = TRUE)) %>%
  ungroup() %>%
  inner_join(select(park_placekeys,park,placekey),.,by = "placekey")

for_review %>%
  write_csv("build/cache/location_date_ranges.csv")


park_monthly_pat %>%
  select(placekey,parent_placekey,location_name,date_range_start,raw_visitor_counts,visitor_home_aggregation,wkt_area_sq_meters) %>%
  filter(placekey %in% c("zzy-223@5qg-4zk-9vf")) %>%
  arrange(date_range_start) %>%
  View()

park_geo <- park_monthly_pat %>%
  drop_na(visitor_home_aggregation) %>%
  select(placekey,parent_placekey,location_name,latitude,longitude,wkt_area_sq_meters,polygon_wkt) %>%
  distinct(placekey,.keep_all = TRUE) %>%
  st_as_sf(wkt = "polygon_wkt",crs=4326)

mapview(park_geo,alpha=.5)
park_geo %>%
  filter(str_detect(placekey,"zzy-222@63s-sgd-7yv")) %>%
  mapview(zcol="placekey",alpha=.2)


#Can we identify all of the geos that overlap
overlap <- st_intersects(park_geo)
overlap <- st_covered_by(park_geo)

overlap_area <- st_intersection(park_geo[2,],park_geo) %>%
  mutate(overlap_area = st_area(polygon_wkt))
