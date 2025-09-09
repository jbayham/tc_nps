#Get travel distance and time

#install.packages("mapsapi")
library(pacman)
p_load(tidyverse,sf,mapsapi,janitor,measurements,progress,readxl)

source("project_init.R")

#Set google API key
key = Sys.getenv("GOOGLE_MAPS_API_KEY")

dir_ifnot("build/cache/google_dist")
dir_ifnotif("build/cache/google_dist/mobile")
dir_ifnotif("build/cache/google_dist/survey")
dir_ifnotif("build/cache/osrm_dist/mobile")
dir_ifnot("build/cache/osrm_dist/survey")

#####################################
#Mobile data
####################################
#Read in parks
park_subset <- readRDS("build/cache/park_subset.rds") %>%
  select(code,code_dest,placekey,dest_lon,dest_lat)


#Read in data of all unique origins
census_geo <- readRDS("build/cache/census_geo_points_2019.rds") %>%
  rename(tract=geoid)


od_dat <- readRDS("build/cache/parks_home_tract_t1.rds") %>%
  distinct(placekey,tract) %>%
  inner_join(park_subset,by = join_by(placekey)) %>% #Join with park subset
  distinct(code_dest,tract,dest_lon,dest_lat) %>%  #Distinct to find unique combinations of park code and dest_* because 
  left_join(select(census_geo,tract,orig_lon=longitude,orig_lat=latitude), by = c("tract")) %>%
  arrange(code_dest,tract)

left_out <- od_dat %>%
  filter(is.na(orig_lon))


# xwalk <- read_delim("build/inputs/tab20_tract20_tract10_natl.txt",delim = "|") %>%
#   clean_names()


####################
#Already queried google
cache_flist <- list.files("build/cache/google_dist/mobile",full.names = T,pattern = ".rds") 


if(is_empty(cache_flist)){
  remaining <- od_dat
} else {
  remaining <- cache_flist %>%
    map(readRDS) %>%
    bind_rows() %>%
    select(code_dest,tract,trav_dist) %>%
    drop_na() %>%
    distinct() %>%
    anti_join(od_dat,.,by=c("code_dest","tract"))
}

# remaining <- remaining %>%
#   filter(code_dest=="GRTE_02")

##############
#Error checking
# remaining_geo <- remaining %>%
#   filter(code_dest!="GRTE_02") %>%
#   st_as_sf(coords = c("orig_lon","orig_lat"),crs=4326)
# 
# mapview::mapview(remaining_geo)

######################
cache_flist <- list.files("build/cache/osrm_dist/mobile",full.names = T,pattern = ".rds") 


if(is_empty(cache_flist)){
  remaining <- od_dat
} else {
  remaining <- cache_flist %>%
    map(readRDS) %>%
    bind_rows() %>%
    select(code_dest,tract,trav_dist) %>%
    drop_na() %>%
    distinct() %>%
    anti_join(od_dat,.,by=c("code_dest","tract"))
}

# remaining <- remaining %>%
#   filter(code_dest=="GRTE_02")



####################
#Break up origins into chunks of 25 with same destination
split_points <- remaining %>%
  group_split(group = grp_num_assign(tract,25),code_dest) 

# map_dbl(split_points,nrow) %>%
#   hist()

df=split_points[[2]] 

pb <- progress_bar$new(
  format = "/n  [:bar] :current/:total :elapsedfull eta: :eta",
  total = length(split_points))

split_points[1] %>%
  walk(get_google_od,
       cache_dir = "build/cache/google_dist/mobile", #Careful: this must match the name of the directory created above
       progress = TRUE)
       

google_dist <- list.files("build/cache/google_dist/mobile",full.names = T) %>%
  #map(~read_csv(.,col_select = c(park,placekey,tract,trav_dist,trav_time),col_types = "cccnn")) %>%
  map(readRDS) %>% 
  bind_rows() %>%
  distinct(code_dest,tract,.keep_all = T) 

google_dist_nomatch <- google_dist %>%
  filter(if_any(everything(),is.na))

google_dist <- google_dist %>%
  drop_na()

saveRDS(google_dist,"build/cache/mobile_google_dist.rds")

########################
#OSRM

#Break up origins into chunks of 250 with same destination
split_points <- remaining %>%
  group_split(group = grp_num_assign(tract,250),code_dest) 

# map_dbl(split_points,nrow) %>%
#   hist()

df=split_points[[2]] 

pb <- progress_bar$new(
  format = "/n  [:bar] :current/:total :elapsedfull eta: :eta",
  total = length(split_points))

split_points %>%
  walk(get_osrm_od,
       cache_dir = "build/cache/osrm_dist/mobile", #Careful: this must match the name of the directory created above
       progress = TRUE)


osrm_dist <- list.files("build/cache/osrm_dist/mobile",full.names = T) %>%
  map(readRDS) %>% 
  bind_rows() %>%
  distinct(code_dest,tract,.keep_all = T) 

osrm_dist_nomatch <- osrm_dist %>%
  filter(if_any(everything(),is.na))

osrm_dist <- osrm_dist %>%
  drop_na()

saveRDS(osrm_dist,"build/cache/mobile_osrm_dist.rds")

##########################################
#Calculating crow-flies distance for flying
split_points <- od_dat %>%
  group_split(code_dest)

df=split_points[[1]]

#check <- st_crow_flies(df)

flight_dist <- map(split_points,st_crow_flies) %>%
  bind_rows() %>%
  select(-c(dest_lon:orig_lat))


saveRDS(flight_dist,"build/cache/flight_dist.rds")






##############################################
#Survey data
#############################################
survey_raw <- readRDS("build/inputs/AE/SEM_dems.rds")

survey_od_dat <- survey_raw %>%
  clean_names() %>%
  select(parkcode,zcta,starts_with("orig"),starts_with("dest")) %>%
  drop_na()

####################
#Already queried
cache_flist <- list.files("build/cache/osrm_dist/survey",full.names = T,pattern = ".rds") 


if(is_empty(cache_flist)){
  remaining <- survey_od_dat
} else {
  remaining <- cache_flist %>%
    map(readRDS) %>%
    bind_rows() %>%
    select(parkcode,zcta,trav_dist) %>%
    drop_na() %>%
    distinct() %>%
    anti_join(survey_od_dat,.,by=c("parkcode","zcta"))
}
####################
#Break up origins into chunks of 25 with same destination
split_points <- remaining %>%
  group_split(group = grp_num_assign(zcta,250),parkcode) 


pb <- progress_bar$new(
  format = "/n  [:bar] :current/:total :elapsedfull eta: :eta",
  total = length(split_points))

split_points %>%
  walk(get_osrm_od,
       cache_dir = "build/cache/osrm_dist/survey", #Careful: this must match the name of the directory created above
       progress = TRUE)

#survey_split_points[[20]] %>% View()

survey_osrm_dist <- list.files("build/cache/osrm_dist/survey",full.names = T) %>%
  map(readRDS) %>%
  bind_rows() %>%
  arrange(parkcode,zcta,desc(trav_time)) %>%
  #select(parkcode,zcta,trav_dist,trav_time) %>%
  #drop_na() %>%
  distinct(parkcode,zcta,.keep_all = T) 

write_csv(survey_osrm_dist,"build/cache/survey_osrm_dist.csv.gz")

check <- inner_join(survey_od_dat,survey_osrm_dist,by=c("parkcode","zcta"))
