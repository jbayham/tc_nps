#Get travel distance and time

#install.packages("mapsapi")
library(pacman)
p_load(tidyverse,sf,mapsapi,janitor,measurements,progress,readxl)

source("project_init.R")

#Set google API key
key = Sys.getenv("GOOGLE_MAPS_API_KEY")

if(!dir.exists("build/cache/google_dist")) dir.create("build/cache/google_dist")
if(!dir.exists("build/cache/google_dist/mobile")) dir.create("build/cache/google_dist/mobile")
if(!dir.exists("build/cache/google_dist/survey")) dir.create("build/cache/google_dist/survey")

#####################################
#Mobile data
####################################
#Read in parks
park_subset <- readRDS("build/cache/park_subset.rds") %>%
  select(code,placekey,dest_lon,dest_lat)

# arches = filter(park_placekeys,park=="Arches") %>%
#   select(park,placekey)

#Read in data of all unique origins
census_geo <- readRDS("build/cache/census_geo_points_2019.rds") %>%
  rename(tract=geoid)


od_dat <- readRDS("build/cache/parks_home_tract_t1.rds") %>%
  distinct(placekey,tract) %>%
  inner_join(park_subset,by = join_by(placekey)) %>%
  distinct(code,tract,dest_lon,dest_lat) %>%
  left_join(select(census_geo,tract,orig_lon=longitude,orig_lat=latitude), by = c("tract"))

left_out <- od_dat %>%
  filter(is.na(orig_lon)) %>%
  select(placekey,tract)


# xwalk <- read_delim("build/inputs/tab20_tract20_tract10_natl.txt",delim = "|") %>%
#   clean_names()

od_dat <- od_dat %>%
  drop_na(dest_lon:orig_lat) %>%
  arrange(code,tract)

####################
#Already queried
dist_comp <- list.files("build/cache/google_dist/mobile",full.names = T,pattern = ".rds") %>%
  map(readRDS) %>%
  bind_rows() %>%
  select(placekey,tract,trav_dist) %>%
  drop_na() %>%
  distinct()

if(nrow(dist_comp)==0){
  remaining <- od_dat
} else {
  remaining <- anti_join(od_dat,dist_comp,by=c("placekey","tract"))
}


####################
#Break up origins into chunks of 25 with same destination
split_points <- remaining %>%
  group_split(group = grp_num_assign(tract,25),placekey) 


df=split_points[[2]] 

pb <- progress_bar$new(
  format = "/n  [:bar] :current/:total :elapsedfull eta: :eta",
  total = length(split_points))

split_points %>%
  walk(get_google_od,
       cache_dir = "mobile", #Careful: this must match the name of the directory created above
       progress = TRUE)
       

google_dist <- list.files("build/cache/google_dist/mobile",full.names = T) %>%
  map(~read_csv(.,col_select = c(park,placekey,tract,trav_dist,trav_time),col_types = "cccnn")) %>%
  bind_rows() %>%
  drop_na() %>%
  distinct(park,placekey,tract,.keep_all = T) 

write_csv(google_dist,"build/cache/mobile_google_dist.csv")

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
survey_dist_comp <- list.files("build/cache/google_dist/survey",full.names = T,pattern = ".rds") %>%
  map(readRDS) %>%
  bind_rows() %>%
  select(parkcode,zcta,trav_dist) %>%
  #drop_na() %>%
  distinct()

if(nrow(survey_dist_comp)==0){
  survey_remaining <- survey_od_dat
} else {
  survey_remaining <- anti_join(survey_od_dat,survey_dist_comp,by=c("parkcode","zcta"))
}

####################
#Break up origins into chunks of 25 with same destination
survey_split_points <- survey_remaining %>%
  group_split(group = grp_num_assign(zcta,25),parkcode) 


pb <- progress_bar$new(
  format = "/n  [:bar] :current/:total :elapsedfull eta: :eta",
  total = length(survey_split_points))

survey_split_points %>%
  walk(get_google_od,
       cache_dir = "survey", #Careful: this must match the name of the directory created above
       progress = TRUE)

#survey_split_points[[20]] %>% View()

survey_google_dist <- list.files("build/cache/google_dist/survey",full.names = T) %>%
  map(readRDS) %>%
  bind_rows() %>%
  arrange(parkcode,zcta,desc(trav_time)) %>%
  #select(parkcode,zcta,trav_dist,trav_time) %>%
  drop_na() %>%
  distinct(parkcode,zcta,.keep_all = T) 

write_csv(survey_google_dist,"build/cache/survey_google_dist.csv.gz")
