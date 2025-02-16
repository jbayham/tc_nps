#This script compares the distance estimates between Google and OSRM

library(pacman)
p_load(tidyverse,sf,mapsapi,janitor,measurements,progress,readxl)

source("project_init.R")

od_dat <- readRDS("build/cache/parks_home_tract_t1.rds") %>%
  distinct(placekey,tract) %>%
  inner_join(park_subset,by = join_by(placekey)) %>% #Join with park subset
  distinct(code_dest,tract,dest_lon,dest_lat) %>%  #Distinct to find unique combinations of park code and dest_* because 
  arrange(code_dest,tract)

osrm <- readRDS("build/cache/mobile_osrm_dist.rds")
google <- readRDS("build/cache/mobile_google_dist.rds")

#How does missingness compare (counts, where from)
google_missing <- anti_join(od_dat,google,by=c("code_dest","tract"))
osrm_missing <- anti_join(od_dat,osrm,by=c("code_dest","tract"))

#How do they correlate
dist_dat <- select(od_dat,code_dest,tract) %>%
  left_join(select(google,code_dest,tract,google_dist=trav_dist,google_time=trav_time),
            by=c("code_dest","tract")) %>%
  left_join(select(osrm,code_dest,tract,osrm_dist=trav_dist,osrm_time=trav_time),
            by=c("code_dest","tract")) 

dist_dat %>%
  ggplot(aes(x=google_dist,y=osrm_dist)) +
  geom_point(alpha=.01)

summary(lm(google_dist ~ 0 + osrm_dist,data=dist_dat))

dist_dat %>%
  ggplot(aes(x=google_time,y=osrm_time)) +
  geom_point(alpha=.01)

summary(lm(google_time ~ 0 + osrm_time,data=dist_dat))
