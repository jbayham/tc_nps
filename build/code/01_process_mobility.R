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
visits <- read_csv("build/inputs/park_monthly_pat.csv") %>%
  mutate(date_range_start=as_date(date_range_start)) %>%
  inner_join(select(park_subset,placekey))
  

#Calculate ratio of visits to visitors to estimate visits per visitor
#Adjust for sample used in regression
visits_to_visitors <- visits %>%
  mutate(visit_ratio = raw_visit_counts/raw_visitor_counts) 

visits_to_visitors %>%
  group_by(placekey,location_name) %>%
  summarise(vr = mean(visit_ratio,na.rm=TRUE)) %>%
  write_csv("build/cache/visit_ratio.csv")


#Explode visits by home tract
visits_by_ct <- visits %>%
  select(placekey,date_range_start,visitor_home_aggregation) %>%
  arrange(placekey,date_range_start) %>%
  as.data.table() %>%
  expand_cat_json(.,expand='visitor_home_aggregation',index='tract',by=c('placekey','date_range_start')) %>%
  rename(visits=visitor_home_aggregation) 


#cache visits by home tract
write_csv(visits_by_ct,"build/cache/parks_home_tract.csv")
