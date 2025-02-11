

library(pacman)
p_load(tidyverse,janitor,arrow,readxl)

source("project_init.R")



#Connect to monthly patterns
arrow_con <- open_dataset("/data/restricted_data/dewey/advan_monthly_patterns_parquet/") 

yr_range <- c(2019:2023)
i=2021
for(i in yr_range){
  #RMNP
  check <- arrow_con %>%
    filter(str_detect(location_name,"National Recreation Area"),
           #region=="CO",
           date_range_start == as_date(paste0(i,"-07-01")),
           !is.na(visitor_home_aggregation)) %>%
    select(placekey,parent_placekey,location_name,top_category,category_tags,raw_visitor_counts,visitor_home_aggregation) %>%
    collect()
  
  write_csv(check,paste0("build/cache/queries/usa_rec_area_",i,".csv"))
}

parks <- list.files("build/cache/queries",pattern = "usa_park",full.names = T) %>%
  map(read_csv) %>%
  bind_rows() %>%
  distinct(placekey,location_name,.keep_all = T) %>%
  arrange(location_name)

write_csv(parks,"build/cache/queries/cleaned_parks.csv")

monuments <- list.files("build/cache/queries",pattern = "usa_monument",full.names = T) %>%
  map(read_csv) %>%
  bind_rows() %>%
  distinct(placekey,location_name,.keep_all = T) %>%
  arrange(location_name)

write_csv(monuments,"build/cache/queries/cleaned_monument.csv")

historic <- list.files("build/cache/queries",pattern = "usa_historic",full.names = T) %>%
  map(read_csv) %>%
  bind_rows() %>%
  distinct(placekey,location_name,.keep_all = T) %>%
  arrange(location_name)

write_csv(historic,"build/cache/queries/cleaned_historic.csv")

rec_area <- list.files("build/cache/queries",pattern = "usa_rec_area",full.names = T) %>%
  map(read_csv) %>%
  bind_rows() %>%
  distinct(placekey,location_name,.keep_all = T) %>%
  arrange(location_name)

write_csv(rec_area,"build/cache/queries/cleaned_rec_area.csv")

###################################
i=2022
check <- arrow_con %>%
  filter(str_detect(location_name,"Cape Hatteras"),
         #region=="CO",
         date_range_start == as_date(paste0(i,"-07-01")),
         !is.na(visitor_home_aggregation)) %>%
  select(placekey,parent_placekey,location_name,top_category,category_tags,raw_visitor_counts,visitor_home_aggregation) %>%
  collect()



#Check all sub units of parent placekey
parent <- arrow_con 


#Check boundaries


  