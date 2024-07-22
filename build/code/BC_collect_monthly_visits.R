#Bryce Canyon National Park
p_load(mapview, arrow, tools, data.table)

bc_pt <- data.frame(lon = c(-112.321332, -112.017834),
                    lat = c(37.444509, 37.768717))

bc_pt_geo <- bc_pt %>% 
  st_as_sf(x=.,coords=c("lon","lat"), crs=4326)
mapview(bc_pt_geo)

sw_lat_bc=bc_pt[1,2]
ne_lat_bc=bc_pt[2,2]
sw_lon_bc=bc_pt[1,1]
ne_lon_bc=bc_pt[2,1]

pat_con <- open_dataset("/data/restricted_data/dewey/advan_monthly_patterns_parquet")

bc_visits_test1 <- pat_con %>% 
  filter(date_range_start==as_datetime("2023-07-01"), naics_code %in% c("713110","712120","712190","7211", "5615","453220", "532284","487", "713940"),
         region == "UT", iso_country_code=="US", 
         longitude>=sw_lon_bc,longitude<=ne_lon_bc,latitude>=sw_lat_bc,latitude<=ne_lat_bc) %>%
  collect()


bc_test1 <- bc_visits_test1 %>%
  select(-open_hours, -visits_by_day, -visitor_home_cbgs, -visitor_home_aggregation, -visitor_daytime_cbgs, 
         -bucketed_dwell_times, -related_same_day_brand, -related_same_month_brand, -popularity_by_hour,
         -popularity_by_day) %>%
  collect()

bc_visits_test1_geo <- bc_test1 %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(bc_visits_test1_geo)

bc_visits_test2 <- pat_con %>% 
  filter(date_range_start==as_datetime("2022-06-01"), naics_code %in% c("713110","712120","712190","7211", "5615","453220", "532284","487", "713940"),
         region == "UT", iso_country_code=="US", 
         longitude>=sw_lon_bc,longitude<=ne_lon_bc,latitude>=sw_lat_bc,latitude<=ne_lat_bc) %>%
  collect()

bc_test2 <- bc_visits_test2 %>%
  select(-open_hours, -visits_by_day, -visitor_home_cbgs, -visitor_home_aggregation, -visitor_daytime_cbgs, 
         -bucketed_dwell_times, -related_same_day_brand, -related_same_month_brand, -popularity_by_hour,
         -popularity_by_day) %>%
  collect()

bc_visits_test2_geo <- bc_test2 %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(bc_visits_test2_geo)

bc_visits_test3 <- pat_con %>% 
  filter(date_range_start==as_datetime("2021-08-01"), naics_code %in% c("713110","712120","712190","7211", "5615","453220", "532284","487", "713940"),
         region == "UT", iso_country_code=="US", 
         longitude>=sw_lon_bc,longitude<=ne_lon_bc,latitude>=sw_lat_bc,latitude<=ne_lat_bc) %>%
  collect()

bc_visits_test4 <- pat_con %>% 
  filter(date_range_start==as_datetime("2019-10-01"), naics_code %in% c("713110","712120","712190","7211", "5615","453220", "532284","487", "713940"),
         region == "UT", iso_country_code=="US", 
         longitude>=sw_lon_bc,longitude<=ne_lon_bc,latitude>=sw_lat_bc,latitude<=ne_lat_bc) %>%
  collect()

bc_visits <- pat_con %>%
  filter(parent_placekey == "zzy-222@5yy-84c-52k" | placekey %in% c("zzy-222@5yy-84c-52k", "zzy-222@5yy-82s-5mk", "zzy-222@5yy-82r-zxq", 
                                                                    "zzy-222@5yy-84x-nwk", "zzy-222@5yy-84b-z75", "zzy-226@5yy-82r-zxq", 
                                                                    "zzy-224@5yy-82r-zxq", "zzy-222@5yy-53w-t7q", "zzw-222@5yy-82s-28v", 
                                                                    "zzy-222@5yy-526-5xq", "zzy-228@5yy-82r-zxq", "zzy-222@5yy-848-cqz", 
                                                                    "zzy-222@5yy-84x-s3q", "zzy-222@5yy-53w-vcq", "zzy-222@5yy-82r-rtv"),
         longitude>=sw_lon_bc,longitude<=ne_lon_bc,latitude>=sw_lat_bc,latitude<=ne_lat_bc) %>%
  collect()

bc_check <- bc_visits %>%
  filter(placekey == "zzy-222@5yy-84c-52k") %>%
  collect()

bc_check_geo <- bc_check %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(bc_check_geo)

bc_total <- bc_visits %>%
  select(-open_hours, -visits_by_day, -visitor_home_cbgs, -visitor_home_aggregation, -visitor_daytime_cbgs, 
         -bucketed_dwell_times, -related_same_day_brand, -related_same_month_brand, -popularity_by_hour,
         -popularity_by_day) %>%
  collect()

bc_visits_geo <- bc_total %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(bc_visits_geo)

saveRDS(bc_visits, "/data/kfloersheim/tc_nps/build/cache/bc_visits_months.rds")
