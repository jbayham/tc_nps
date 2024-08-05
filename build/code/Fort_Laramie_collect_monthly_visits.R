#Fort Laramie National Historical Site
p_load(mapview, arrow, tools, data.table)

fl_pt <- data.frame(lon = c(-104.575916, -104.52313),
                    lat = c(42.191161,42.214876))

fl_pt_geo <- fl_pt %>% 
  st_as_sf(x=.,coords=c("lon","lat"), crs=4326)
mapview(fl_pt_geo)

sw_lat_fl=fl_pt[1,2]
ne_lat_fl=fl_pt[2,2]
sw_lon_fl=fl_pt[1,1]
ne_lon_fl=fl_pt[2,1]

pat_con <- open_dataset("/data/restricted_data/dewey/advan_monthly_patterns_parquet")

fl_test_1 <- pat_con %>%
  filter(date_range_start==as_datetime("2023-07-01"),
         longitude>=sw_lon_fl,longitude<=ne_lon_fl,latitude>=sw_lat_fl,latitude<=ne_lat_fl) %>%
  collect()

fl_1 <- fl_test_1 %>%
  select(-open_hours, -visits_by_day, -visitor_home_cbgs, -visitor_home_aggregation, -visitor_daytime_cbgs, 
         -bucketed_dwell_times, -related_same_day_brand, -related_same_month_brand, -popularity_by_hour,
         -popularity_by_day) %>%
  collect()

fl_test1_geo <- fl_1 %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(fl_test1_geo)

fl_test_2 <- pat_con %>%
  filter(date_range_start==as_datetime("2022-06-01"),
         longitude>=sw_lon_fl,longitude<=ne_lon_fl,latitude>=sw_lat_fl,latitude<=ne_lat_fl) %>%
  collect()

fl_visit_test2 <- fl_test_2 %>%
  select(-open_hours, -visits_by_day, -visitor_home_cbgs, -visitor_home_aggregation, -visitor_daytime_cbgs, 
         -bucketed_dwell_times, -related_same_day_brand, -related_same_month_brand, -popularity_by_hour,
         -popularity_by_day) %>%
  collect()

fl_visits_test2_geo <- fl_visit_test2 %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(fl_visits_test2_geo)

fl_test_3 <- pat_con %>%
  filter(date_range_start==as_datetime("2021-08-01"),
         longitude>=sw_lon_fl,longitude<=ne_lon_fl,latitude>=sw_lat_fl,latitude<=ne_lat_fl) %>%
  collect()

fl_test_4 <- pat_con %>%
  filter(date_range_start==as_datetime("2019-10-01"),
         longitude>=sw_lon_fl,longitude<=ne_lon_fl,latitude>=sw_lat_fl,latitude<=ne_lat_fl) %>%
  collect()

fl_visits <- pat_con %>%
  filter(longitude>=sw_lon_fl,longitude<=ne_lon_fl,latitude>=sw_lat_fl,latitude<=ne_lat_fl) %>%
  collect()

fl_visits_geo <- fl_visits %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(fl_visits_geo)

saveRDS(fl_visits, "/data/kfloersheim/tc_nps/build/cache/fl_visits_months.rds")
