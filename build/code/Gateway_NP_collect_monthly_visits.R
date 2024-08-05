#Gateway Arch National Park
p_load(mapview, arrow, tools, data.table)

ga_pt <- data.frame(lon = c(-90.192707, -90.181389),
                    lat = c(38.618415,38.629821))

ga_pt_geo <- ga_pt %>% 
  st_as_sf(x=.,coords=c("lon","lat"), crs=4326)
mapview(ga_pt_geo)

sw_lat_ga=ga_pt[1,2]
ne_lat_ga=ga_pt[2,2]
sw_lon_ga=ga_pt[1,1]
ne_lon_ga=ga_pt[2,1]

pat_con <- open_dataset("/data/restricted_data/dewey/advan_monthly_patterns_parquet")

ga_test_1 <- pat_con %>%
  filter(date_range_start==as_datetime("2023-07-01"),
         longitude>=sw_lon_ga,longitude<=ne_lon_ga,latitude>=sw_lat_ga,latitude<=ne_lat_ga) %>%
  collect()

ga_1 <- ga_test_1 %>%
  select(-open_hours, -visits_by_day, -visitor_home_cbgs, -visitor_home_aggregation, -visitor_daytime_cbgs, 
         -bucketed_dwell_times, -related_same_day_brand, -related_same_month_brand, -popularity_by_hour,
         -popularity_by_day) %>%
  collect()

ga_test1_geo <- ga_1 %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(ga_test1_geo)

ga_test_2 <- pat_con %>%
  filter(date_range_start==as_datetime("2022-06-01"),
         longitude>=sw_lon_ga,longitude<=ne_lon_ga,latitude>=sw_lat_ga,latitude<=ne_lat_ga) %>%
  collect()

ga_visit_test2 <- ga_test_2 %>%
  select(-open_hours, -visits_by_day, -visitor_home_cbgs, -visitor_home_aggregation, -visitor_daytime_cbgs, 
         -bucketed_dwell_times, -related_same_day_brand, -related_same_month_brand, -popularity_by_hour,
         -popularity_by_day) %>%
  collect()

ga_visits_test2_geo <- ga_visit_test2 %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(ga_visits_test2_geo)

ga_test_3 <- pat_con %>%
  filter(date_range_start==as_datetime("2021-08-01"),
         longitude>=sw_lon_ga,longitude<=ne_lon_ga,latitude>=sw_lat_ga,latitude<=ne_lat_ga) %>%
  collect()

ga_test_4 <- pat_con %>%
  filter(date_range_start==as_datetime("2019-10-01"),
         longitude>=sw_lon_ga,longitude<=ne_lon_ga,latitude>=sw_lat_ga,latitude<=ne_lat_ga) %>%
  collect()

ga_check1 <- ga_test_1 %>%
  filter(placekey %in% c("222-222@5pj-5gg-dqf", "222-222@5pj-5gg-d9z", "222-222@5pj-5gg-cbk",
                         "225-22g@5pj-5gg-d7q","zzy-223@5pj-5gg-f9f", "222-229@5pj-5gg-dqf", "222-228@5pj-5gg-dqf",
                         "222-223@5pj-5gg-dqf", "222-223@5pj-5gg-cbk", "222-226@5pj-5gg-dqf", "222-227@5pj-5gg-dqf")) %>%
  collect()

ga_check1_geo <- ga_check1 %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(ga_check1_geo)

ga_check2 <- ga_test_2 %>%
  filter(placekey %in% c("zzw-222@5pj-5gg-dqf", "zzy-222@5pj-5gg-c5z", "zzy-222@5pj-5gg-dy9",
                         "222-223@5pj-5gg-d9z", "222-228@5pj-5gg-dqf","222-223@5pj-5gg-dqf",
                         "222-227@5pj-5gg-dqf", "222-226@5pj-5gg-dqf",
                         "222-222@5pj-5gg-dqf", "222-225@5pj-5gg-dqf")) %>%
  collect()

ga_check2_geo <- ga_check2 %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(ga_check2_geo)

ga_visits <- pat_con %>%
  filter(placekey %in% c("zzw-222@5pj-5gg-dqf", "zzy-222@5pj-5gg-c5z", "zzy-222@5pj-5gg-dy9","222-227@5pj-5gg-dqf",
                         "222-223@5pj-5gg-d9z", "222-225@5pj-5gg-dqf","222-222@5pj-5gg-dqf", "222-222@5pj-5gg-d9z", 
                         "222-222@5pj-5gg-cbk", "225-22g@5pj-5gg-d7q","zzy-223@5pj-5gg-f9f", "222-229@5pj-5gg-dqf", 
                         "222-228@5pj-5gg-dqf", "222-223@5pj-5gg-dqf", "222-223@5pj-5gg-cbk", "222-226@5pj-5gg-dqf") | 
           parent_placekey %in% c("223-223@5pj-5gg-dgk", "222-223@5pj-5gg-dqf",
                                  "zzz-222@5pj-5gg-dn5", "zzy-222@5pj-5gg-2ff")) %>%
  collect()

ga_visits_corrected <- ga_visits %>%
  filter(naics_code %in% c("453220", "712110", "712120", "813110", "712190", "722513", "487", "5615", "722511"),
         placekey != "zzy-222@5pj-5gg-c5z") %>%
  collect()

#did not include placekey for "Solar Eclipse 2017"

ga_correct <- ga_visits_corrected %>%
  select(-open_hours, -visits_by_day, -visitor_home_cbgs, -visitor_home_aggregation, -visitor_daytime_cbgs, 
         -bucketed_dwell_times, -related_same_day_brand, -related_same_month_brand, -popularity_by_hour,
         -popularity_by_day) %>%
  collect()

ga_visits_corrected_geo <- ga_correct %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(ga_visits_corrected_geo)

saveRDS(ga_visits_corrected, "/data/kfloersheim/tc_nps/build/cache/ga_visits_months.rds")

