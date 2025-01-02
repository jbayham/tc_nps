#Lake Meredith National Recreation Area
p_load(mapview, arrow, tools, data.table)

lm_pt <- data.frame(lon = c(-101.884815, -101.488250),
                    lat = c(35.343514,35.755914))

lm_pt_geo <- lm_pt %>% 
  st_as_sf(x=.,coords=c("lon","lat"), crs=4326)
mapview(lm_pt_geo)

sw_lat_lm=lm_pt[1,2]
ne_lat_lm=lm_pt[2,2]
sw_lon_lm=lm_pt[1,1]
ne_lon_lm=lm_pt[2,1]

pat_con <- open_dataset("/data/restricted_data/dewey/advan_monthly_patterns_parquet")

lm_test_1 <- pat_con %>%
  filter(date_range_start==as_datetime("2023-07-01"),
         longitude>=sw_lon_lm,longitude<=ne_lon_lm,latitude>=sw_lat_lm,latitude<=ne_lat_lm) %>%
  collect()

lm_1 <- lm_test_1 %>%
  select(-open_hours, -visits_by_day, -visitor_home_cbgs, -visitor_home_aggregation, -visitor_daytime_cbgs, 
         -bucketed_dwell_times, -related_same_day_brand, -related_same_month_brand, -popularity_by_hour,
         -popularity_by_day) %>%
  collect()

lm_test1_geo <- lm_1 %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(lm_test1_geo)

lm_test_2 <- pat_con %>%
  filter(date_range_start==as_datetime("2022-06-01"),
         longitude>=sw_lon_lm,longitude<=ne_lon_lm,latitude>=sw_lat_lm,latitude<=ne_lat_lm) %>%
  collect()

lm_2 <- lm_test_2 %>%
  select(-open_hours, -visits_by_day, -visitor_home_cbgs, -visitor_home_aggregation, -visitor_daytime_cbgs, 
         -bucketed_dwell_times, -related_same_day_brand, -related_same_month_brand, -popularity_by_hour,
         -popularity_by_day) %>%
  collect()

lm_test_2_geo <- lm_2 %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(lm_test_2_geo)

lm_test_3 <- pat_con %>%
  filter(date_range_start==as_datetime("2021-08-01"),
         longitude>=sw_lon_lm,longitude<=ne_lon_lm,latitude>=sw_lat_lm,latitude<=ne_lat_lm) %>%
  collect()

lm_test_4 <- pat_con %>%
  filter(date_range_start==as_datetime("2019-10-01"),
         longitude>=sw_lon_lm,longitude<=ne_lon_lm,latitude>=sw_lat_lm,latitude<=ne_lat_lm) %>%
  collect()

lm_check_placekeys <- pat_con %>%
  filter(placekey %in% c("222-222@5qy-jkc-4vz", "zzy-222@5qy-jjk-k75", "zzy-222@5qy-jjq-k75", 
                         "zzw-222@5qy-jkn-qs5", "zzw-222@5qy-jkf-snq", "222-222@5qy-j6h-94v",
                         "zzy-222@5qy-jk8-w6k") | parent_placekey == "zzy-222@5qy-jjq-k75",
         date_range_start==as_datetime("2021-08-01")) %>%
  collect()

lm_check <- lm_check_placekeys %>%
  select(-open_hours, -visits_by_day, -visitor_home_cbgs, -visitor_home_aggregation, -visitor_daytime_cbgs, 
         -bucketed_dwell_times, -related_same_day_brand, -related_same_month_brand, -popularity_by_hour,
         -popularity_by_day) %>%
  collect()

lm_check_geo <- lm_check %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(lm_check_geo)

lm_visits <- pat_con %>%
  filter(placekey %in% c("222-222@5qy-jkc-4vz", "zzy-222@5qy-jjk-k75", "zzy-222@5qy-jjq-k75", 
                         "zzw-222@5qy-jkn-qs5", "zzw-222@5qy-jkf-snq", "222-222@5qy-j6h-94v",
                         "zzy-222@5qy-jk8-w6k") | parent_placekey == "zzy-222@5qy-jjq-k75") %>%
  collect()

lm_visits_geo <- lm_visits %>%
  filter(!is.na(polygon_wkt)) %>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(lm_visits_geo)

saveRDS(lm_visits, "/data/kfloersheim/tc_nps/build/cache/lm_visits_months.rds")

lm_visits_final <- lm_visits_months %>%
  filter(placekey != "222-222@5qy-j6h-94v") %>%
  collect()

saveRDS(lm_visits_final, "/data/kfloersheim/tc_nps/build/cache/lm_visits_months_correct.rds")
