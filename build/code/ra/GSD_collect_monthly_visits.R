#Great Sand Dunes National Park
p_load(mapview, arrow, tools, data.table)

gsd_pt <- data.frame(lon = c(-105.747846, -105.370878),
                    lat = c(37.645167,37.982031))

gsd_pt_geo <- gsd_pt %>% 
  st_as_sf(x=.,coords=c("lon","lat"), crs=4326)
mapview(gsd_pt_geo)

sw_lat_gsd=gsd_pt[1,2]
ne_lat_gsd=gsd_pt[2,2]
sw_lon_gsd=gsd_pt[1,1]
ne_lon_gsd=gsd_pt[2,1]

pat_con <- open_dataset("/data/restricted_data/dewey/advan_monthly_patterns_parquet")

gsd_visits_test1 <- pat_con %>% 
  filter(date_range_start==as_datetime("2023-07-01"),
         longitude>=sw_lon_gsd,longitude<=ne_lon_gsd,latitude>=sw_lat_gsd,latitude<=ne_lat_gsd) %>%
  collect()

gsd_test1 <- gsd_visits_test1 %>%
  select(-open_hours, -visits_by_day, -visitor_home_cbgs, -visitor_home_aggregation, -visitor_daytime_cbgs, 
         -bucketed_dwell_times, -related_same_day_brand, -related_same_month_brand, -popularity_by_hour,
         -popularity_by_day) %>%
  collect()

gsd_visits_test1_geo <- gsd_test1 %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(gsd_visits_test1_geo)

gsd_visits_test2 <- pat_con %>% 
  filter(date_range_start==as_datetime("2022-06-01"),
         longitude>=sw_lon_gsd,longitude<=ne_lon_gsd,latitude>=sw_lat_gsd,latitude<=ne_lat_gsd) %>%
  collect()

gsd_test2 <- gsd_visits_test2 %>%
  select(-open_hours, -visits_by_day, -visitor_home_cbgs, -visitor_home_aggregation, -visitor_daytime_cbgs, 
         -bucketed_dwell_times, -related_same_day_brand, -related_same_month_brand, -popularity_by_hour,
         -popularity_by_day) %>%
  collect()

gsd_visits_test2_geo <- gsd_test2 %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(gsd_visits_test2_geo)

gsd_visits_test3 <- pat_con %>% 
  filter(date_range_start==as_datetime("2021-08-01"),
         longitude>=sw_lon_gsd,longitude<=ne_lon_gsd,latitude>=sw_lat_gsd,latitude<=ne_lat_gsd) %>%
  collect()

gsd_visits_test4 <- pat_con %>% 
  filter(date_range_start==as_datetime("2019-10-01"),
         longitude>=sw_lon_gsd,longitude<=ne_lon_gsd,latitude>=sw_lat_gsd,latitude<=ne_lat_gsd) %>%
  collect()

gsd_visits_check <- gsd_visits_test4 %>%
  filter(placekey %in% c("zzw-222@5q8-qdj-yvz", "223-223@5q8-qdj-yqf", "zzy-222@5q8-qfh-ysq",
                          "222-222@5q8-qm7-f4v", "zzy-222@5q8-qdj-yvz", "zzy-222@5q8-ptb-g49",
                          "zzy-222@5q8-qcy-bx5", "zzy-222@5q8-r69-rp9", "zzy-222@5q8-r67-td9", 
                          "zzy-223@5q8-qnz-2x5", "222-222@5q8-k55-wx5", "223-223@5q8-qdj-yqf")) %>%
  collect

gsd_check <- gsd_visits_check %>%
  select(-open_hours, -visits_by_day, -visitor_home_cbgs, -visitor_home_aggregation, -visitor_daytime_cbgs, 
         -bucketed_dwell_times, -related_same_day_brand, -related_same_month_brand, -popularity_by_hour,
         -popularity_by_day) %>%
  collect()

gsd_visits_check_geo <- gsd_check %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(gsd_visits_check_geo)

gsd_check2 <- gsd_visits_test4 %>%
  filter(placekey %in% c("zzw-222@5q8-qdj-yvz", "223-223@5q8-qdj-yqf", "zzy-222@5q8-qfh-ysq",
                         "zzy-222@5q8-qdj-yvz", "zzy-222@5q8-qcy-bx5", "223-223@5q8-qdj-yqf") |
           parent_placekey %in% c("zzw-222@5q8-qdj-yvz", "zzy-222@5q8-qcy-bx5")) %>%
  collect()

gsd_check_2 <- gsd_check2 %>%
  select(-open_hours, -visits_by_day, -visitor_home_cbgs, -visitor_home_aggregation, -visitor_daytime_cbgs, 
         -bucketed_dwell_times, -related_same_day_brand, -related_same_month_brand, -popularity_by_hour,
         -popularity_by_day) %>%
  collect()

gsd_visits_check_geo2 <- gsd_check_2 %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(gsd_visits_check_geo2)

gsd_visits <- pat_con %>%
  filter(placekey %in% c("zzw-222@5q8-qdj-yvz", "223-223@5q8-qdj-yqf", "zzy-222@5q8-qfh-ysq",
                         "zzy-222@5q8-qdj-yvz", "zzy-222@5q8-qcy-bx5", "223-223@5q8-qdj-yqf") |
           parent_placekey %in% c("zzw-222@5q8-qdj-yvz", "zzy-222@5q8-qcy-bx5")) %>%
  collect()

gsd_visits_correct <- pat_con %>%
  filter(placekey %in% c("zzw-222@5q8-qdj-yvz", "223-223@5q8-qdj-yqf", "zzy-222@5q8-qfh-ysq",
                         "zzy-222@5q8-qdj-yvz", "zzy-222@5q8-qcy-bx5") |
           parent_placekey == "zzy-222@5q8-qcy-bx5") %>%
  collect()

gsd_visits_final <- gsd_visits_correct %>%
  filter(placekey != "223-223@5q8-qdj-yqf") %>%
  collect()

gsd_visits_geo <- gsd_visits_final %>%
  filter(!is.na(polygon_wkt)) %>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(gsd_visits_geo)

saveRDS(gsd_visits_final, "/data/kfloersheim/tc_nps/build/cache/gsd_visits_months.rds")

