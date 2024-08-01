#Isle Royale National Park
p_load(mapview, arrow, tools, data.table)

ir_pt <- data.frame(lon = c(-89.382365, -88.261587),
                    lat = c(47.76743,48.262361))

ir_pt_geo <- ir_pt %>% 
  st_as_sf(x=.,coords=c("lon","lat"), crs=4326)
mapview(ir_pt_geo)

sw_lat_ir=ir_pt[1,2]
ne_lat_ir=ir_pt[2,2]
sw_lon_ir=ir_pt[1,1]
ne_lon_ir=ir_pt[2,1]

pat_con <- open_dataset("/data/restricted_data/dewey/advan_monthly_patterns_parquet")

ir_test_1 <- pat_con %>%
  filter(date_range_start==as_datetime("2023-07-01"),
         longitude>=sw_lon_ir,longitude<=ne_lon_ir,latitude>=sw_lat_ir,latitude<=ne_lat_ir) %>%
  collect()

ir_1 <- ir_test_1 %>%
  select(-open_hours, -visits_by_day, -visitor_home_cbgs, -visitor_home_aggregation, -visitor_daytime_cbgs, 
         -bucketed_dwell_times, -related_same_day_brand, -related_same_month_brand, -popularity_by_hour,
         -popularity_by_day) %>%
  collect()

ir_test1_geo <- ir_1 %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(ir_test1_geo)

ir_test_2 <- pat_con %>%
  filter(date_range_start==as_datetime("2022-06-01"),
         longitude>=sw_lon_ir,longitude<=ne_lon_ir,latitude>=sw_lat_ir,latitude<=ne_lat_ir) %>%
  collect()

ir_visit_test2 <- ir_test_2 %>%
  select(-open_hours, -visits_by_day, -visitor_home_cbgs, -visitor_home_aggregation, -visitor_daytime_cbgs, 
         -bucketed_dwell_times, -related_same_day_brand, -related_same_month_brand, -popularity_by_hour,
         -popularity_by_day) %>%
  collect()

ir_visits_test2_geo <- ir_visit_test2 %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(ir_visits_test2_geo)

ir_test_3 <- pat_con %>%
  filter(date_range_start==as_datetime("2021-08-01"),
         longitude>=sw_lon_ir,longitude<=ne_lon_ir,latitude>=sw_lat_ir,latitude<=ne_lat_ir) %>%
  collect()

ir_test_4 <- pat_con %>%
  filter(date_range_start==as_datetime("2019-10-01"),
         longitude>=sw_lon_ir,longitude<=ne_lon_ir,latitude>=sw_lat_ir,latitude<=ne_lat_ir) %>%
  collect()

ir_visits <- pat_con %>%
  filter(longitude>=sw_lon_ir,longitude<=ne_lon_ir,latitude>=sw_lat_ir,latitude<=ne_lat_ir) %>%
  collect()

ir_visits_geo <- ir_visits %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(ir_visits_geo)

saveRDS(ir_visits, "/data/kfloersheim/tc_nps/build/cache/ir_visits_months.rds")
