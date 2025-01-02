#Arches National Park
p_load(mapview, arrow, tools, data.table)

arches_pt <- data.frame(lon = c(-109.799807, -109.448368),
                        lat = c(38.572869, 38.853929))

arches_pt_geo <- arches_pt %>% 
  st_as_sf(x=.,coords=c("lon","lat"), crs=4326)

mapview(arches_pt_geo)

sw_lat_arches=arches_pt[1,2]
ne_lat_arches=arches_pt[2,2]
sw_long_arches=arches_pt[1,1]
ne_long_arches=arches_pt[2,1]

pat_con <- open_dataset("/data/restricted_data/dewey/advan_monthly_patterns_parquet")

arches_visits_test1 <- pat_con %>% 
  filter(date_range_start==as_datetime("2023-07-01"), naics_code %in% c("713110","712120","712190","7211", "5615","453220", "532284","487", "713940"),
         region == "UT", iso_country_code=="US", 
         longitude>=sw_long_arches,longitude<=ne_long_arches,latitude>=sw_lat_arches,latitude<=ne_lat_arches) %>%
  collect()

#to make it easier to see if placekeys are in park
a_visits_test1 <- arches_visits_test1 %>%
  select(-open_hours, -visits_by_day, -visitor_home_cbgs, -visitor_home_aggregation, -visitor_daytime_cbgs, 
         -bucketed_dwell_times, -related_same_day_brand, -related_same_month_brand, -popularity_by_hour,
         -popularity_by_day) %>%
  collect()

a_visits_test1_geo <- a_visits_test1 %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(a_visits_test1_geo)

arches_visits_test2 <- pat_con %>% 
  filter(date_range_start==as_datetime("2022-06-01"), naics_code %in% c("713110","712120","712190","7211", "5615","453220", "532284","487", "713940"),
         region == "UT", iso_country_code=="US", 
         longitude>=sw_long_arches,longitude<=ne_long_arches,latitude>=sw_lat_arches,latitude<=ne_lat_arches) %>%
  collect()

a_visits_test2 <- arches_visits_test2 %>%
  select(-open_hours, -visits_by_day, -visitor_home_cbgs, -visitor_home_aggregation, -visitor_daytime_cbgs, 
         -bucketed_dwell_times, -related_same_day_brand, -related_same_month_brand, -popularity_by_hour,
         -popularity_by_day) %>%
  collect()

a_visits_test2_geo <- a_visits_test2 %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(a_visits_test2_geo)

arches_visits_test3 <- pat_con %>% 
  filter(date_range_start==as_datetime("2021-08-01"), naics_code %in% c("713110","712120","712190","7211", "5615","453220", "532284","487", "713940"),
         region == "UT", iso_country_code=="US", 
         longitude>=sw_long_arches,longitude<=ne_long_arches,latitude>=sw_lat_arches,latitude<=ne_lat_arches) %>%
  collect()

arches_visits_test4 <- pat_con %>% 
  filter(date_range_start==as_datetime("2019-10-01"), naics_code %in% c("713110","712120","712190","7211", "5615","453220", "532284","487", "713940"),
         region == "UT", iso_country_code=="US", 
         longitude>=sw_long_arches,longitude<=ne_long_arches,latitude>=sw_lat_arches,latitude<=ne_lat_arches) %>%
  collect()

#testing specific placekeys
arches_place_test <- pat_con %>% 
  filter(placekey %in% c("224-223@5qg-54n-d7q", "222-224@5qg-55n-snq", "222-223@5qg-532-8n5", "zzy-225@5qg-8n8-jn5", 
                         "zzy-228@5qg-532-8jv", "222-222@5qg-54m-8vz", "zzy-225@5qg-4zk-9vf", "zzw-222@5qg-54m-hyv"),
         longitude>=sw_long_arches,longitude<=ne_long_arches,latitude>=sw_lat_arches,latitude<=ne_lat_arches) %>%
  collect()

a_place_test <- arches_place_test %>%
  select(-open_hours, -visits_by_day, -visitor_home_cbgs, -visitor_home_aggregation, -visitor_daytime_cbgs, 
         -bucketed_dwell_times, -related_same_day_brand, -related_same_month_brand, -popularity_by_hour,
         -popularity_by_day) %>%
  collect()

arches_place_test_geo <- a_place_test %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(arches_place_test_geo)


arches_visits <- pat_con %>% 
  filter(parent_placekey =="zzy-222@5qg-8sr-mtv" | placekey %in% c("zzy-227@5qg-4zk-9vf", "zzy-222@5qg-8sr-mtv", "zzy-222@5qg-524-nyv",
                                                                  "zzy-222@5qg-524-nyv", "zzw-222@5qg-8p5-575", "zzy-225@5qg-532-8jv",
                                                                  "zzy-223@5qg-4zk-9vf","zzy-222@5qg-8p5-rff","zzy-226@5qg-4zk-9vf",
                                                                  "zzy-224@5qg-4zk-9vf","zzy-222@5qg-8db-cnq","zzy-222@5qg-8sz-28v"),
         longitude>=sw_long_arches,longitude<=ne_long_arches,latitude>=sw_lat_arches,latitude<=ne_lat_arches) %>%
  collect()

a_visits <- arches_visits %>%
  select(-open_hours, -visits_by_day, -visitor_home_cbgs, -visitor_home_aggregation, -visitor_daytime_cbgs, 
         -bucketed_dwell_times, -related_same_day_brand, -related_same_month_brand, -popularity_by_hour,
         -popularity_by_day) %>%
  collect()

arches_visits_geo <- a_visits %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(arches_visits_geo)


saveRDS(arches_visits, "/data/kfloersheim/tc_nps/build/cache/arches_visits_months.rds")
