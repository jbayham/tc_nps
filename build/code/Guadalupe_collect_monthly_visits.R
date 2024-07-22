#Guadalupe Mountains National Park
p_load(mapview, arrow, tools, data.table)

pat_con <- open_dataset("/data/restricted_data/dewey/advan_monthly_patterns_parquet")

gp_pt <- data.frame(lon = c(-105.06766, -104.711978),
                    lat = c(31.775036, 32.021043))

gp_pt_geo <- gp_pt %>% 
  st_as_sf(x=.,coords=c("lon","lat"), crs=4326)

mapview(gp_pt_geo)

sw_lat_gp=gp_pt[1,2]
ne_lat_gp=gp_pt[2,2]
sw_lon_gp=gp_pt[1,1]
ne_lon_gp=gp_pt[2,1]

gp_visits_test1 <- pat_con %>%
  filter(date_range_start==as_datetime("2023-07-01"),
         longitude>=sw_lon_gp,longitude<=ne_lon_gp,latitude>=sw_lat_gp,latitude<=ne_lat_gp) %>%
  collect()

gp_test1 <- gp_visits_test1 %>%
  select(-open_hours, -visits_by_day, -visitor_home_cbgs, -visitor_home_aggregation, -visitor_daytime_cbgs, 
         -bucketed_dwell_times, -related_same_day_brand, -related_same_month_brand, -popularity_by_hour,
         -popularity_by_day) %>%
  collect()

gp_visits_test1_geo <- gp_test1 %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(gp_visits_test1_geo)

gp_check <- gp_visits_test1 %>%
  filter(placekey == "zzw-222@8tk-m5c-3yv")

gp_check_geo <- gp_check %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(gp_check_geo)

gp_visits_test2 <- pat_con %>% 
  filter(date_range_start==as_datetime("2022-06-01"),
         longitude>=sw_lon_gp,longitude<=ne_lon_gp,latitude>=sw_lat_gp,latitude<=ne_lat_gp) %>%
  collect()

gp_test2 <- gp_visits_test2 %>%
  select(-open_hours, -visits_by_day, -visitor_home_cbgs, -visitor_home_aggregation, -visitor_daytime_cbgs, 
         -bucketed_dwell_times, -related_same_day_brand, -related_same_month_brand, -popularity_by_hour,
         -popularity_by_day) %>%
  collect()

gp_visits_test2_geo <- gp_test2 %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(gp_visits_test2_geo)

gp_visits_test3 <- pat_con %>% 
  filter(date_range_start==as_datetime("2021-08-01"),
         longitude>=sw_lon_gp,longitude<=ne_lon_gp,latitude>=sw_lat_gp,latitude<=ne_lat_gp) %>%
  collect()

gp_visits_test4 <- pat_con %>% 
  filter(date_range_start==as_datetime("2019-10-01"),
         longitude>=sw_lon_gp,longitude<=ne_lon_gp,latitude>=sw_lat_gp,latitude<=ne_lat_gp) %>%
  collect()

gp_visits <- pat_con %>%
  filter(longitude>=sw_lon_gp,longitude<=ne_lon_gp,latitude>=sw_lat_gp,latitude<=ne_lat_gp,
         parent_placekey %in% c("222-224@8tk-m3b-bff", "zzy-222@8tk-m57-ch5") | 
           placekey %in% c("222-225@8tk-m3b-bff", "222-224@8tk-m3b-bff", "zzy-222@8tk-m3b-c89",
                           "222-223@8tk-m3b-bff", "222-222@8tk-m3b-bff", "zzy-222@8tk-m57-ch5",
                           "zzy-222@8tk-m5c-3yv")) %>%
  collect()

gp_visits_corrected <- gp_visits %>%
  filter(location_name != "Hammack Ranch Cemetery") %>%
  collect()

gp_visits_geo <- gp_visits_corrected %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(gp_visits_geo)

saveRDS(gp_visits_corrected, "/data/kfloersheim/tc_nps/build/cache/gp_visits_months.rds")
