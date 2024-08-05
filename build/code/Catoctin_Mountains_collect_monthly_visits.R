#Catoctin Mountain Park
p_load(mapview, arrow, tools, data.table)

cm_pt <- data.frame(lon = c(-77.512388, -77.407331),
                    lat = c(39.618963,39.696671))

cm_pt_geo <- cm_pt %>% 
  st_as_sf(x=.,coords=c("lon","lat"), crs=4326)
mapview(cm_pt_geo)

sw_lat_cm=cm_pt[1,2]
ne_lat_cm=cm_pt[2,2]
sw_lon_cm=cm_pt[1,1]
ne_lon_cm=cm_pt[2,1]

pat_con <- open_dataset("/data/restricted_data/dewey/advan_monthly_patterns_parquet")

cm_test_1 <- pat_con %>%
  filter(date_range_start==as_datetime("2023-07-01"),
         longitude>=sw_lon_cm,longitude<=ne_lon_cm,latitude>=sw_lat_cm,latitude<=ne_lat_cm,
         location_name != "Cunningham Falls State Park",
         naics_code %in% c("712190", "712120", "5615", "7212")) %>%
  collect()

cm_1 <- cm_test_1 %>%
  select(-open_hours, -visits_by_day, -visitor_home_cbgs, -visitor_home_aggregation, -visitor_daytime_cbgs, 
         -bucketed_dwell_times, -related_same_day_brand, -related_same_month_brand, -popularity_by_hour,
         -popularity_by_day) %>%
  collect()

cm_test1_geo <- cm_1 %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(cm_test1_geo)

cm_test_1_filter <- pat_con %>%
  filter(date_range_start==as_datetime("2023-07-01"),
         longitude>=sw_lon_cm,longitude<=ne_lon_cm,latitude>=sw_lat_cm,latitude<=ne_lat_cm,
         location_name != "Cunningham Falls State Park" & location_name != "Mountain Top Travel" &
           location_name != "Community Park" & location_name != "Ice Plant Park" &
           location_name != "Thurmont Historical Society" & location_name != "Mechanicstown Square Park" &
           location_name != "Thurmont Memorial Park",
         naics_code %in% c("712190", "712120", "5615", "7212")) %>%
  collect()

cm_1_filter <- cm_test_1_filter %>%
  select(-open_hours, -visits_by_day, -visitor_home_cbgs, -visitor_home_aggregation, -visitor_daytime_cbgs, 
         -bucketed_dwell_times, -related_same_day_brand, -related_same_month_brand, -popularity_by_hour,
         -popularity_by_day) %>%
  collect()

cm_test1_filter_geo <- cm_1_filter %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(cm_test1_filter_geo)


cm_test_2 <- pat_con %>%
  filter(date_range_start==as_datetime("2022-06-01"),
         location_name != "Cunningham Falls State Park" & location_name != "Mountain Top Travel" &
           location_name != "Community Park" & location_name != "Ice Plant Park" &
           location_name != "Thurmont Historical Society" & location_name != "Mechanicstown Square Park" &
           location_name != "Thurmont Memorial Park",
         naics_code %in% c("712190", "712120", "5615", "7212"),
         longitude>=sw_lon_cm,longitude<=ne_lon_cm,latitude>=sw_lat_cm,latitude<=ne_lat_cm) %>%
  collect()

cm_visit_test2 <- cm_test_2 %>%
  select(-open_hours, -visits_by_day, -visitor_home_cbgs, -visitor_home_aggregation, -visitor_daytime_cbgs, 
         -bucketed_dwell_times, -related_same_day_brand, -related_same_month_brand, -popularity_by_hour,
         -popularity_by_day) %>%
  collect()

cm_visits_test2_geo <- cm_visit_test2 %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(cm_visits_test2_geo)

cm_test_3 <- pat_con %>%
  filter(date_range_start==as_datetime("2021-08-01"),
         location_name != "Cunningham Falls State Park" & location_name != "Mountain Top Travel" &
           location_name != "Community Park" & location_name != "Ice Plant Park" &
           location_name != "Thurmont Historical Society" & location_name != "Mechanicstown Square Park" &
           location_name != "Thurmont Memorial Park",
         naics_code %in% c("712190", "712120", "5615", "7212"),
         longitude>=sw_lon_cm,longitude<=ne_lon_cm,latitude>=sw_lat_cm,latitude<=ne_lat_cm) %>%
  collect()

cm_test_4 <- pat_con %>%
  filter(date_range_start==as_datetime("2019-10-01"),
         location_name != "Cunningham Falls State Park" & location_name != "Mountain Top Travel" &
           location_name != "Community Park" & location_name != "Ice Plant Park" &
           location_name != "Thurmont Historical Society" & location_name != "Mechanicstown Square Park" &
           location_name != "Thurmont Memorial Park",
         naics_code %in% c("712190", "712120", "5615", "7212"),
         longitude>=sw_lon_cm,longitude<=ne_lon_cm,latitude>=sw_lat_cm,latitude<=ne_lat_cm) %>%
  collect()

cm_visits_test <- pat_con %>%
  filter(date_range_start==as_datetime("2019-10-01"),
         placekey %in% c("zzy-222@63s-sgb-qj9", "zzy-222@63s-sgb-x3q", "zzy-222@63s-sgd-7yv") | 
           parent_placekey == "zzy-222@63s-sgb-qj9") %>%
  collect()

cm_visits_test_total_geo <- cm_visits_test %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(cm_visits_test_total_geo)

cm_visits <- pat_con %>%
  filter(placekey %in% c("zzy-222@63s-sgb-qj9", "zzy-222@63s-sgb-x3q", "zzy-222@63s-sgd-7yv") | 
           parent_placekey == "zzy-222@63s-sgb-qj9") %>%
  collect()

cm_visits_geo <- cm_visits %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(cm_visits_geo)

saveRDS(cm_visits, "/data/kfloersheim/tc_nps/build/cache/cm_visits_months.rds")
