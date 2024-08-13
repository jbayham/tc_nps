#Cumberland Gap
p_load(mapview, arrow, tools, data.table)

cg_pt <- data.frame(lon = c(-83.80646, -83.404429),
                    lat = c(36.511067,36.720086))

cg_pt_geo <- cg_pt %>% 
  st_as_sf(x=.,coords=c("lon","lat"), crs=4326)
mapview(cg_pt_geo)

sw_lat_cg=cg_pt[1,2]
ne_lat_cg=cg_pt[2,2]
sw_lon_cg=cg_pt[1,1]
ne_lon_cg=cg_pt[2,1]

pat_con <- open_dataset("/data/restricted_data/dewey/advan_monthly_patterns_parquet")

cg_test_1 <- pat_con %>%
  filter(date_range_start==as_datetime("2023-07-01"),
         longitude>=sw_lon_cg,longitude<=ne_lon_cg,latitude>=sw_lat_cg,latitude<=ne_lat_cg,
         naics_code %in% c("712190", "712120", "7212", "5615"),
         location_name != "Harrogate City Park" & location_name != "Harrogate Little League" &
           location_name != "Noetown Park" & location_name != "Civic Center Park" & 
           location_name != "Fords Wood Park" & location_name != "Old Lincoln High School Park") %>%
  collect()

cg_1 <- cg_test_1 %>%
  select(-open_hours, -visits_by_day, -visitor_home_cbgs, -visitor_home_aggregation, -visitor_daytime_cbgs, 
         -bucketed_dwell_times, -related_same_day_brand, -related_same_month_brand, -popularity_by_hour,
         -popularity_by_day) %>%
  collect()

cg_test1_geo <- cg_1 %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(cg_test1_geo)

cg_test_2 <- pat_con %>%
  filter(date_range_start==as_datetime("2022-06-01"),
         naics_code %in% c("712190", "712120", "7212", "5615", "713110"),
         location_name != "Harrogate City Park" & location_name != "Harrogate Little League" &
           location_name != "Noetown Park" & location_name != "Civic Center Park" & 
           location_name != "Fords Wood Park" & location_name != "Old Lincoln High School Park" &
           location_name != "Pineville Machine Shop" & location_name != "Cumberland Gap Dog Park" &
           location_name != "Cumberland Gap Town Hall" & location_name != "Eastern National Park & Monument" &
           location_name != "Wilderness Road State Park",
         longitude>=sw_lon_cg,longitude<=ne_lon_cg,latitude>=sw_lat_cg,latitude<=ne_lat_cg) %>%
  collect()

cg_visit_test2 <- cg_test_2 %>%
  select(-open_hours, -visits_by_day, -visitor_home_cbgs, -visitor_home_aggregation, -visitor_daytime_cbgs, 
         -bucketed_dwell_times, -related_same_day_brand, -related_same_month_brand, -popularity_by_hour,
         -popularity_by_day) %>%
  collect()

cg_visits_test2_geo <- cg_visit_test2 %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(cg_visits_test2_geo)

cg_test_3 <- pat_con %>%
  filter(date_range_start==as_datetime("2021-08-01"),
         naics_code %in% c("712190", "712120", "7212", "5615", "713110"),
         location_name != "Harrogate City Park" & location_name != "Harrogate Little League" &
           location_name != "Noetown Park" & location_name != "Civic Center Park" & 
           location_name != "Fords Wood Park" & location_name != "Old Lincoln High School Park" &
           location_name != "Pineville Machine Shop" & location_name != "Cumberland Gap Dog Park" &
           location_name != "Cumberland Gap Town Hall" & location_name != "Eastern National Park & Monument" &
           location_name != "Wilderness Road State Park",
         longitude>=sw_lon_cg,longitude<=ne_lon_cg,latitude>=sw_lat_cg,latitude<=ne_lat_cg) %>%
  collect()

cg_test_4 <- pat_con %>%
  filter(date_range_start==as_datetime("2019-10-01"),
         naics_code %in% c("712190", "712120", "7212", "5615", "713110"),
         location_name != "Harrogate City Park" & location_name != "Harrogate Little League" &
           location_name != "Noetown Park" & location_name != "Civic Center Park" & 
           location_name != "Fords Wood Park" & location_name != "Old Lincoln High School Park" &
           location_name != "Pineville Machine Shop" & location_name != "Cumberland Gap Dog Park" &
           location_name != "Cumberland Gap Town Hall" & location_name != "Eastern National Park & Monument" &
           location_name != "Wilderness Road State Park",
         longitude>=sw_lon_cg,longitude<=ne_lon_cg,latitude>=sw_lat_cg,latitude<=ne_lat_cg) %>%
  collect()

cg_visits <- pat_con %>%
  filter(naics_code %in% c("712190", "712120", "7212", "5615", "713110"),
         location_name != "Harrogate City Park" & location_name != "Harrogate Little League" &
           location_name != "Noetown Park" & location_name != "Civic Center Park" & 
           location_name != "Fords Wood Park" & location_name != "Old Lincoln High School Park" &
           location_name != "Pineville Machine Shop" & location_name != "Cumberland Gap Dog Park" &
           location_name != "Cumberland Gap Town Hall" & location_name != "Eastern National Park & Monument" &
           location_name != "Wilderness Road State Park",
         longitude>=sw_lon_cg,longitude<=ne_lon_cg,latitude>=sw_lat_cg,latitude<=ne_lat_cg) %>%
  collect()

cg_visits_correct <- cg_visits %>%
  filter(placekey != "222-222@63m-k26-bzf") %>% 
  collect()

cg_visits_geo <- cg_visits_correct %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(cg_visits_geo)

cg_visits_final <- cg_visits_months %>%
  filter(placekey != "zzy-223@63m-k6k-hkf") %>%
  collect()

cg_visits_geo_corrected <- cg_visits_final %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(cg_visits_geo_corrected)

saveRDS(cg_visits_final, "/data/kfloersheim/tc_nps/build/cache/cg_visits_months.rds")
