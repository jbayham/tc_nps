#Capitol Reef National Park
p_load(mapview, arrow, tools, data.table)

cr_pt <- data.frame(lon = c(-111.862753, -110.511435),
                    lat = c(37.377867,38.735944))

cr_pt_geo <- cr_pt %>% 
  st_as_sf(x=.,coords=c("lon","lat"), crs=4326)
mapview(cr_pt_geo)

sw_lat_cr=cr_pt[1,2]
ne_lat_cr=cr_pt[2,2]
sw_lon_cr=cr_pt[1,1]
ne_lon_cr=cr_pt[2,1]

pat_con <- open_dataset("/data/restricted_data/dewey/advan_monthly_patterns_parquet")

cr_visits_test1 <- pat_con %>% 
  filter(date_range_start==as_datetime("2023-07-01"), naics_code %in% c("713110","712120","712190","7211", "5615","453220", "532284","487", "713940"),
         region == "UT", iso_country_code=="US", 
         longitude>=sw_lon_cr,longitude<=ne_lon_cr,latitude>=sw_lat_cr,latitude<=ne_lat_cr, location_name != "Glen Canyon National Recreation Area" & location_name != "Goblin Valley State Park" &
           location_name != "Escalante Petrified Forest State Park") %>%
  collect()

cr_visits_test1_geo <- cr_visits_test1 %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(cr_visits_test1_geo)

cr_visits_test2 <- pat_con %>% 
  filter(date_range_start==as_datetime("2022-06-01"), naics_code %in% c("713110","712120","712190","7211", "5615","453220", "532284","487", "713940"),
         region == "UT", iso_country_code=="US", 
         longitude>=sw_lon_cr,longitude<=ne_lon_cr,latitude>=sw_lat_cr,latitude<=ne_lat_cr, location_name != "Glen Canyon National Recreation Area" & location_name != "Goblin Valley State Park" &
           location_name != "Escalante Petrified Forest State Park") %>%
  collect()

cr_test2 <- cr_visits_test2 %>%
  select(-open_hours, -visits_by_day, -visitor_home_cbgs, -visitor_home_aggregation, -visitor_daytime_cbgs, 
         -bucketed_dwell_times, -related_same_day_brand, -related_same_month_brand, -popularity_by_hour,
         -popularity_by_day) %>%
  collect()

cr_visits_test2_geo <- cr_test2 %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(cr_visits_test2_geo)

cr_check <- cr_visits_test1 %>%
  filter(placekey == "zzy-222@5yy-qhs-7wk") %>%
  collect

cr_check_geo <- cr_check %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(cr_check_geo)

cr_visits_test3 <- pat_con %>% 
  filter(date_range_start==as_datetime("2021-08-01"), naics_code %in% c("713110","712120","712190","7211", "5615","453220", "532284","487", "713940"),
         region == "UT", iso_country_code=="US", 
         longitude>=sw_lon_cr,longitude<=ne_lon_cr,latitude>=sw_lat_cr,latitude<=ne_lat_cr, location_name != "Glen Canyon National Recreation Area" & location_name != "Goblin Valley State Park" &
           location_name != "Escalante Petrified Forest State Park") %>%
  collect()

cr_visits_test4 <- pat_con %>% 
  filter(date_range_start==as_datetime("2019-10-01"), naics_code %in% c("713110","712120","712190","7211", "5615","453220", "532284","487", "713940"),
         region == "UT", iso_country_code=="US", 
         longitude>=sw_lon_cr,longitude<=ne_lon_cr,latitude>=sw_lat_cr,latitude<=ne_lat_cr, location_name != "Glen Canyon National Recreation Area" & location_name != "Goblin Valley State Park" &
           location_name != "Escalante Petrified Forest State Park") %>%
  collect()

cr_visits <- pat_con %>%
  filter(longitude>=sw_lon_cr,longitude<=ne_lon_cr,latitude>=sw_lat_cr,latitude<=ne_lat_cr,
         parent_placekey %in% c("223-222@5yy-q9m-z75", "zzy-226@5yy-q7x-jn5", "zzy-222@5yy-qj7-7t9", 
                                "zzy-222@5yy-pdp-d7q") | placekey %in% c("zzy-222@5yy-qhq-5mk", "zzy-222@5yy-qj7-7t9", "zzy-222@5yy-qhs-7kf",
                                                                         "zzy-227@5yy-q7x-jn5", "zzy-226@5yy-q7x-jn5", "zzy-222@5yy-p9p-wc5",
                                                                         "255-222@5yy-q7m-7nq", "223-222@5yy-q9m-z75", "zzy-222@5yy-qyc-4sq", 
                                                                         "zzy-222@5yy-pdp-d7q", "zzy-222@5yy-qhs-7wk", "zzw-223@5yy-q7x-jn5", 
                                                                         "zzw-222@5yy-q7z-syv", "zzy-228@5yy-q7x-jn5", 
                                                                         "zzy-223@5yy-q7x-jn5", "zzy-222@5yy-q7d-zfz", "zzy-222@5yy-qyy-g49", 
                                                                         "zzy-225@5yy-q7x-jn5", "zzy-224@5yy-q7x-jn5")) %>%
  collect()


cr_visits_check_geo <- cr_visits %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(cr_visits_check_geo)

saveRDS(cr_visits, "/data/kfloersheim/tc_nps/build/cache/cr_visits_months.rds")
