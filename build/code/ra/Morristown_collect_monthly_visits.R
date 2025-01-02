#Morristown National Historical Park
p_load(mapview, arrow, tools, data.table)

mt_pt <- data.frame(lon = c(-74.568433, -74.465824),
                    lat = c(40.73666,40.800027))

mt_pt_geo <- mt_pt %>% 
  st_as_sf(x=.,coords=c("lon","lat"), crs=4326)
mapview(mt_pt_geo)

sw_lat_mt=mt_pt[1,2]
ne_lat_mt=mt_pt[2,2]
sw_lon_mt=mt_pt[1,1]
ne_lon_mt=mt_pt[2,1]

pat_con <- open_dataset("/data/restricted_data/dewey/advan_monthly_patterns_parquet")

mt_test_1 <- pat_con %>%
  filter(date_range_start==as_datetime("2023-07-01"),
         longitude>=sw_lon_mt,longitude<=ne_lon_mt,latitude>=sw_lat_mt,latitude<=ne_lat_mt) %>%
  collect()

mt_1 <- mt_test_1 %>%
  select(-open_hours, -visits_by_day, -visitor_home_cbgs, -visitor_home_aggregation, -visitor_daytime_cbgs, 
         -bucketed_dwell_times, -related_same_day_brand, -related_same_month_brand, -popularity_by_hour,
         -popularity_by_day) %>%
  collect()

mt_test1_geo <- mt_1 %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(mt_test1_geo)

mt_test_1_filtered <- pat_con %>%
  filter(date_range_start==as_datetime("2023-07-01"),
         longitude>=sw_lon_mt,longitude<=ne_lon_mt,latitude>=sw_lat_mt,latitude<=ne_lat_mt,
         naics_code %in% c("712120", "712190"),
         location_name != "Audobuon Society" & location_name != "New Jersey Audubon's Scherman Hoffman Wildlife Sanctuary" &
           location_name != "Morristown National Guard Armory" & location_name != "Foote Park" & location_name != "Lidgerwood Park" &
           location_name != "Foote's Pond Wood" & location_name != "Bob Tracy Park" & location_name != "Victor Woodhill Park" &
           location_name != "Vail Mansion Lawn" & location_name != "Morristown Green" & location_name != "Burnham Park Jones Woods Trails" &
           location_name != "Burnham Park" & location_name != "Klietman Woods" & location_name != "Budd Street Recreation Area" &
           location_name != "Lewis Morris County Park" & location_name != "Dismal Harmony Natural Area" &
           location_name != "Harding Twp Historical Society") %>%
  collect()

mt_1_filter <- mt_test_1_filtered %>%
  select(-open_hours, -visits_by_day, -visitor_home_cbgs, -visitor_home_aggregation, -visitor_daytime_cbgs, 
         -bucketed_dwell_times, -related_same_day_brand, -related_same_month_brand, -popularity_by_hour,
         -popularity_by_day) %>%
  collect()

mt_test1_filter_geo <- mt_1_filter %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(mt_test1_filter_geo)

mt_test_2 <- pat_con %>%
  filter(date_range_start==as_datetime("2022-06-01"),
         longitude>=sw_lon_mt,longitude<=ne_lon_mt,latitude>=sw_lat_mt,latitude<=ne_lat_mt,
         naics_code %in% c("712120", "712190"),
         location_name != "Audobuon Society" & location_name != "New Jersey Audubon's Scherman Hoffman Wildlife Sanctuary" &
           location_name != "Morristown National Guard Armory" & location_name != "Foote Park" & location_name != "Lidgerwood Park" &
           location_name != "Foote's Pond Wood" & location_name != "Bob Tracy Park" & location_name != "Victor Woodhill Park" &
           location_name != "Vail Mansion Lawn" & location_name != "Morristown Green" & location_name != "Burnham Park Jones Woods Trails" &
           location_name != "Burnham Park" & location_name != "Klietman Woods" & location_name != "Budd Street Recreation Area" &
           location_name != "Lewis Morris County Park" & location_name != "Dismal Harmony Natural Area" &
           location_name != "Harding Twp Historical Society") %>%
  collect()

mt_visit_test2 <- mt_test_2 %>%
  select(-open_hours, -visits_by_day, -visitor_home_cbgs, -visitor_home_aggregation, -visitor_daytime_cbgs, 
         -bucketed_dwell_times, -related_same_day_brand, -related_same_month_brand, -popularity_by_hour,
         -popularity_by_day) %>%
  collect()

mt_visits_test2_geo <- mt_visit_test2 %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(mt_visits_test2_geo)

mt_test_3 <- pat_con %>%
  filter(date_range_start==as_datetime("2021-08-01"),
         naics_code %in% c("712120", "712190"),
         location_name != "Audobuon Society" & location_name != "New Jersey Audubon's Scherman Hoffman Wildlife Sanctuary" &
           location_name != "Morristown National Guard Armory" & location_name != "Foote Park" & location_name != "Lidgerwood Park" &
           location_name != "Foote's Pond Wood" & location_name != "Bob Tracy Park" & location_name != "Victor Woodhill Park" &
           location_name != "Vail Mansion Lawn" & location_name != "Morristown Green" & location_name != "Burnham Park Jones Woods Trails" &
           location_name != "Burnham Park" & location_name != "Klietman Woods" & location_name != "Budd Street Recreation Area" &
           location_name != "Lewis Morris County Park" & location_name != "Dismal Harmony Natural Area" &
           location_name != "Harding Twp Historical Society",
         longitude>=sw_lon_mt,longitude<=ne_lon_mt,latitude>=sw_lat_mt,latitude<=ne_lat_mt) %>%
  collect()

mt_test_4 <- pat_con %>%
  filter(date_range_start==as_datetime("2019-10-01"),
         naics_code %in% c("712120", "712190"),
         location_name != "Audobuon Society" & location_name != "New Jersey Audubon's Scherman Hoffman Wildlife Sanctuary" &
           location_name != "Morristown National Guard Armory" & location_name != "Foote Park" & location_name != "Lidgerwood Park" &
           location_name != "Foote's Pond Wood" & location_name != "Bob Tracy Park" & location_name != "Victor Woodhill Park" &
           location_name != "Vail Mansion Lawn" & location_name != "Morristown Green" & location_name != "Burnham Park Jones Woods Trails" &
           location_name != "Burnham Park" & location_name != "Klietman Woods" & location_name != "Budd Street Recreation Area" &
           location_name != "Lewis Morris County Park" & location_name != "Dismal Harmony Natural Area" &
           location_name != "Harding Twp Historical Society",
         longitude>=sw_lon_mt,longitude<=ne_lon_mt,latitude>=sw_lat_mt,latitude<=ne_lat_mt) %>%
  collect()

mt_visits <- pat_con %>%
  filter(naics_code %in% c("712120", "712190"),
         location_name != "Audobuon Society" & location_name != "New Jersey Audubon's Scherman Hoffman Wildlife Sanctuary" &
           location_name != "Morristown National Guard Armory" & location_name != "Foote Park" & location_name != "Lidgerwood Park" &
           location_name != "Foote's Pond Wood" & location_name != "Bob Tracy Park" & location_name != "Victor Woodhill Park" &
           location_name != "Vail Mansion Lawn" & location_name != "Morristown Green" & location_name != "Burnham Park Jones Woods Trails" &
           location_name != "Burnham Park" & location_name != "Klietman Woods" & location_name != "Budd Street Recreation Area" &
           location_name != "Lewis Morris County Park" & location_name != "Dismal Harmony Natural Area" &
           location_name != "Harding Twp Historical Society",
         longitude>=sw_lon_mt,longitude<=ne_lon_mt,latitude>=sw_lat_mt,latitude<=ne_lat_mt) %>%
  collect()

mt_visits_geo <- mt_visits %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(mt_visits_geo)

saveRDS(mt_visits, "/data/kfloersheim/tc_nps/build/cache/mt_visits_months.rds")
