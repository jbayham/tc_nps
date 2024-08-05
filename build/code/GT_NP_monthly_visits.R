#Grand Teton National Park
p_load(mapview, arrow, tools, data.table)

pat_con <- open_dataset("/data/restricted_data/dewey/advan_monthly_patterns_parquet")

gt_pt <- data.frame(lon = c(-110.971941, -110.355333),
                    lat = c(43.536375, 44.123352))

gt_pt_geo <- gt_pt %>% 
  st_as_sf(x=.,coords=c("lon","lat"), crs=4326)
mapview(gt_pt_geo)

sw_lat_gt=gt_pt[1,2]
ne_lat_gt=gt_pt[2,2]
sw_lon_gt=gt_pt[1,1]
ne_lon_gt=gt_pt[2,1]

gt_test <- pat_con %>%
  filter(date_range_start==as_datetime("2023-07-01"),
         naics_code %in% c("712190", "722511", "7212", "712110", "621493", "2774434", "922160",
                           "492110", "491110", "447", "721110", "6214", "713110", "713990", "5615",
                           "813110", "532284", "447110", "451110", "712120", "812220", "3231",
                           "488119", "484230"),
         city != "Teton Village", postal_code != "83025", location_name != "Headwaters Lodge & Cabins at Flagg Ranch" &
           location_name != "Bedroll and Breakfast" & location_name != "Hatchet Resort" & location_name != "Diamond Cross Ranch" &
           location_name != "Luton's Teton Cabins" & location_name != "Heart Six Guest Ranch" & location_name != "Sheffields Restaurant" &
           location_name != "The Whetstone" & location_name != "Grand Teton Park RV Resort" & location_name != "Gros Ventre River Ranch" &
           location_name != "North Grill" & location_name != "Corsa" & location_name != "Malaka's" & 
           location_name != "John D Rockefeller Jr Memorial Parkway" & location_name != "Grand Targhee" &
           location_name != "Jackson Hole" & location_name != "Lost Creek Ranch And Spa" & location_name != "Teton Range Resort" &
           location_name != "Teton Canyon Campground" & location_name != "Teton Camping" & location_name != "Wild Bill's Grill" &
           location_name != "Fireside Buffalo Valley" & location_name != "Upper Teton View Camping Area",
         placekey != "zzy-222@5wg-qcm-jd9" & placekey != "222-222@5qp-y5t-k4v" & placekey != "zzw-222@5qp-t6q-r8v" & 
           placekey != "zzy-22m@5qp-t6y-d35" & placekey != "222-222@5qq-25z-rrk" & placekey != "zzw-223@5qq-25z-rp9",
         iso_country_code=="US", longitude>=sw_lon_gt,longitude<=ne_lon_gt,latitude>=sw_lat_gt,latitude<=ne_lat_gt) %>%
  collect()

gt_1 <- gt_test %>%
  select(-open_hours, -visits_by_day, -visitor_home_cbgs, -visitor_home_aggregation, -visitor_daytime_cbgs, 
         -bucketed_dwell_times, -related_same_day_brand, -related_same_month_brand, -popularity_by_hour,
         -popularity_by_day) %>%
  collect()

gt_visits_test1_geo <- gt_1 %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(gt_visits_test1_geo)

gt_check1 <- gt_test %>%
  filter(city == "Teton Village") %>%
  collect()

gt_check1_geo <- gt_check1 %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(gt_check1_geo)

gt_check2 <- gt_test %>%
  filter(city == "Alta") %>%
  collect()

gt_check2_geo <- gt_check2 %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(gt_check2_geo)

gt_test_2 <- pat_con %>%
  filter(date_range_start==as_datetime("2022-06-01"),
         naics_code %in% c("712190", "722511", "7212", "712110", "621493", "2774434", "922160",
                           "492110", "491110", "447", "721110", "6214", "713110", "713990", "5615",
                           "813110", "532284", "447110", "451110", "712120", "812220", "3231",
                           "488119", "484230"),
         city != "Teton Village", postal_code != "83025", location_name != "Headwaters Lodge & Cabins at Flagg Ranch" &
           location_name != "Bedroll and Breakfast" & location_name != "Hatchet Resort" & location_name != "Diamond Cross Ranch" &
           location_name != "Luton's Teton Cabins" & location_name != "Heart Six Guest Ranch" & location_name != "Sheffields Restaurant" &
           location_name != "The Whetstone" & location_name != "Grand Teton Park RV Resort" & location_name != "Gros Ventre River Ranch" &
           location_name != "North Grill" & location_name != "Corsa" & location_name != "Malaka's" & 
           location_name != "John D Rockefeller Jr Memorial Parkway" & location_name != "Grand Targhee" &
           location_name != "Jackson Hole" & location_name != "Lost Creek Ranch And Spa" & location_name != "Teton Range Resort" &
           location_name != "Teton Canyon Campground" & location_name != "Teton Camping" & location_name != "Wild Bill's Grill" &
           location_name != "Fireside Buffalo Valley" & location_name != "Upper Teton View Camping Area",
         placekey != "zzy-222@5wg-qcm-jd9" & placekey != "222-222@5qp-y5t-k4v" & placekey != "zzw-222@5qp-t6q-r8v" & 
           placekey != "zzy-22m@5qp-t6y-d35" & placekey != "222-222@5qq-25z-rrk" & placekey != "zzw-223@5qq-25z-rp9",
         iso_country_code=="US", longitude>=sw_lon_gt,longitude<=ne_lon_gt,latitude>=sw_lat_gt,latitude<=ne_lat_gt) %>%
  collect()

gt_visit_test2 <- gt_test_2 %>%
  select(-open_hours, -visits_by_day, -visitor_home_cbgs, -visitor_home_aggregation, -visitor_daytime_cbgs, 
         -bucketed_dwell_times, -related_same_day_brand, -related_same_month_brand, -popularity_by_hour,
         -popularity_by_day) %>%
  collect()

gt_visits_test2_geo <- gt_visit_test2 %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(gt_visits_test2_geo)

gt_test_3 <- pat_con %>%
  filter(date_range_start==as_datetime("2021-08-01"),
         naics_code %in% c("712190", "722511", "7212", "712110", "621493", "2774434", "922160",
                           "492110", "491110", "447", "721110", "6214", "713110", "713990", "5615",
                           "813110", "532284", "447110", "451110", "712120", "812220", "3231",
                           "488119", "484230"),
         city != "Teton Village", postal_code != "83025", location_name != "Headwaters Lodge & Cabins at Flagg Ranch" &
           location_name != "Bedroll and Breakfast" & location_name != "Hatchet Resort" & location_name != "Diamond Cross Ranch" &
           location_name != "Luton's Teton Cabins" & location_name != "Heart Six Guest Ranch" & location_name != "Sheffields Restaurant" &
           location_name != "The Whetstone" & location_name != "Grand Teton Park RV Resort" & location_name != "Gros Ventre River Ranch" &
           location_name != "North Grill" & location_name != "Corsa" & location_name != "Malaka's" & 
           location_name != "John D Rockefeller Jr Memorial Parkway" & location_name != "Grand Targhee" &
           location_name != "Jackson Hole" & location_name != "Lost Creek Ranch And Spa" & location_name != "Teton Range Resort" &
           location_name != "Teton Canyon Campground" & location_name != "Teton Camping" & location_name != "Wild Bill's Grill" &
           location_name != "Fireside Buffalo Valley" & location_name != "Upper Teton View Camping Area",
         placekey != "zzy-222@5wg-qcm-jd9" & placekey != "222-222@5qp-y5t-k4v" & placekey != "zzw-222@5qp-t6q-r8v" & 
           placekey != "zzy-22m@5qp-t6y-d35" & placekey != "222-222@5qq-25z-rrk" & placekey != "zzw-223@5qq-25z-rp9",
         iso_country_code=="US", longitude>=sw_lon_gt,longitude<=ne_lon_gt,latitude>=sw_lat_gt,latitude<=ne_lat_gt) %>%
  collect()

gt_test_4 <- pat_con %>%
  filter(date_range_start==as_datetime("2019-10-01"),
         naics_code %in% c("712190", "722511", "7212", "712110", "621493", "2774434", "922160",
                           "492110", "491110", "447", "721110", "6214", "713110", "713990", "5615",
                           "813110", "532284", "447110", "451110", "712120", "812220", "3231",
                           "488119", "484230"),
         city != "Teton Village", postal_code != "83025", location_name != "Headwaters Lodge & Cabins at Flagg Ranch" &
           location_name != "Bedroll and Breakfast" & location_name != "Hatchet Resort" & location_name != "Diamond Cross Ranch" &
           location_name != "Luton's Teton Cabins" & location_name != "Heart Six Guest Ranch" & location_name != "Sheffields Restaurant" &
           location_name != "The Whetstone" & location_name != "Grand Teton Park RV Resort" & location_name != "Gros Ventre River Ranch" &
           location_name != "North Grill" & location_name != "Corsa" & location_name != "Malaka's" & 
           location_name != "John D Rockefeller Jr Memorial Parkway" & location_name != "Grand Targhee" &
           location_name != "Jackson Hole" & location_name != "Lost Creek Ranch And Spa" & location_name != "Teton Range Resort" &
           location_name != "Teton Canyon Campground" & location_name != "Teton Camping" & location_name != "Wild Bill's Grill" &
           location_name != "Fireside Buffalo Valley" & location_name != "Upper Teton View Camping Area",
         placekey != "zzy-222@5wg-qcm-jd9" & placekey != "222-222@5qp-y5t-k4v" & placekey != "zzw-222@5qp-t6q-r8v" & 
           placekey != "zzy-22m@5qp-t6y-d35" & placekey != "222-222@5qq-25z-rrk" & placekey != "zzw-223@5qq-25z-rp9",
         iso_country_code=="US", longitude>=sw_lon_gt,longitude<=ne_lon_gt,latitude>=sw_lat_gt,latitude<=ne_lat_gt) %>%
  collect()

gt_visits <- pat_con %>%
  filter(naics_code %in% c("712190", "722511", "7212", "712110", "621493", "2774434", "922160",
                           "492110", "491110", "447", "721110", "6214", "713110", "713990", "5615",
                           "813110", "532284", "447110", "451110", "712120", "812220", "3231",
                           "488119", "484230"),
         city != "Teton Village", postal_code != "83025", location_name != "Headwaters Lodge & Cabins at Flagg Ranch" &
           location_name != "Bedroll and Breakfast" & location_name != "Hatchet Resort" & location_name != "Diamond Cross Ranch" &
           location_name != "Luton's Teton Cabins" & location_name != "Heart Six Guest Ranch" & location_name != "Sheffields Restaurant" &
           location_name != "The Whetstone" & location_name != "Grand Teton Park RV Resort" & location_name != "Gros Ventre River Ranch" &
           location_name != "North Grill" & location_name != "Corsa" & location_name != "Malaka's" & 
           location_name != "John D Rockefeller Jr Memorial Parkway" & location_name != "Grand Targhee" &
           location_name != "Jackson Hole" & location_name != "Lost Creek Ranch And Spa" & location_name != "Teton Range Resort" &
           location_name != "Teton Canyon Campground" & location_name != "Teton Camping" & location_name != "Wild Bill's Grill" &
           location_name != "Fireside Buffalo Valley" & location_name != "Upper Teton View Camping Area",
         placekey != "zzy-222@5wg-qcm-jd9" & placekey != "222-222@5qp-y5t-k4v" & placekey != "zzw-222@5qp-t6q-r8v" & 
           placekey != "zzy-22m@5qp-t6y-d35" & placekey != "222-222@5qq-25z-rrk" & placekey != "zzw-223@5qq-25z-rp9",
         iso_country_code=="US", longitude>=sw_lon_gt,longitude<=ne_lon_gt,latitude>=sw_lat_gt,latitude<=ne_lat_gt) %>%
  collect()

gt_visits_correct <- gt_visits %>%
  filter(placekey != "zzy-222@5qp-zmb-wkz" & placekey != "zzy-222@5qp-t6y-gzf") %>%
  collect()

gt_visits_geo <- gt_visits_correct %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(gt_visits_geo)

saveRDS(gt_visits_correct, "/data/kfloersheim/tc_nps/build/cache/gt_visits_months.rds")
