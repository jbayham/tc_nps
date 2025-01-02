#General Grant National Memorial
p_load(mapview, arrow, tools, data.table)

gg_pt <- data.frame(lon = c(-73.964741, -73.962388),
                     lat = c(40.812749,40.814218))

gg_pt_geo <- gg_pt %>% 
  st_as_sf(x=.,coords=c("lon","lat"), crs=4326)
mapview(gg_pt_geo)

sw_lat_gg=gg_pt[1,2]
ne_lat_gg=gg_pt[2,2]
sw_lon_gg=gg_pt[1,1]
ne_lon_gg=gg_pt[2,1]

pat_con <- open_dataset("/data/restricted_data/dewey/advan_monthly_patterns_parquet")

gg_test_1 <- pat_con %>%
  filter(date_range_start==as_datetime("2023-07-01"),
         longitude>=sw_lon_gg,longitude<=ne_lon_gg,latitude>=sw_lat_gg,latitude<=ne_lat_gg) %>%
  collect()

gg_test1_geo <- gg_test_1 %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(gg_test1_geo)

gg_test_2 <- pat_con %>%
  filter(date_range_start==as_datetime("2022-06-01"),
         longitude>=sw_lon_gg,longitude<=ne_lon_gg,latitude>=sw_lat_gg,latitude<=ne_lat_gg) %>%
  collect()

gg_test2_geo <- gg_test_2 %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(gg_test2_geo)

gg_test_3 <- pat_con %>%
  filter(date_range_start==as_datetime("2021-08-01"),
         longitude>=sw_lon_gg,longitude<=ne_lon_gg,latitude>=sw_lat_gg,latitude<=ne_lat_gg) %>%
  collect()

gg_test_4 <- pat_con %>%
  filter(date_range_start==as_datetime("2019-10-01"),
         longitude>=sw_lon_gg,longitude<=ne_lon_gg,latitude>=sw_lat_gg,latitude<=ne_lat_gg) %>%
  collect()

gg_visits <- pat_con %>%
  filter(placekey %in% c("zzy-222@627-s4s-djv", "zzy-226@627-s4s-zmk") | parent_placekey == "zzy-222@627-s4s-djv") %>%
  collect()

gg_visits_geo <- gg_visits %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(gg_visits_geo)

saveRDS(gg_visits, "/data/kfloersheim/tc_nps/build/cache/gg_visits_months.rds")
