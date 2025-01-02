#Arches National Park
p_load(mapview, arrow, tools, data.table)

arrow_con <-open_dataset("/data/restricted_data/dewey/safegraph_core_geometry_parquet")
glimpse(arrow_con)

arches_pt <- data.frame(lon = c(-109.799807, -109.448368),
                        lat = c(38.572869, 38.853929))

arches_pt_geo <- arches_pt %>% 
  st_as_sf(x=.,coords=c("lon","lat"), crs=4326)

mapview(arches_pt_geo)

sw_lat_arches=arches_pt[1,2]
ne_lat_arches=arches_pt[2,2]
sw_long_arches=arches_pt[1,1]
ne_long_arches=arches_pt[2,1]

arches_pois <- arrow_con %>%
  filter(iso_country_code=="US", longitude>=sw_long_arches,longitude<=ne_long_arches,latitude>=sw_lat_arches,latitude<=ne_lat_arches) %>%
  collect()

arches_pois_geo <- arches_pois %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(arches_pois_geo)

arches_pois_filter <- arches_pois_geo %>%
  filter(naics_code %in% c("713110","712120","712190","7211")) %>%
  collect()

arches_placekeys <- arches_pois_filter %>%
  filter(parent_placekey %in% c("zzy-222@5qg-8sr-mtv", "zzy-227@5qg-4zk-9vf") | placekey %in% c("zzy-227@5qg-4zk-9vf", "226-225@5qg-52z-ffz", "zzy-222@5qg-8sr-mtv",
                                                                                                "zzy-222@5qg-8sw-jvz", "228-223@5qg-54m-vzz", "zzy-22g@5qg-532-8jv", 
                                                                                                "zzy-222@5qg-8rj-k75", "zzy-222@5qg-54m-sh5", "zzy-222@5qg-8sx-cdv", 
                                                                                                "zzy-222@5qg-8sx-w6k", "zzy-222@5qg-8qg-nwk", "zzy-222@5qg-8rj-xh5",
                                                                                                "zzy-222@5qg-8q7-cnq", "zzy-228@5qg-4zk-9vf", "228-224@5qg-532-9xq")) %>%
  collect()

arches_placekeys_geo <- arches_placekeys %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(arches_placekeys_geo)

arches_placekeys_corrected <- arches_pois_filter %>%
  filter(parent_placekey %in% c("zzy-222@5qg-8sr-mtv", "zzy-227@5qg-4zk-9vf") | placekey %in% c("zzy-227@5qg-4zk-9vf", "zzy-222@5qg-8sr-mtv", "zzy-222@5qg-8sw-jvz", 
                                                                                                "zzy-222@5qg-8rj-k75", "zzy-222@5qg-8sx-cdv", "zzy-222@5qg-8sx-w6k", 
                                                                                                "zzy-222@5qg-8qg-nwk", "zzy-222@5qg-8rj-xh5",
                                                                                                "zzy-222@5qg-8q7-cnq", "zzy-228@5qg-4zk-9vf")) %>%
  collect()

arches_placekeys_corrected_geo <- arches_placekeys_corrected %>%
  filter(!is.na(polygon_wkt))%>%
  st_as_sf(wkt = "polygon_wkt", crs=4326)
mapview(arches_placekeys_corrected_geo)

saveRDS(arches_pois, "/data/kfloersheim/tc_nps/build/cache/arches_pois.rds")
saveRDS(arches_pois_filter, "/data/kfloersheim/tc_nps/build/cache/arches_pois_filter.rds")
saveRDS(arches_placekeys, "/data/kfloersheim/tc_nps/build/cache/arches_placekeys.rds")
saveRDS(arches_placekeys_corrected, "/data/kfloersheim/tc_nps/build/cache/arches_placekeys_corrected.rds")

pat_con <- open_dataset("/data/restricted_data/dewey/advan_monthly_patterns_parquet")

arches_visits_test <- pat_con %>% 
  filter(date_range_start==as_datetime("2023-07-01"), parent_placekey %in% c("zzy-222@5qg-8sr-mtv", "zzy-227@5qg-4zk-9vf") | 
           placekey %in% c("zzy-227@5qg-4zk-9vf", "zzy-222@5qg-8sr-mtv", "zzy-222@5qg-8sw-jvz", 
                     "zzy-222@5qg-8rj-k75", "zzy-222@5qg-8sx-cdv", "zzy-222@5qg-8sx-w6k", 
                    "zzy-222@5qg-8qg-nwk", "zzy-222@5qg-8rj-xh5", "zzy-222@5qg-8q7-cnq", "zzy-228@5qg-4zk-9vf")) %>%
  collect()

arches_visits <- pat_con %>% 
  filter(parent_placekey %in% c("zzy-222@5qg-8sr-mtv", "zzy-227@5qg-4zk-9vf") | 
           placekey %in% c("zzy-227@5qg-4zk-9vf", "zzy-222@5qg-8sr-mtv", "zzy-222@5qg-8sw-jvz", 
                           "zzy-222@5qg-8rj-k75", "zzy-222@5qg-8sx-cdv", "zzy-222@5qg-8sx-w6k", 
                           "zzy-222@5qg-8qg-nwk", "zzy-222@5qg-8rj-xh5", "zzy-222@5qg-8q7-cnq", "zzy-228@5qg-4zk-9vf")) %>%
  collect()

saveRDS(arches_visits, "/data/kfloersheim/tc_nps/build/cache/arches_visits.rds")
