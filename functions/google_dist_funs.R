

get_google_od <- 
  function(df,
         cache_dir="mobile",
         progress=TRUE){
  #Function to query google maps api
  require(mapsapi)
  require(dplyr)
  
  
  ###########
  message(paste0("Starting group ",df$group[1]))
  
  if(progress) pb$tick()
  
  
  #Check that all destinations are same
  if(nrow(df)>1 & var(df$dest_lon)!=0) stop("Not all destinations are the same.")
  
  #Query maps API
  doc = mp_matrix(
    origins = as.matrix(df[,c("orig_lon","orig_lat")]),
    destinations = as.matrix(df[1,c("dest_lon","dest_lat")]),
    mode = "driving",
    key = key,
    quiet = T
  )
  
  #Extract distance component and convert meters to miles and seconds to hours
  m = conv_unit(mp_get_matrix(doc, value = "distance_m"),"m","mi")
  s = conv_unit(mp_get_matrix(doc, value = "duration_s"),"sec","hr")
  
  #Build output df
  out <- bind_cols(
    df,
    rownames_to_column(as.data.frame(m),var = "origin_label") %>%
      rename(trav_dist = 2) %>%
      mutate(trav_time = as.numeric(s),
             dest_label = attr(s,"dimnames")[[2]])
  )
  
  
  #Sys.sleep(5)
  saveRDS(out,paste0("build/cache/google_dist/",cache_dir,"/group_",
                     str_pad(out$group[1],3,"left","0"),"_",
                     generate_random_string(length=5),".rds"))
  
  }
