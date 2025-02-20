

get_google_od <- 
  function(df,
         cache_dir="build/cache/google_dist/mobile",
         progress=TRUE){
  #Function to query google maps api
  require(mapsapi)
  require(dplyr)
  
  
  ###########
  message(paste0("Starting group ",df$group[1]))
  
  if(progress) pb$tick()
  
  
  #Check that all destinations are same
  if(nrow(df)>1 & var(df$dest_lon)!=0) stop("Not all destinations are the same.")
  if(!dir.exists(cache_dir)) stop(paste0(cache_dir," does not exist"))
  
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
  saveRDS(out,paste0(cache_dir,"/group_",
                     str_pad(out$group[1],3,"left","0"),"_",
                     generate_random_string(length=5),".rds"))
  
  }

#OSRM query
get_osrm_od <- 
  function(df,
           cache_dir="build/cache/osrm_dist/mobile",
           progress=TRUE){
    #Function to query osrm api
    require(osrm)
    require(dplyr)
    
    ###########
    message(paste0("Starting group ",df$group[1]))
    
    if(progress) pb$tick()
    
    #Check that all destinations are same
    if(!dir.exists(cache_dir)) stop(paste0(cache_dir," does not exist"))
    if(nrow(df)>1 & var(df$dest_lon)!=0) stop("Not all destinations are the same.")
    
    #Query osrm
    trav_temp <- osrmTable(src = as.matrix(df[,c("orig_lon","orig_lat")]),
                           dst = as.matrix(df[1,c("dest_lon","dest_lat")]),
                           measure = c('duration', 'distance'),
                           osrm.profile = "car")
    
    
    
    #Extract distance component
    m = as.numeric(conv_unit(trav_temp$distances,"m","mi"))
    s = as.numeric(conv_unit(trav_temp$durations,"min","hr"))
    
    #Build output df
    out <- bind_cols(
      df,
      tibble(trav_dist=m,
             trav_time=s))
      
    #pause to reduce load on api
    Sys.sleep(3)
      
    saveRDS(out,paste0(cache_dir,"/group_",
                       str_pad(out$group[1],3,"left","0"),"_",
                       generate_random_string(length=5),".rds"))
    
  }

st_crow_flies <- function(df,
                          fly_mph = (480+575)/2){
  #Check if all destinations are the same
  if(nrow(df)>1 & var(df$dest_lon)!=0) stop("Not all destinations are the same.")
  #Set st_distance to use lwgeom for more accurate distance calculation
  sf_use_s2(FALSE)
  
  #Origin points
  orig <- df %>%
    select(code_dest,tract,starts_with("orig")) %>%
    st_as_sf(coords = c("orig_lon","orig_lat"),crs=4326)
  
  #Destination point
  dest <- df[1,] %>%
    select(code_dest,tract,starts_with("dest")) %>%
    st_as_sf(coords = c("dest_lon","dest_lat"),crs=4326)
  
  #Calculate the distance
  calc_dist <- as.vector(st_distance(x=dest,y=orig))
  
  #Convert to miles
  out <- df %>%
    mutate(f_distance = conv_unit(calc_dist,"m","mi"),
           f_time = f_distance/fly_mph)
  
  return(out)
}

