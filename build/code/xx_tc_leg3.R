#This script queries OSRM to get origin destination travel time and distance
#from all census tracts and zip codes to airports

library(pacman)
p_load(tidyverse,sf,janitor,measurements,progress,osrm,furrr,arrow,here)


source(here("project_init.R"))

#Read in parks
park_subset <- readRDS("build/cache/park_subset.rds") %>%
  select(code,code_dest,placekey,dest_lon,dest_lat)

park_geo <- readRDS("build/cache/parks_home_tract_t1.rds") %>%
  distinct(placekey,tract) %>%
  inner_join(park_subset,by = join_by(placekey)) %>% #Join with park subset
  distinct(code_dest,dest_lon,dest_lat)

# Read in airports
airports_geo <- read_csv(here("build", "cache", "airports_geo.csv"))

###########
#Start where left off
#airports_geo = airports_geo[(which(airports_geo$code=="DAY")-1):nrow(airports_geo),]

############


# Set OSRM server
options(osrm.server = "http://darecompute-01.aggie.colostate.edu:5000/", osrm.profile = "car")


airport_row=airports_geo[1,]
chunk=park_geo
i=1

dir_ifnot(here("build", "cache", "fly", "leg3"))

# Function to calculate and cache OD matrix for one airport
process_airport <- function(airport_row) {
  airport_code <- airport_row$code
  airport_id <- paste0("dest_", airport_code)
  
  dest_coords <- tibble(
    lon = airport_row$lon,
    lat = airport_row$lat
  )
  
  message("Processing airport: ", airport_code)
  
  chunk=park_geo
    chunk_id <- paste0("chunk")
    out_path <- here("build", "cache", "fly", "leg3", paste0(airport_code, "_", chunk_id, ".parquet"))
    
    if (file.exists(out_path)) {
      message("Skipping cached file: ", out_path)
      return(NULL)
    }
    
    src_coords <- chunk %>%
      transmute(lon = dest_lon, lat = dest_lat)
    
    result <- tryCatch({
      osrmTable(src = dest_coords, #Note: switching origins and destinations because code organized around airports
                dst = src_coords,
                measure = c("duration","distance"))    
      }, error = function(e) {
      message("Error: ", e$message)
      return(NULL)
    })
    
    if (!is.null(result)) {
      dist_df <- tibble(
        #tract = chunk$tract,
        code_dest = chunk$code_dest,
        airport = airport_code,
        duration_hr = conv_unit(as.vector(result$durations),from = "min",to = "hour"),
        distance_mi = conv_unit(as.vector(result$distances),from = "m", to = "mi")
      ) %>%
        #bind_cols(rename(result$sources,tract_lon=lon,tract_lat=lat)) %>%
        bind_cols(rename(result$destinations,park_lon=lon,park_lat=lat)) %>%
        add_column(airport_lon=result$sources$lon,
                   airport_lat=result$sources$lat)
      
      write_parquet(dist_df, out_path)
      message("Wrote: ", out_path)
    }
  
}

# Process all airports
walk(split(airports_geo, seq(nrow(airports_geo))), process_airport)


# leg3_con <- open_dataset("build/cache/fly/leg3")
# 
# tc_leg3 <- leg3_con %>%
#   collect()
