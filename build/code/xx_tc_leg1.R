#This script queries OSRM to get origin destination travel time and distance
#from all census tracts and zip codes to airports

library(pacman)
p_load(tidyverse,sf,janitor,measurements,progress,osrm,furrr,arrow,here)

message("here() root: ", here::here())
i_am("build/code/xx_tc_leg1.R")
source(here("project_init.R"))

# Read in data of all unique origins
census_geo <- readRDS(here("build", "cache", "census_geo_points_2019.rds")) %>%
  rename(tract = geoid)

# Read in airports
airports_geo <- read_csv(here("build", "cache", "airports_geo.csv"))


if(interactive()){
  plan(multisession(workers = 10)) 
} else {
  plan(multicore(workers = 10))
}
 


# Set OSRM server
options(osrm.server = "http://darecompute-01.aggie.colostate.edu:5000/", osrm.profile = "car")

# Break census tracts into chunks of 450
census_chunks <- split(census_geo, ceiling(seq_along(census_geo$tract) / 450))

# airport_row=airports_geo[1,]
# chunk=census_chunks[[1]]
# i=1

dir_ifnot(here("build", "cache", "fly", "leg1"))

# Function to calculate and cache OD matrix for one airport
process_airport <- function(airport_row) {
  airport_code <- airport_row$code
  airport_id <- paste0("dest_", airport_code)
  
  dest_coords <- tibble(
    lon = airport_row$lon,
    lat = airport_row$lat
  )
  
  message("Processing airport: ", airport_code)
  
  future_walk2(census_chunks, seq_along(census_chunks), function(chunk, i) {
    chunk_id <- paste0("chunk_", sprintf("%03d", i))
    out_path <- here("build", "cache", "fly", "leg1", paste0(airport_code, "_", chunk_id, ".parquet"))
    
    if (file.exists(out_path)) {
      message("Skipping cached file: ", out_path)
      return(NULL)
    }
    
    src_coords <- chunk %>%
      transmute(lon = longitude, lat = latitude)
    
    result <- tryCatch({
      osrmTable(src = src_coords, 
                dst = dest_coords,
                measure = c("duration","distance"))    
      }, error = function(e) {
      message("Error: ", e$message)
      return(NULL)
    })
    
    if (!is.null(result)) {
      dist_df <- tibble(
        tract = chunk$tract,
        airport = airport_code,
        duration_hr = conv_unit(as.vector(result$durations),from = "min",to = "hour"),
        distance_mi = conv_unit(as.vector(result$distances),from = "m", to = "mi")
      ) %>%
        bind_cols(rename(result$sources,tract_lon=lon,tract_lat=lat)) %>%
        add_column(airport_lon=result$destinations$lon,
                   airport_lat=result$destinations$lat)
      
      write_parquet(dist_df, out_path)
      message("Wrote: ", out_path)
    }
  })
}

# Process all airports
walk(split(airports_geo, seq(nrow(airports_geo))), process_airport)
