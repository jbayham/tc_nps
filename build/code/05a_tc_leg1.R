library(pacman)
p_load(tidyverse, sf, janitor, measurements, progress, osrm, furrr, arrow, here)

source(here("project_init.R"))

# Choose origin type: "tract" or "zip"
origin_type <- "zip"  # Change to "tract" if using census tracts

# Load appropriate origin file
origin_geo <- switch(origin_type,
                     "tract" = readRDS(here("build", "cache", "census_geo_points_2019.rds")) %>% rename(origin_id = geoid),
                     "zip"   = readRDS(here("build", "cache", "census_geo_zip_2024.rds")) %>% rename(origin_id = geoid20))

# Load airport data
airports_geo <- read_csv(here("build", "cache", "airports_geo.csv"))

# Set OSRM server
options(osrm.server = "http://darecompute-01.aggie.colostate.edu:5000/", osrm.profile = "car")

# Set parallel plan
if (interactive()) {
  plan(multisession(workers = 10)) 
} else {
  plan(multicore(workers = 10))
}

# Determine chunking
if (nrow(origin_geo) > 450) {
  chunk_size <- 450
  origin_chunks <- split(origin_geo, ceiling(seq_along(origin_geo$origin_id) / chunk_size))
} else {
  origin_chunks <- list(origin_geo)
}

# Set output directory based on origin_type
out_dir <- here("build", "cache", "fly", paste0(origin_type, "_leg1"))
dir_ifnot(out_dir)

# Function to process one airport
process_airport <- function(airport_row) {
  airport_code <- airport_row$code
  airport_id <- paste0("dest_", airport_code)
  dest_coords <- tibble(lon = airport_row$lon, lat = airport_row$lat)
  
  message("Processing airport: ", airport_code)
  
  future_walk2(origin_chunks, seq_along(origin_chunks), function(chunk, i) {
    chunk_id <- paste0("chunk_", sprintf("%03d", i))
    out_path <- file.path(out_dir, paste0(airport_code, "_", chunk_id, ".parquet"))
    
    if (file.exists(out_path)) {
      message("Skipping cached file: ", out_path)
      return(NULL)
    }
    
    src_coords <- chunk %>% transmute(lon = longitude, lat = latitude)
    
    result <- tryCatch({
      osrmTable(src = src_coords, dst = dest_coords, measure = c("duration", "distance"))
    }, error = function(e) {
      message("Error: ", e$message)
      return(NULL)
    })
    
    if (!is.null(result)) {
      dist_df <- tibble(
        origin_id = chunk$origin_id,
        airport = airport_code,
        duration_hr = conv_unit(as.vector(result$durations), from = "min", to = "hour"),
        distance_mi = conv_unit(as.vector(result$distances), from = "m", to = "mi")
      ) %>%
        bind_cols(rename(result$sources, origin_lon = lon, origin_lat = lat)) %>%
        add_column(airport_lon = result$destinations$lon,
                   airport_lat = result$destinations$lat)
      
      write_parquet(dist_df, out_path)
      message("Wrote: ", out_path)
    }
  })
}

# Run the process
walk(split(airports_geo, seq(nrow(airports_geo))), process_airport)


#####################################
#Read in the whole dataset and reorganize it by airport

leg1_ds <- open_dataset("build/cache/fly/tract_leg1")


all_dat <- leg1_ds %>%
  collect()

#keep top 10 nearest airports to each tract
tract_top10 <- all_dat %>%
  group_by(tract) %>%
  slice_min(order_by = distance_mi,n=10,with_ties = FALSE) %>%
  ungroup()

saveRDS(tract_top10,"build/cache/tract_to_airpot10.rds")

######
leg1_ds <- open_dataset("build/cache/fly/zip_leg1")


all_dat <- leg1_ds %>%
  collect()

#keep top 10 nearest airports to each tract
zip_top10 <- all_dat %>%
  group_by(zip) %>%
  slice_min(order_by = distance_mi,n=10,with_ties = FALSE) %>%
  ungroup()

saveRDS(zip_top10,"build/cache/zip_to_airpot10.rds")
