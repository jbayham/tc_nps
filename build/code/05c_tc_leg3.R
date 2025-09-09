library(pacman)
p_load(tidyverse, sf, janitor, measurements, progress, osrm, furrr, arrow, here)

source(here("project_init.R"))

# Load airports
airports_geo <- read_csv(here("build", "cache", "airports_geo.csv"))

# Load park destinations
park_subset <- readRDS(here("build", "cache", "park_subset.rds")) %>%
  select(code, code_dest, placekey, dest_lon, dest_lat)

park_geo <- readRDS(here("build", "cache", "parks_home_tract_t1.rds")) %>%
  distinct(placekey, tract) %>%
  inner_join(park_subset, by = join_by(placekey)) %>%
  distinct(code_dest, dest_lon, dest_lat)

# Set OSRM server
options(osrm.server = "http://darecompute-01.aggie.colostate.edu:5000/", osrm.profile = "car")

# Create output directory
out_dir <- here("build", "cache", "fly", "leg3")
dir_ifnot(out_dir)

# Optional chunking if needed for large park sets
if (nrow(park_geo) > 450) {
  chunk_size <- 450
  park_chunks <- split(park_geo, ceiling(seq_along(park_geo$code_dest) / chunk_size))
  plan(multisession(workers = 10))
} else {
  park_chunks <- list(park_geo)
  plan(sequential)
}

# Function to calculate travel time/distance from one airport to all parks
process_airport <- function(airport_row) {
  airport_code <- airport_row$code
  src_coords <- tibble(lon = airport_row$lon, lat = airport_row$lat)
  
  message("Processing airport: ", airport_code)
  
  furrr::future_walk2(park_chunks, seq_along(park_chunks), function(chunk, i) {
    chunk_id <- paste0("chunk_", sprintf("%03d", i))
    out_path <- file.path(out_dir, paste0(airport_code, "_", chunk_id, ".parquet"))
    
    if (file.exists(out_path)) {
      message("Skipping cached file: ", out_path)
      return(NULL)
    }
    
    dest_coords <- chunk %>% transmute(lon = dest_lon, lat = dest_lat)
    
    result <- tryCatch({
      osrmTable(src = src_coords, dst = dest_coords, measure = c("duration", "distance"))
    }, error = function(e) {
      message("Error: ", e$message)
      return(NULL)
    })
    
    if (!is.null(result)) {
      dist_df <- tibble(
        code_dest = chunk$code_dest,
        airport = airport_code,
        duration_hr = conv_unit(as.vector(result$durations), from = "min", to = "hour"),
        distance_mi = conv_unit(as.vector(result$distances), from = "m", to = "mi")
      ) %>%
        bind_cols(rename(result$destinations, park_lon = lon, park_lat = lat)) %>%
        add_column(airport_lon = result$sources$lon, airport_lat = result$sources$lat)
      
      write_parquet(dist_df, out_path)
      message("Wrote: ", out_path)
    }
  })
}

# Run for all airports
walk(split(airports_geo, seq(nrow(airports_geo))), process_airport)

# Optional: collect output
# leg3_con <- open_dataset(here("build", "cache", "fly", "leg3"))
# tc_leg3 <- leg3_con %>% collect()
