#This script assembles the dataset for the cell TCM

library(pacman)
p_load(tidyverse,conflicted,janitor,readxl,arrow,janitor)

source("project_init.R")

conflicts_prefer(lubridate::month)
conflicts_prefer(dplyr::filter)
######################
#Load park subset if not loaded
park_subset <- readRDS("build/cache/park_subset.rds") %>%
  filter(primary==1) %>%
  mutate(sample_end = sample_period - months(11),
         drange = sample_end %--% sample_period)

#Read in weekly visits by park and ct or origin
parks_home_tract <- readRDS("build/cache/parks_home_tract_t1.rds") %>%
  rename(measure_date = date_range_start) #%>%


#Read in census income and xwalk. Rowbinding and letting xwalk data have precedent
census_data <- readRDS("build/cache/census_data_2023.rds") %>%
  rename(tract=geoid)

xwalk_data <- readRDS("build/cache/xwalk_data_2023.rds")

census_xwalk <- bind_rows(xwalk_data,census_data) %>%
  distinct(tract,.keep_all = TRUE) %>%
  select(tract,income=med_inc,age=med_age,householdsize=hh_size) %>%
  filter(if_all(income:householdsize,~.>0)) 


#Read in travel distance and time
trav_dist_time <- readRDS("build/cache/mobile_osrm_dist.rds") %>%
  select(code_dest,tract,osrm_ow_miles=trav_dist,osrm_ow_hrs=trav_time) %>%
  inner_join(readRDS("build/cache/flight_dist.rds"),by=c("code_dest","tract"))


#Model predicted probabilities and TC split number
fly_nsplit <- readRDS("build/cache/fly_nsplit.rds") %>%
  select(code_dest,tract,starts_with("fly"),starts_with("nsplit")) %>%
  mutate(across(where(is.numeric),as.numeric))


#Read in devices residing in each tract
tract_devices <- read_csv("build/inputs/tract_devices.csv") %>%
  mutate(measure_date = as_date(paste0(year,"-",mon,"-01")))

#Read in cached leg1: census tract to airports
leg1_ds <- readRDS("build/cache/tract_to_airpot10.rds") %>%
  select(tract,airport1_code=airport,duration_hr1=duration_hr,distance_mi1=distance_mi) %>%
  inner_join(distinct(parks_home_tract,tract),by = join_by(tract))

#Read in leg3: airports to final destinations
leg3_ds <- open_dataset("build/cache/fly/leg3") %>%
  select(code_dest,airport2_code=airport,duration_hr3=duration_hr,distance_mi3=distance_mi) %>%
  collect() %>%
  group_by(code_dest) %>%
  slice_min(order_by = distance_mi3,n=10,with_ties = FALSE) %>% #subsetting only the top 10 nearest airports to park
  ungroup()

#Read in fare estimates between all airports
airfare_df <- read_csv("build/cache/airfares.csv") %>%
  select(airport1_code,airport2_code,fare=flyfare_predict_2022,nsmiles) %>%
  mutate(fly_time = nsmiles/528) #distance divided by speed of plane

#Hotel costs
# hotel_costs_df <- readRDS("build/cache/hotel_costs.rds") %>%
#   select(measure_date,seasonal_ahla)

#Rental car prices
rental_car_df <- readRDS("build/cache/rental_car_prices.csv")
  #select(iata,daily_rental_cost=daily_price)

#Parking fees
parking_cost_df <- readRDS("build/cache/parking_prices.csv")
  #select(iata,daily_parking_cost=daily_price)

#Days on location
loc_days <- read_csv("build/inputs/AE/n_locdays.csv")

##############################
#Cost params
wage_frac = 1/3
work_hours = 40*51 #51 40-hour weeks
AAA <- c(`2022` = 0.2767,
         `2023` = 0.2576)
hotel_rate = c(`2019` = 131.56,
               `2020` = 103.28,
               `2021` = 124.71,
               `2022` = 149.5,
               `2023` = 155.94,
               `2024` = 159) #from https://www.ahla.com/sites/default/files/25_SOTI.pdf



###########################################
#Building datasets for each year


# using the discounted travel cost; truncated at 2 visits
# trunc_replace=4
# discount_tc=T
# drange_filter=c((as_date("2021-04-01") %--% as_date("2022-06-30")))

#placekey_list <- sort(unique(parks_home_tract$placekey))

park_code="BADL"

travel_cost_calc <- function(pk,
                             park_code,
                             dates,  #Must be a date range object
                             fly_prob = "fly_prob_nodem",
                             nsplit = "nsplit_nodem"){
  
  #Check if user input a park code and not a placekey
  if(missing(pk) & !missing(park_code)){
    park_temp <- park_subset %>%
      filter(code_dest == park_code) %>%
      select(code,code_dest,park,placekey,site_fee,fee_type,year,drange)
  } 
  #Check if user input a placekey and not a park code
  if(!missing(pk) & missing(park_code)){
    park_temp <- park_subset %>%
      filter(placekey == pk) %>%
      select(code,code_dest,park,placekey,site_fee,fee_type,year,drange)
  }
  
  #Joining location days to park data
  park_temp <- select(loc_days,parkcode,n_locdays_median) %>%
    left_join(park_temp,.,by=c("code_dest"="parkcode")) %>%
    mutate(n_locdays_median=coalesce(n_locdays_median,1))
  
  #Setting cost params that vary by year
  d_tc = AAA[as.character(park_temp$year[1])]
  hr = hotel_rate[as.character(park_temp$year[1])]
  fee_type=park_temp$fee_type[1]
  site_fee=park_temp$site_fee[1]
  site_loc_days = park_temp$n_locdays_median[1]

  
  #Setting date params based on SEM range or for all dates
  if(missing(dates)){
    date_range = park_temp$drange
  } else {
    date_range = dates
  }
  
  #TC params - use chosen type and reassign variable name
  if(fly_prob!=FALSE){
    fly_param <- fly_nsplit %>%
      select(code_dest,tract,fly_prob=all_of(fly_prob))
  } else {
    fly_param <- fly_nsplit %>%
      select(code_dest,tract) %>%
      mutate(fly_prob=fly_prob)
  }
  
  if(is.character(nsplit)){
    if(!(nsplit %in% names(fly_nsplit))) stop("nsplit is not a valid variable name")
    nsplit_param <- fly_nsplit %>%
      select(code_dest,tract,nsplit=all_of(nsplit))
  } else {
    if(!is.numeric(nsplit)) stop("nsplit is not numeric")
    nsplit_param <- fly_nsplit %>%
      select(code_dest,tract) %>%
      mutate(nsplit=nsplit)
      
  }
  
  

  
  #Filter by date to match survey, deal with truncation, aggregate across parks
  visitors_time <- parks_home_tract %>%
    filter(placekey == park_temp$placekey,             #cutting data by placekey
           measure_date %within% date_range) %>% #cutting data by date range
    arrange(measure_date,tract) 
  
  #Aggregating visits across time
  visitors <- visitors_time %>%
    group_by(placekey,tract) %>%
    summarize(visits=sum(visits,na.rm = T)) %>%  #summing over ct and date still may produce some over estimation because there are 6 pois representing the kuw
    ungroup() %>%
    mutate(code_dest = park_temp$code_dest)
  
  #Filtering tract devices
  tract_devices_sub <- tract_devices %>%
    filter(tract %in% visitors$tract,
           measure_date %within% date_range) %>% #cutting data by date range
    group_by(tract) %>%
    summarize(residing = mean(residing,na.rm = T)) %>%
    ungroup()
  
  #Merge the flight and drive distance and time with socioeconomic data and calculate total flight cost
  air_df <- leg1_ds %>%
    filter(tract %in% visitors$tract) %>%
    inner_join(airfare_df,by=c("airport1_code"),relationship = "many-to-many") %>%
    inner_join(filter(leg3_ds,code_dest==park_temp$code_dest),
               by=c("airport2_code"),relationship = "many-to-many") %>%
    left_join(census_xwalk, by = "tract") %>%
    left_join(nsplit_param,by = c("code_dest","tract")) %>%
    left_join(parking_cost_df,by=c("airport1_code"="airport_code")) %>%
    left_join(rental_car_df,by=c("airport2_code"="airport_code")) %>%
    mutate(
      wage_rate = income / work_hours,
      cost_opp_hr = wage_frac * wage_rate,
      travel_time = duration_hr1 + duration_hr3, # + fly_time,
      cost_hotel = 2*floor(travel_time / 12) * hr, #hotel stays for long trips assuming 12 hour travel days
      cost_time = 2 * (cost_opp_hr * (travel_time + fly_time + 2)),
      cost_drive = 2 * (distance_mi1 + distance_mi3) * d_tc/nsplit,
      cost_fly = 2 * fare,
      cost_park_rent = site_loc_days * (parking_price_daily + rental_price_daily),
      total_flight_cost = cost_drive + cost_time + cost_fly + cost_hotel + cost_park_rent
    )
  
  #Identify the minimum route cost across flight options
  best_routes <- air_df %>%
    group_by(tract) %>%
    slice_min(order_by = total_flight_cost, n = 1) %>%
    ungroup() %>%
    drop_na() %>%
    select(tract, total_flight_cost)
  
  
  #Join with census tract data (income, age, ...)
  reg_data <- visitors %>%
    left_join(tract_devices_sub,by = c("tract")) %>%
    left_join(census_xwalk,by = c("tract")) %>%
    left_join(trav_dist_time %>% select(-c(f_distance,f_time)),by = c("code_dest","tract")) %>%
    left_join(best_routes,by = c("tract")) %>%
    left_join(fly_param,by = c("code_dest","tract")) %>%
    left_join(nsplit_param,by = c("code_dest","tract")) %>%
    mutate(site_fee = case_when(
      fee_type == "pv" ~ site_fee/nsplit,
      fee_type == "pp" ~ site_fee,
      is.na(fee_type) ~ 0
    ))
  
  
  #Keep record of records lost in join
  unmatched <- reg_data %>%
    filter(if_any(everything(),list(is.na,is.nan)))
  
  
  reg_final <- reg_data %>%
    filter(!if_any(everything(),list(is.na,is.nan))) %>%
    mutate(cost_opp = wage_frac*(income/work_hours), #opportunity cost of time $/hour
           cost_hotel = 2*floor(osrm_ow_hrs / 12) * hr, #hotel stays for long trips assuming 12 hour driving days
           cost_d_opp = 2*(cost_opp*osrm_ow_hrs),  #travel cost = 1/3 hourly wage (annual salary = hh_inc/2000) + .59 * miles; round trip so 2x mileage
           cost_d_travel = 2*d_tc*osrm_ow_miles/nsplit,  #multiplying by 2 for round trip; without opportunity cost of time
           cost_d_total = cost_d_opp + cost_d_travel + cost_hotel,
           drange=date_range) 
  
  if(fly_prob==FALSE){
    reg_final <- reg_final %>%
      rowwise() %>%
      mutate(cost_total_weighted = min(cost_d_total,total_flight_cost) + site_fee)
  } else {
    reg_final <- reg_final %>%
      mutate(cost_total_weighted = ((1-fly_prob)*cost_d_total + fly_prob*total_flight_cost) + site_fee)
  }

  
  return(list(reg_final,unmatched))
}


check <- travel_cost_calc(park_code = "AZRU",fly_prob = F)
summary(select(check[[1]],visits,nsplit,fly_prob,cost_total_weighted))


################
# Define modeling configurations for travel cost calculation
# Each row corresponds to a distinct modeling scenario:
# - fly_prob: name of column used to impute probability of flying, or FALSE to disable
# - nsplit: name of column or constant for number of people splitting costs
# - suffix: identifier used to label the output directory
# - time_series: TRUE if we want to generate a time series of datasets
configs <- tibble(
  fly_prob    = list("fly_prob_dem", "fly_prob_nodem", FALSE, "fly_prob_dem"),
  nsplit      = list("nsplit_dem", "nsplit_nodem", 2, "nsplit_dem"),
  suffix      = c("dem", "nodem", "nopred", "dem"),
  time_series = c(FALSE, FALSE, FALSE, TRUE)
)


# Loop over model configurations and time windows
walk(seq_len(nrow(configs)), function(j) {
  fly  <- configs$fly_prob[[j]]
  split <- configs$nsplit[[j]]
  suffix <- configs$suffix[[j]]
  time_series <- configs$time_series[[j]]
  
  # Create a table of date ranges (trailing 12-month windows)
  # Only used if time_series = TRUE for the current config
  date_ranges <- if (time_series) {
    tibble(sample_end = seq(as_date("2020-07-01"), as_date("2024-07-01"), by = "1 year")) %>%
      mutate(
        sample_start = sample_end - months(11),
        drange = sample_start %--% sample_end
      )
  } else {
    tibble(drange = NA)
  }
  
  # Loop over each date window (or a single pass if time_series = FALSE)
  walk(seq_len(nrow(date_ranges)), function(i) {
    dr <- date_ranges$drange[i]
    
    # Define output directory based on modeling choice and date
    dir_name <- if (time_series) {
      paste0("analysis/inputs/date_", int_start(dr))  # e.g., date_2020-07-01
    } else {
      paste0("analysis/inputs/regs_", suffix)         # e.g., regs_dem, regs_nopred
    }
    
    dir_ifnot(dir_name)  # Create output directory if it doesn't exist
    
    pk_list <- unique(park_subset$placekey)  # List of park destinations
    
    
    # Loop over each park and run travel cost calculation
    walk(pk_list, function(pk) {
      # Conditionally include the date argument only when relevant
      final <- if (!is.na(dr)) {
        travel_cost_calc(pk = pk, fly_prob = fly, nsplit = split, dates = dr)
      } else {
        travel_cost_calc(pk = pk, fly_prob = fly, nsplit = split)
      }
      
      # Construct output filename: e.g., CARE_2020-07-01_2021-06-30.rds
      fname <- paste0(
        final[[1]]$code_dest[1], "_",
        int_start(final[[1]]$drange[1]), "-", int_end(final[[1]]$drange[1]),
        ".rds"
      )
      
      # Save the first element of the result (regression dataset) to disk
      saveRDS(final[[1]], file = file.path(dir_name, paste0("reg_dat_", fname)))
    })
  })
})




