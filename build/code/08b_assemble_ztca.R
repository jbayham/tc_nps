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

#Read in ZTCA data from Aaron - most distance discrepency is from Isle Royale but overall correlation is 99%
survey_dat <- read_csv("build/inputs/AE/SEMvMobile_12_osrm.csv") %>%
  select(-c(starts_with("trav"),ends_with("lat"),ends_with("lon"))) %>%
  mutate(id = row_number(),
         n_split_predmissing = coalesce(n_split,pred_nsplit,n_split_noNAs),
         fly_predmissing = coalesce(m_mode_air,pred_flyprob),
         income_predmissing = coalesce(income_hh,income_hh_noNAs,census_income),
         n_split_impflag = case_when(
           !is.na(n_split) ~ F,
           is.na(n_split)  ~ T
           ),
         fly_impflag = case_when(
           !is.na(m_mode_air) ~ F,
           is.na(m_mode_air)  ~ T,
         ),
         income_impflag = case_when(
           !is.na(income_hh) ~ F,
           is.na(income_hh)  ~ T
         ))
  




#Read in cached leg1: census tract to airports
leg1_ds <- readRDS("build/cache/zip_to_airpot10.rds") %>%
  select(ztca=zip,airport1_code=airport,duration_hr1=duration_hr,distance_mi1=distance_mi) #%>%
  #inner_join(distinct(parks_home_tract,tract),by = join_by(tract))

#Read in leg3: airports to final destinations
leg3_ds <- open_dataset("build/cache/fly/leg3") %>%
  select(code_dest,airport2_code=airport,duration_hr3=duration_hr,distance_mi3=distance_mi) %>%
  collect() %>%
  group_by(code_dest) %>%
  slice_min(order_by = distance_mi3,n=10,with_ties = FALSE) %>% #subsetting only the top 10 nearest airports to park
  ungroup() %>%
  filter(!(code_dest %in% c("FOLA_02","GRTE_03","GUMO_02","MORR_01"))) %>%
  mutate(code_dest = str_remove(code_dest,"_.*")) #removing the extra park code for multi-park areas


#Read in fare estimates between all airports (leg2)
airfare_df <- read_csv("build/cache/airfares.csv") %>%
  select(airport1_code,airport2_code,fare=flyfare_predict_2022,nsmiles) %>%
  mutate(fly_time = nsmiles/528) #distance divided by speed of plane


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
#placekey_list <- sort(unique(parks_home_tract$placekey))

parkcode="FOLA"

travel_cost_calc_ztca <- 
  function(parkcode,
           imp_model = "imp_only" #options are obs_only, obs_imp, imp_only
           ){
  
  #Check if user input a park code and not a placekey
    park_temp <- park_subset %>%
      filter(code == parkcode) %>%
      select(code,site_fee,fee_type,year) %>%
      distinct(code,.keep_all = TRUE)
  
  #Joining location days to park data
  park_temp <- select(loc_days,parkcode,n_locdays_median) %>%
    left_join(park_temp,.,by=c("code"="parkcode")) %>%
    mutate(n_locdays_median=coalesce(n_locdays_median,1))
  
  #Setting cost params that vary by year
  d_tc = AAA[as.character(park_temp$year[1])]
  hr = hotel_rate[as.character(park_temp$year[1])]
  fee_type=park_temp$fee_type[1]
  site_fee=park_temp$site_fee[1]
  site_loc_days = park_temp$n_locdays_median[1]

  
  #TC params - use chosen type and reassign variable name
  if(imp_model == "imp_only"){ 
    survey_prepped <- survey_dat %>%
      filter(parkcode == park_temp$code) %>%
      group_by(zcta_final) %>%
      summarize(across(c(parkcode,year,starts_with("census")),first),
                across(c(trips_12mos,trips_5yrs),sum),
                across(c(pred_flyprob,pred_nsplit,distance_miles_osrm,time_hours_osrm),mean)) %>%
      mutate(id=row_number()) %>%
      rename(income=census_income,
             fly_prob=pred_flyprob,
             nsplit=pred_nsplit)
  } else {
    survey_prepped <- survey_dat %>%
      filter(parkcode == park_temp$code) %>%
      mutate(
        fly_prob = case_when(
          imp_model == "obs_only" ~ as.numeric(m_mode_air),
          imp_model == "obs_imp"  ~ as.numeric(fly_predmissing),
          imp_model == "imp_only" ~ pred_flyprob
        ),
        nsplit = case_when(
          imp_model == "obs_only" ~ as.numeric(n_split),
          imp_model == "obs_imp"  ~ as.numeric(n_split_predmissing),
          imp_model == "imp_only" ~ pred_nsplit
        ),
        income = case_when(
          imp_model == "obs_only" ~ as.numeric(income_hh),
          imp_model == "obs_imp"  ~ as.numeric(income_predmissing),
          imp_model == "imp_only" ~ census_income
        )
      )
  }
  
  
  
  
  #Merge the flight and drive distance and time with socioeconomic data and calculate total flight cost
  air_df <- leg1_ds %>%
    filter(ztca %in% survey_prepped$zcta_final) %>%
    inner_join(airfare_df,by=c("airport1_code"),relationship = "many-to-many") %>%
    inner_join(filter(leg3_ds,code_dest==park_temp$code),
               by=c("airport2_code"),relationship = "many-to-many") %>%
    left_join(parking_cost_df,by=c("airport1_code"="airport_code")) %>%
    left_join(rental_car_df,by=c("airport2_code"="airport_code")) %>%
    left_join(survey_prepped, by = c("ztca"="zcta_final"),relationship = "many-to-many") %>% #could be many to many because individuals from same zip could go have different mode choices
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
    group_by(id,ztca) %>%
    slice_min(order_by = total_flight_cost, n = 1) %>%
    ungroup() %>%
    select(id, ztca, total_flight_cost) %>%
    drop_na()
    
  
  #Join with census tract data (income, age, ...)
  reg_data <- survey_prepped %>%
    left_join(select(park_temp,-year),by = c("parkcode"="code")) %>%
    left_join(best_routes,by = c("id","zcta_final"="ztca")) %>%
    mutate(site_fee = case_when(
      fee_type == "pv" ~ site_fee/nsplit,
      fee_type == "pp" ~ site_fee,
      is.na(fee_type) ~ 0
    ))
  
  
  #Keep record of records lost in join
  unmatched <- reg_data %>%
    filter(if_any(c(trips_12mos,total_flight_cost),list(is.na,is.nan)))
  
  
  reg_final <- reg_data %>%
    filter(!if_any(c(trips_12mos,total_flight_cost),list(is.na,is.nan))) %>%
    mutate(cost_opp = wage_frac*(income/work_hours), #opportunity cost of time $/hour
           cost_hotel = 2*floor(time_hours_osrm / 12) * hr, #hotel stays for long trips assuming 12 hour driving days
           cost_d_opp = 2*(cost_opp*time_hours_osrm),  #travel cost = 1/3 hourly wage (annual salary = hh_inc/2000) + .59 * miles; round trip so 2x mileage
           cost_d_travel = 2*d_tc*distance_miles_osrm/nsplit,  #multiplying by 2 for round trip; without opportunity cost of time
           cost_d_total = cost_d_opp + cost_d_travel + cost_hotel) %>%
    select(-c(cost_opp,cost_hotel,cost_d_opp,cost_d_travel))
  
  reg_final <- reg_final %>%
    mutate(cost_total_weighted = ((1-fly_prob)*cost_d_total + fly_prob*total_flight_cost) + site_fee)
  

  return(list(reg_final,unmatched))
}


#check <- travel_cost_calc_ztca(parkcode = "AZRU",imp_model = "imp_only")
#summary(select(check[[1]],visits,nsplit,fly_prob,cost_total_weighted))


################
# Define modeling configurations for travel cost calculation
# Each row corresponds to a distinct modeling scenario:
# - fly_prob: method for handling missing flight probability
# - nsplit: method for handling missing number of splits
# - income: method for handling missing income data
configs <- tibble(
  imp_model    = list("obs_only", "obs_imp", "imp_only")
)


# Loop over model configurations and time windows
walk(seq_len(nrow(configs)), function(j) {
  imp_model  <- configs$imp_model[[j]]
  
  message(paste0("Running travel cost calculation for model: ", imp_model))
  
  # Define output directory based on modeling choice and date
  dir_name <- paste0("analysis/inputs/AE/regs_", imp_model)         # e.g., regs_dem, regs_nopred
  
  dir_ifnot(dir_name)  # Create output directory if it doesn't exist
  
  pk_list <- unique(survey_dat$parkcode)  # List of park destinations
  
  # Loop over each park and run travel cost calculation
  walk(pk_list, function(pk) {
    message(paste0("Processing park: ", pk))
    # Conditionally include the date argument only when relevant
    final <- travel_cost_calc_ztca(parkcode = pk, imp_model = imp_model)
    
    # Construct output filename: e.g., CARE_2020-07-01_2021-06-30.rds
    fname <- paste0(final[[1]]$parkcode[1],".rds")
    
    # Save the first element of the result (regression dataset) to disk
    saveRDS(final[[1]], file = file.path(dir_name, paste0("reg_dat_", fname)))
    # Optionally, save the unmatched records for diagnostics
    saveRDS(final[[2]], file = file.path(dir_name, paste0("unmatched_", fname)))
  })

})




