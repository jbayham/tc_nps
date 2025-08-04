#This script assembles the dataset for the cell TCM

library(pacman)
p_load(tidyverse,conflicted,readxl)

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


##############################
#Cost params
wage_frac = 1/3
work_hours = 40*51 #51 40-hour weeks
AAA <- c(`2022` = 0.2767,
         `2023` = 0.2576)
hotelrate <- c(`2022` = 149.24,
               `2023` = 155.47)



#Flight params
fly_USDperKM_2018 <- (0.13+0.14+0.14+0.16+0.20+0.20+0.21+0.25+0.26+0.28+0.29+0.54+0.68)/13
fly_USDpermile <- c(`2022`=(fly_USDperKM_2018 * 292.7/251.1) / 0.621371, #CPI deflator and KM to Mi conversion
                    `2023`=(fly_USDperKM_2018 * 304.7/251.1) / 0.621371)



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
  
  #Setting cost params that vary by year
  d_tc = AAA[as.character(park_temp$year[1])]
  #f_tc = fly_USDpermile[as.character(park_temp$year[1])]
  hr = hotelrate[as.character(park_temp$year[1])]
  fee_type=park_temp$fee_type[1]
  site_fee=park_temp$site_fee[1]

  
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
  visitors <- parks_home_tract %>%
    filter(placekey == park_temp$placekey,             #cutting data by placekey
           measure_date %within% date_range) %>% #cutting data by date range
    arrange(measure_date,tract) %>%
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
    mutate(
      wage_rate = income / work_hours,
      cost_opp_hr = wage_frac * wage_rate,
      cost_drive = 2 * (distance_mi1 + distance_mi3) * d_tc/nsplit,
      travel_time = duration_hr1 + duration_hr3 + fly_time,
      hotelnights = 2*floor(travel_time / 12) * hr, #hotel stays for long trips assuming 12 hour driving days
      cost_time = 2 * cost_opp_hr * travel_time + hotelnights,
      cost_fly = 2 * fare,
      total_flight_cost = cost_drive + cost_time + cost_fly
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
  
  #choose which income measure to use
  # if(inc_measure=="med") { #use median household income
  #   reg_data$income = reg_data$med_hh_inc
  # } else {
  #   reg_data$income = reg_data$perc_inc
  # }
  
  reg_final <- reg_data %>%
    filter(!if_any(everything(),list(is.na,is.nan))) %>%
    mutate(cost_opp = wage_frac*(income/work_hours), #opportunity cost of time $/hour
           hotelnights = 2*floor(osrm_ow_hrs / 12) * hr, #hotel stays for long trips assuming 12 hour driving days
           cost_d_opp = 2*(cost_opp*osrm_ow_hrs),  #travel cost = 1/3 hourly wage (annual salary = hh_inc/2000) + .59 * miles; round trip so 2x mileage
           cost_d_travel = 2*d_tc*osrm_ow_miles/nsplit,  #multiplying by 2 for round trip; without opportunity cost of time
           cost_d_total = cost_d_opp + cost_d_travel + hotelnights,
           #cost_f_opp = 2*(cost_opp*f_time),
           #cost_f_travel = 2*f_tc*f_distance,
           #cost_f_total = cost_f_opp + cost_f_travel,
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
check <- travel_cost_calc(park_code = "AZRU")
summary(select(check[[1]],visits,nsplit,fly_prob,cost_total_weighted))
# check <- travel_cost_calc(pk = "zzz-222@5qf-fyv-zfz")
# check <- travel_cost_calc(park_code = "AZRU",dates = (as_date("2019-01-01") %--% as_date("2019-06-30")))

#################
#Dataset for comparison with SEM data (using fly prob and nsplit predictions)

pk_list <- unique(park_subset$placekey)
dir_name = "analysis/inputs/regs_dem"
dir_ifnot(dir_name)

pk_list %>%
  walk(function(x){
    final <- travel_cost_calc(
      pk=x,
      fly_prob = "fly_prob_dem",
      nsplit = "nsplit_dem") 
    
    fname = paste0(final[[1]]$code_dest[1],"_",int_start(final[[1]]$drange[1]),"-",int_end(final[[1]]$drange[1]),".rds")
    
    final[[1]] %>%
      saveRDS(paste0(dir_name,"/reg_dat_",fname))
    
    #final[[2]] %>%
    #write_csv(paste0("analysis/inputs/",df$um_fname))
  })

##########################################
#Dataset for comparison with SEM data (using fly prob and nsplit=2)

pk_list <- unique(park_subset$placekey)
dir_name = "analysis/inputs/regs_nopred"
dir_ifnot(dir_name)

pk_list %>%
  walk(function(x){
    final <- travel_cost_calc(
      pk=x,
      fly_prob = F,
      nsplit = 2) 
    
    fname = paste0(final[[1]]$code_dest[1],"_",int_start(final[[1]]$drange[1]),"-",int_end(final[[1]]$drange[1]),".rds")
    
    final[[1]] %>%
      saveRDS(paste0(dir_name,"/reg_dat_",fname))
    
    #final[[2]] %>%
    #write_csv(paste0("analysis/inputs/",df$um_fname))
  })

##############################
#For time series
#construct all dataset combinations

dlist <- tibble(sample_end = seq.Date(from=as_date("2020-07-01"),to=as_date("2024-07-01"),by = "1 year")) %>%
  mutate(sample_start = sample_end - months(11),
         drange = sample_start %--% sample_end) 

for(i in 1:nrow(dlist)){
  dname = paste0("analysis/inputs/date_",dlist$sample_start[i])
  dir_ifnot(dir_name = dname)
  
  pk_list <- unique(park_subset$placekey)
  
  walk(pk_list,function(x){
    final <- travel_cost_calc(
      pk=x,
      dates = dlist$drange[i],
      fly_prob = "fly_prob_dem",
      nsplit = "nsplit_dem") 
    
    fname = paste0(final[[1]]$code_dest[1],"_",int_start(final[[1]]$drange[1]),"-",int_end(final[[1]]$drange[1]),".rds")
    
    final[[1]] %>%
      saveRDS(paste0(dname,"/reg_dat_",fname))
  })
  
}


