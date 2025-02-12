#This script assembles the dataset for the cell TCM

library(pacman)
p_load(tidyverse,conflicted,readxl)

source("project_init.R")

conflicts_prefer(lubridate::month)
conflicts_prefer(dplyr::filter)
######################
#Read in weekly visits by park and ct or origin
parks_home_tract <- readRDS("build/cache/parks_home_tract_t1.rds") %>%
  rename(measure_date = date_range_start) #%>%
#mutate(survey_sample = measure_date %within% c((as_date("2021-04-01") %--% as_date("2022-06-30"))),
#      panel_no = as.integer((year(measure_date) - 2019) * 2 + (month(measure_date) - 1) %/% 6 + 1))

#Read in census income and xwalk. Rowbinding and letting xwalk data have precedent
census_data <- readRDS("build/cache/census_data_2022.rds") %>%
  rename(tract=geoid)

xwalk_data <- readRDS("build/cache/xwalk_data_2022.rds")

census_xwalk <- bind_rows(xwalk_data,census_data) %>%
  distinct(tract,.keep_all = TRUE)


#Read in travel distance and time
# tc_raw <- read_csv("build/cache/AEcensus_final.csv") %>%
#   mutate(ct = str_pad(tract_id,11,"left","0"),
#          trav_miles = conv_unit(dist_km,"km","mi")) #converting from km to miles
tc_raw <- read_csv("build/cache/google_dist.csv")


#Read in devices residing in each tract
tract_devices <- read_csv("build/inputs/tract_devices.csv") %>%
  mutate(measure_date = as_date(paste0(year,"-",mon,"-01")))



###########################################
#Building datasets for each year


# using the discounted travel cost; truncated at 2 visits
# trunc_replace=4
# discount_tc=T
# drange_filter=c((as_date("2021-04-01") %--% as_date("2022-06-30")))

#placekey_list <- sort(unique(parks_home_tract$placekey))

travel_cost_calc <- function(drange_filter,
                             inc_measure="med"){
  
  
  
  #Filter by date to match survey, deal with truncation, aggregate across parks
  visitors <- parks_home_tract %>%
    filter(measure_date %within% drange_filter) %>% #cutting data by date range
    group_by(placekey,tract) %>%
    summarize(visits=sum(visits,na.rm = T)) %>%  #summing over ct and date still may produce some over estimation because there are 6 pois representing the kuw
    ungroup()
  
  #Filtering tract devices
  tract_devices_sub <- tract_devices %>%
    filter(measure_date %within% drange_filter) %>% #cutting data by date range
    group_by(tract) %>%
    summarize(residing = mean(residing,na.rm = T)) %>%
    ungroup()
  
  #Join with census tract data (income, age, ...)
  reg_data <- visitors %>%
    left_join(tract_devices_sub,by = c("tract")) %>%
    left_join(census_xwalk,by = c("tract")) %>%
    left_join(tc_raw,by = c("placekey","tract")) 
  
  
  #Keep record of recods los in join
  unmatched <- reg_data %>%
    filter(if_any(everything(),list(is.na,is.nan)))
  
  #choose which income measure to use
  if(inc_measure=="med") { #use median household income
    reg_data$income = reg_data$med_hh_inc
  } else {
    reg_data$income = reg_data$perc_inc
  }
  
  reg_final <- reg_data %>%
    filter(!if_any(everything(),list(is.na,is.nan))) %>%
    mutate(travel_time_cost = 2*(1/3*income/(40*50)*trav_time),  #travel cost = 1/3 hourly wage (annual salary = hh_inc/2000) + .59 * miles; round trip so 2x mileage
           travel_cost = (.59*trav_dist*2)/2,  #multiplying by 2 for round trip; without opportunity cost of time
           travel_total_cost = (travel_time_cost + travel_cost)) 
  
  return(list(reg_final,unmatched))
}

#################
#For time series
#construct all dataset combinations
param_grid <- expand_grid(inc_measure="med",
                          drange_filter=c(#(as_date("2018-01-01") %--% as_date("2018-06-30")),
                                          #(as_date("2018-07-01") %--% as_date("2018-12-31")),
                                          (as_date("2019-01-01") %--% as_date("2019-06-30")),
                                          (as_date("2019-07-01") %--% as_date("2019-12-31")),
                                          (as_date("2020-01-01") %--% as_date("2020-06-30")),
                                          (as_date("2020-07-01") %--% as_date("2020-12-31")),
                                          (as_date("2021-01-01") %--% as_date("2021-06-30")),
                                          (as_date("2021-07-01") %--% as_date("2021-12-31")),
                                          (as_date("2022-01-01") %--% as_date("2022-06-30")),
                                          (as_date("2022-07-01") %--% as_date("2022-12-31")),
                                          (as_date("2023-01-01") %--% as_date("2023-06-30")),
                                          (as_date("2023-07-01") %--% as_date("2023-12-31")))) %>%
  mutate(fname=paste0(year(int_start(drange_filter)),month(int_start(drange_filter)),"-",year(int_end(drange_filter)),month(int_end(drange_filter)),".csv"))

group_split(param_grid,fname) %>%
  walk(function(df){
    final <- travel_cost_calc(
                              drange_filter=df$drange_filter,
                              inc_measure=df$inc_measure) 
    
    final[[1]] %>%
      write_csv(paste0("analysis/inputs/ts_reg_dat_",df$fname))
    
    #final[[2]] %>%
    #write_csv(paste0("analysis/inputs/",df$um_fname))
  })

###########################################
#Plotting distribution of visits by state (move to EDA)

parks_home_tract %>%
  group_by(st=str_sub(tract,1,2)) %>%
  summarize(visits=sum(visits,na.rm = TRUE)) %>% View()
ggplot(aes(y=reorder(st,visits),x=visits)) +
  geom_col()

###########################################
#Caching data by tract and week to investigate the consequence of summing over truncated values

visitors <- parks_home_tract %>%
  filter(measure_date %within% drange_filter) %>% #cutting data to match survey date range
  mutate(visits=ifelse(visits==4,trunc_replace,visits)) %>% #conservative imputation method replacing 4 with trunc_replace
  group_by(ct,measure_date) %>%
  summarize(visits=sum(visits,na.rm = T)) %>%  #summing over ct and date still may produce some over estimation because there are 6 pois representing the kuw
  ungroup()

reg_data <- visitors %>%
  inner_join(select(tract_devices,-daytime),by = c("ct","measure_date")) %>%
  inner_join(census_data,by = "ct") %>%
  inner_join(tc_reg,by = "ct") %>%
  filter(!if_any(everything(),list(is.na,is.nan))) %>%
  mutate(travel_time_cost = 2*(1/3*med_hh_inc/(40*50)*trav_hours)*trip_fraction,  #travel cost = 1/3 hourly wage (annual salary = hh_inc/2000) + .59 * miles; round trip so 2x mileage
         travel_cost = (.59*trav_miles*2)*trip_fraction,  #multiplying by 2 for round trip; without opportunity cost of time
         travel_total_cost = travel_time_cost + travel_cost) 

write_csv(reg_data,"analysis/inputs/weekly_reg_dat.csv")




################################################

#construct all dataset combinations
param_grid <- expand_grid(trunc_replace=c(2,4),
                          discount_tc=c(TRUE,FALSE),
                          drange_filter=c((as_date("2021-04-01") %--% as_date("2022-06-30"))),
                          inc_measure=c("med","perc")) %>%
  mutate(fname=paste0("trunc_",trunc_replace,"_discount_",discount_tc,"_income_",inc_measure,"_",year(int_start(drange_filter)),"-",year(int_end(drange_filter)),".csv"),
         um_fname = paste0("unmatched_",fname))

group_split(param_grid,fname,um_fname) %>%
  walk(function(df){
    final <- travel_cost_calc(
      trunc_replace=df$trunc_replace,
      discount_tc=df$discount_tc,
      drange_filter=df$drange_filter,
      inc_measure=df$inc_measure) 
    
    final[[1]] %>%
      write_csv(paste0("analysis/inputs/reg_dat_",df$fname))
    
    final[[2]] %>%
      write_csv(paste0("analysis/inputs/",df$um_fname))
    
  })



