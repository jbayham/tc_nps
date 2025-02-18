#Get travel distance and time

#install.packages("mapsapi")
library(pacman)
p_load(tidyverse,sf,janitor,measurements,progress,readxl,modelsummary)

source("project_init.R")

#########################
#Read data

driving <- readRDS("build/cache/mobile_osrm_dist.rds") %>%
  select(code_dest,tract,d_miles_oneway=trav_dist)

# flight_calc <- readRDS("build/cache/flight_dist.rds") %>%
#   select(code_dest,tract,d_miles_oneway=f_distance)

#Read in census income and xwalk. Rowbinding and letting xwalk data have precedent
census_data <- readRDS("build/cache/census_data_2022.rds") %>%
  rename(tract=geoid)

xwalk_data <- readRDS("build/cache/xwalk_data_2022.rds")

census_xwalk <- bind_rows(xwalk_data,census_data) %>%
  distinct(tract,.keep_all = TRUE) %>%
  select(tract,income=med_inc,age=med_age,householdsize=hh_size) %>%
  filter(if_all(income:householdsize,~.>0)) 

#DF for predict regressions
mobile_df <- driving %>%
  #filter(code_dest=="AZRU") %>%
  inner_join(census_xwalk,by="tract") %>%
  mutate(year = factor(2023, levels = levels(old_df$`as.factor(year)`)),
         code=str_extract(code_dest,"[:alpha:]{4}"),.before = "code_dest") %>%
  arrange(code,code_dest,tract)

#####################
#Construct data in structure that will work with predict

mobile_split <- mobile_df %>%
  group_split(code)

p_list <- unique(mobile_df$code)

dest=p_list[1]
df=mobile_split[[1]]
prob_df <- map2(mobile_split,
                p_list,
                function(df,dest){
  
  #Checking park code matches
  if(df$code[1] == dest) stop("Data does not match the park code")
                  
  #Loading the flying prob prediction models
  ae_fly_dem_all = readRDS(paste0("build/inputs/AE/Reg_Fly/reg_fly_dem_allsites.rds")) #Flight model with demographics all sites
  ae_fly_nodem_all = readRDS(paste0("build/inputs/AE/Reg_Fly/reg_fly_nodem_allsites.rds"))
  if(file.exists(paste0("build/inputs/AE/Reg_Fly/reg_fly_nodem_",dest,".rds"))){
    ae_fly_nodem = readRDS(paste0("build/inputs/AE/Reg_Fly/reg_fly_nodem_",dest,".rds")) #Flight model no demographics
  } else {
    ae_fly_nodem = ae_fly_nodem_all
  }
  if(file.exists(paste0("build/inputs/AE/Reg_Fly/reg_fly_dem_",dest,".rds"))){
    ae_fly_dem = readRDS(paste0("build/inputs/AE/Reg_Fly/reg_fly_dem_",dest,".rds")) #Flight model with demographics
  } else {
    ae_fly_dem = ae_fly_dem_all
  }
  
  
  ae_nsplit_dem_all = readRDS(paste0("build/inputs/AE/Reg_nSplit/reg_nsplit_pois_dem_allsites.rds"))
  ae_nsplit_nodem_all = readRDS(paste0("build/inputs/AE/Reg_nSplit/reg_nsplit_pois_nodem_allsites.rds"))
  if(file.exists(paste0("build/inputs/AE/Reg_nSplit/reg_nsplit_pois_nodem_",dest,".rds"))){
    ae_nsplit_nodem = readRDS(paste0("build/inputs/AE/Reg_nSplit/reg_nsplit_pois_nodem_",dest,".rds")) #Number splitting costs model no demographics
  } else {
    ae_nsplit_nodem = ae_nsplit_nodem_all
  }
  if(file.exists(paste0("build/inputs/AE/Reg_nSplit/reg_nsplit_pois_dem_",dest,".rds"))){
    ae_nsplit_dem = readRDS(paste0("build/inputs/AE/Reg_nSplit/reg_nsplit_pois_dem_",dest,".rds")) #Number splitting costs model no demographics
  } else {
    ae_nsplit_dem = ae_nsplit_dem_all
  }
  
  ae_nsplit_nodem_nb_all = readRDS(paste0("build/inputs/AE/Reg_nSplit/reg_nsplit_negb_nodem_allsites.rds"))
  
  #Use models to predict probabilities and number of people to split costs
  out <- df %>%
    mutate(fly_prob_nodem = predict(object = ae_fly_nodem,type = "response",newdata = df),
           fly_prob_dem = predict(object = ae_fly_dem,type = "response",newdata = df),
           fly_prob_dem_all = predict(object = ae_fly_dem_all,type = "response",newdata = df),
           nsplit_nodem = predict(object = ae_nsplit_nodem,type = "response",newdata = df),
           nsplit_dem = predict(object = ae_nsplit_dem,type = "response",newdata = df),
           nsplit_dem_all = predict(object = ae_nsplit_dem_all,type = "response",newdata = df),
           nsplit_nodem_nb_all = predict(object = ae_nsplit_nodem_nb_all,type = "response",newdata = df))
  
  return(out)
})

prob_df <- bind_rows(prob_df)

saveRDS(prob_df,"build/cache/fly_nsplit.rds")


###############################
#No Dems check threshold
ae_flight_mod_nodem <- readRDS("build/inputs/AE/Reg_Fly/reg_fly_nodem_allsites.rds")



summary(driving$d_miles_oneway)

dist_only <- driving %>%
  select(d_miles_oneway) %>%
  mutate(year = factor(2023, levels = levels(old_df$`as.factor(year)`)))

dist_only <- tibble(d_miles_oneway = seq.int(from=0,to=5000,by=1)) %>%
  mutate(year = factor(2022, levels = levels(old_df$`as.factor(year)`))) %>%
  bind_rows(tibble(d_miles_oneway = seq.int(from=0,to=5000,by=1)) %>%
              mutate(year = factor(2023, levels = levels(old_df$`as.factor(year)`))))

dist_only %>%
  mutate(pred_new = predict(ae_flight_mod_nodem,
                            type = "response",
                            newdata = dist_only)) %>%
  ggplot(aes(x=d_miles_oneway,y=pred_new,color=year)) +
  geom_line(alpha=1)

raw_driving <- readRDS("build/cache/mobile_osrm_dist.rds") 

raw_driving %>%
  ggplot(aes(x=trav_dist,y=trav_time)) +
  geom_point(alpha=.01) +
  geom_smooth()

summary(lm(trav_dist ~ trav_time, data = raw_driving))


###########################
#Tables

modelsummary(list(ae_fly_dem,ae_fly_dem_all,ae_fly_nodem,ae_fly_nodem_all),
             stars = T)
