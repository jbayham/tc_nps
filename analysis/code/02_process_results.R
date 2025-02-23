#Script to read in and process results

#Preliminaries
library(pacman)
p_load(tidyverse,progress,modelsummary,janitor,readxl)

source("project_init.R")
########################################
all_models = c(paste0("regs_",c("dem","nopred")),
               paste0("date_",c(2019:2023),"-08-01"))
m_choice = "date_2019-08-01"

for(m_choice in all_models){

  #Material for tables
  cache_list <- list.files(paste0("analysis/cache/",m_choice,"/")) %>%
    str_remove(".rds")
  
  
  park_subset <- readRDS("build/cache/park_subset.rds") %>%
    filter(primary==1,
           code_dest %in% cache_list)
  
  #Cached  visit ratios
  visit_ratio <- readRDS("build/cache/visit_ratio.rds") %>%
    inner_join(park_subset,.,by="placekey") %>%
    select(code_dest,vr)
  
  
  #Regression results
  df_list <- list.files(paste0("analysis/inputs/",m_choice),full.names = TRUE) %>%
    map(function(x){
      readRDS(x)
    }) 
  
  #Calculating the number of observations for each unit
  nobs_df <- map(df_list,~tibble(code_dest=.$code_dest[1],obs=nrow(.))) %>%
    bind_rows()
  #######################################
  
  
  #subset only models that have matches in the NPS data and we have regression results
  mlist = park_subset$code_dest
  
  
  #mlist = c("20191-20196_arches_national_park") #model choice - corresponds to the full reg name and folder of bs runs
  mc=mlist[1]
  mod_results <- map(mlist,function(mc){
    #Call up all results
    results <- readRDS(paste0("analysis/cache/",m_choice,"/",mc,".rds"))
    results_bs <- readRDS(paste0("analysis/cache/bs_runs/",m_choice,"/",mc,".rds"))
    
  
    nobs <- nobs_df %>%
      filter(code_dest == mc) %>%
      pull(obs)
    
    vr <- visit_ratio %>%
      filter(code_dest == mc) %>%
      pull(vr)
    
    
    ################
    #View histogram
    #hist(results$travel_total_cost)
    
    #Generate table of class modelsummary - you can extract the table of coefs, se, pvals, cis from the list if needed
    mod <- bs_table(reg_coef = results,
                    bs_dat = results_bs,
                    nobs = nobs,
                    tc_coef_name = "cost_total_weighted",
                    label_names = c("Travel Cost"="cost_total_weighted","Income"="income","Age"="age","Household Size"="householdsize","Devices"="residing","Constant"="constant"),
                    visit_ratio = vr) 
    
    
  })
  
  #Seaparating cs estimates and caching
  cs_results <- map(mod_results,2) %>%
    bind_rows() %>%
    mutate(code_dest=mlist)
  
  saveRDS(cs_results,paste0("analysis/cache/cs_results_",m_choice,".rds"))
  write_csv(cs_results,paste0("analysis/outputs/cs_results_",m_choice,".csv"))
  
  #
  mod_results <- map(mod_results,1)
  
  names(mod_results) = mlist
  
  saveRDS(mod_results,paste0("analysis/cache/mob_mod_results_",m_choice,".rds"))
  
  
  modelsummary(mod_results,
               stars = TRUE,
               output = paste0("analysis/outputs/mobile_reg_tab_",m_choice,".xlsx"))

}





