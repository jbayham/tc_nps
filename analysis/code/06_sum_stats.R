#Preliminaries
library(pacman)
p_load(tidyverse,progress,modelsummary,janitor,readxl)

source("project_init.R")
#########################################
park_attributes <- read_excel("build/inputs/park_attributes.xlsx") %>%
  select(code=park,short_names) %>%
  drop_na(short_names)

park_subset <- readRDS("build/cache/park_subset.rds") %>%
  inner_join(park_attributes,by=join_by(code)) %>%
  mutate(display_name = paste0("(",code,") ",short_names)) 


#Mobile datasets
mlist = c("regs_dem","regs_nopred")
m=mlist[1]
for(m in mlist){
  #Regression data
  df_list <- list.files(paste0("analysis/inputs/",m,"/"),full.names = TRUE) %>%
    map(function(x){
      readRDS(x)
    }) %>%
    bind_rows()
  
  df_list %>%
    left_join(select(park_subset,placekey,display_name),by="placekey") %>%
    select(display_name,visits,income,age,householdsize,cost_total_weighted) %>%
    pivot_longer(-display_name) %>%
    datasummary(display_name*value ~ name*(Mean+N),
                data = .,
                output = paste0("analysis/outputs/sum_stats_",m,".xlsx"))
}

#NPS data

for(i in c("bronze","silver","gold")){
  temp_dat <- list.files("build/inputs/AE/Reg_TCM_Poisson",pattern = paste0(i,".rds"),full.names = TRUE) %>%
    map(function(x){
      temp_raw <- readRDS(x) 
      temp_dat <- temp_raw$model %>%
        mutate(parkcode = str_extract(x, paste0("(?<=/)[^/]+(?=\\_",i,".rds)")))
    }) %>%
    bind_rows()
  
  temp_dat %>%
    pivot_longer(-parkcode) %>%
    datasummary(parkcode*value ~ name*(Mean+N),
                data = .,
                output = paste0("analysis/outputs/sum_stats_",i,".xlsx"))
}




mean_na <- function(x) mean(x, na.rm = TRUE)

bronze_dat$model %>%
  select(tc=cost_total_weighted_silverbronze,starts_with("census")) %>%
  datasummary(Mean + N ~ tc + census_age + census_income,data=.)

# bronze_all %>%
#   select(parkcode,tc=cost_total_weighted_silverbronze,starts_with("census")) %>%
#   drop_na() %>%
#   pivot_longer(-parkcode) %>%
#   datasummary(parkcode*value ~ name*(mean_na + N),
#               data=.)







#######################################
mlist = c("regs_dem","regs_nopred")
#m=mlist[1]
plot_cost <- map(mlist,
    function(m){
  #Regression data
  df_list <- list.files(paste0("analysis/inputs/",m,"/"),full.names = TRUE) %>%
    map(function(x){
      readRDS(x)
    }) %>%
    bind_rows() %>%
    select(code_dest,placekey,cost_total_weighted) %>%
    mutate(model = m)
}) %>%
  bind_rows() %>%
  left_join(select(park_subset,placekey,display_name),by="placekey")

plot_cost %>%
  ggplot(aes(y=display_name,x=cost_total_weighted,colour = model)) +
  geom_boxplot(outlier.alpha = .2)
  