#Preliminaries
library(pacman)
p_load(tidyverse,progress,modelsummary,janitor,readxl)

source("project_init.R")
########################################
#Parks reference
park_subset <- readRDS("build/cache/park_subset.rds") %>%
  filter(primary==1)

df_list <- list.files("analysis/inputs/compare_reg",full.names = TRUE)  %>%
  map(function(x){
  readRDS(x)
}) 

##########################
#Distribution of travel distances and times by park
library(ggridges)

df_list %>%
  bind_rows() %>%
  ggplot(aes(y=reorder(code_dest,osrm_ow_miles),x=osrm_ow_miles,color=code_dest)) +
  geom_boxplot(show.legend = F,outliers = T,outlier.alpha = .1)

df_list %>%
  bind_rows() %>%
  # ggplot(aes(y=reorder(code_dest,osrm_ow_miles),x=osrm_ow_miles,fill=code_dest)) +
  ggplot(aes(y=code_dest,x=osrm_ow_miles,fill=code_dest)) +
  #geom_boxplot(show.legend = F)
  geom_density_ridges(color=NA,show.legend = F) +
  xlim(NA,2000)

df_list %>%
  bind_rows() %>%
  ggplot(aes(y=reorder(code_dest,cost_total_weighted),x=cost_total_weighted,fill=code_dest)) +
  #geom_boxplot(show.legend = F)
  geom_density_ridges(color=NA,show.legend = F)


