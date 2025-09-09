#Preliminaries
library(pacman)
p_load(tidyverse,progress,modelsummary,janitor,readxl)

source("project_init.R")
########################################
#Parks reference
park_subset <- readRDS("build/cache/park_subset.rds") %>%
  filter(primary==1)

park_attributes <- read_excel("build/inputs/park_attributes.xlsx")


mobile_dat <- list.files("analysis/inputs/regs_dem/",full.names = TRUE)  %>%
  map(function(x){
  readRDS(x)
  }) %>%
  bind_rows() %>%
  select(code_dest,loc_code=tract,Mobile_dist=osrm_ow_miles,Mobile_time=osrm_ow_hrs) %>%
  mutate(parkcode = str_sub(code_dest,1,4)) %>%
  pivot_longer(starts_with("mobile"))

nps_dist <- read_csv("build/cache/survey_osrm_dist.csv.gz") %>%
  select(parkcode,loc_code=zcta,NPS_dist = trav_dist,NPS_time = trav_time)

nps_dat <- readRDS("build/inputs/AE/SEM_dems_fees_pops_bronze.rds") %>%
  select(parkcode,loc_code=zcta) %>%
  inner_join(nps_dist) %>%
  pivot_longer(starts_with("NPS"))

plot_dat <- bind_rows(mobile_dat,nps_dat)

# nps_dat <- list.files("build/inputs/AE/Reg_TCM_Poisson",pattern = "bronze.rds",full.names = TRUE)[1] %>%
#   map(function(x){
#     temp <- readRDS(x)
#     temp$model
#   }) %>%
#   bind_rows()
##########################
#Distribution of travel distances and times by park
################## Use CDF rather than PDF like Sessions et al. #########################

pc="GRTE"
x=plot_dat %>%
  filter(parkcode==pc,
         name == "mobile_dist") %>%
  pull(value)
y=plot_dat %>%
  filter(parkcode==pc,
         name == "survey_dist") %>%
  pull(value)

ksres=ks.test(x,y)
adres=kSamples::ad.test(x,y)

append_test <- function(df,pc){
  x=plot_dat$value[plot_dat$parkcode==pc & plot_dat$name=="Mobile_dist"]
  y=plot_dat$value[plot_dat$parkcode==pc & plot_dat$name=="NPS_dist"]
  
  if(length(x)<3 | length(y)<3) return(NA)
  ksres=ks.test(x,y)
  adres=kSamples::ad.test(x,y)
  
  out <- data.frame(parkcode=pc,
                    ks=ksres$p.value,
                    ad=adres$ad["version 1:"," asympt. P-value"])
  
  return(out)
  # if(test=="ks"){
  #   ksres=ks.test(x,y)
  #   return(ksres$p.value)
  # } 
  # if(test=="ad"){
  #   adres=kSamples::ad.test(x,y)
  #   return(adres$ad["version 1:"," asympt. P-value"])
  # }
}

dist_tests <- map(unique(plot_dat$parkcode),~append_test(df=plot_dat,pc=.)) 
discard(dist_tests,~any(is.na(.))) %>% 
  bind_rows() %>%
  write_csv("analysis/outputs/dist_test.csv")


plot_dat %>%
  filter(name %in% c("Mobile_dist","NPS_dist"),
         !(parkcode %in% c("DEWA","LAVO","MORR","GEGR"))) %>%
  mutate(name = str_remove(name,"_dist")) %>%
  ggplot(aes(x=value,color=name)) +
  #geom_histogram(fill=NA,alpha=.4) +
  #geom_density() +
  scale_color_discrete(name=NULL) +
  stat_ecdf(geom = "step") +
  xlim(0,3000) +
  labs(x="Oneway Distance (mi)",y="ECDF") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "inside",
        legend.position.inside = c(.9,.05)) +
  facet_wrap(~parkcode)

ggsave("analysis/outputs/site_dist_cdf.pdf",width = 10,height = 8,units = "in")
#############################
#Using ggridges
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


