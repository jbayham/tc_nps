#Preliminaries
library(pacman)
p_load(tidyverse,progress,janitor,readxl)

source("project_init.R")

##############################


mon_map <- tibble(mon_label=month(1:12,abbr = T,label = T) %>% as.character() %>%str_to_lower(),
                  mon_num = 1:12)

all_park_dat <- list.files("build/inputs/nps_visits",full.names = T,pattern = ".csv") %>%
  map(function(x){
    park_raw <- read_csv("build/inputs/nps_visits/Arches_NPS_visit_statistics.csv",skip = 3) %>%
      rename_with(str_to_lower) %>%
      select(year,all_of(mon_map$mon_label)) %>%
      pivot_longer(-year,names_to = "mon_label",values_to = "n") %>%
      inner_join(mon_map,by = join_by(mon_label)) %>%
      mutate(park=str_remove(basename(x),"_visit_statistics.csv"))
  }) %>%
  bind_rows()
#all_park_dat[1]

# unique(all_park_dat[,"park"]) %>%
#   write_csv("build/cache/park_names.csv")
park_info <- read_csv("build/cache/park_names.csv") %>%
  filter(is.na(rec)) %>%
  select(park,park_code)

all_park_dat %>%
  inner_join(park_info,by = join_by(park)) %>%
  mutate(measure_date = as_date(paste0(year,"-",
                                       str_pad(mon_num,2,"left",0),
                                       "-01"))) %>%
  select(park_code,measure_date,nps_visits=n) %>%
  saveRDS("build/cache/nps_visits.rds")

park_sup <- read_excel("build/inputs/nps_visit_sup.xlsx") %>%
  mutate(measure_date = as_date(paste0(year,"-",
                                       month,
                                       "-01"))) %>%
  select(-c(year,month)) %>%
  pivot_longer(-c(measure_date),names_to = "park_code",values_to = "nps_visits") %>%
  saveRDS("build/cache/nps_visits_sup.rds")



############################################
tract_devices <- read_csv("build/inputs/tract_devices.csv") %>%
  mutate(measure_date = as_date(paste0(year,"-",mon,"-01")))

tract_devices %>%
  group_by(measure_date) %>%
  mutate(residing=sum(residing)) %>%
  ggplot(aes(x=measure_date,y=residing)) +
  geom_line()

park_subset <- readRDS("build/cache/park_subset.rds") %>%
  filter(primary==1) %>%
  filter(!(code_dest %in% c("FOLA_02","GRTE_03","GUMO_02","GEGR","MORR_01","MORR_02","DEWA","DINO","GARI","LAVO","MORU","TUAI")))

nps_dat <- readRDS("build/cache/nps_visits.rds") %>%
  bind_rows(readRDS("build/cache/nps_visits_sup.rds"))
  

mobile_dat <- readRDS("build/inputs/visit_dat.rds") %>%
  select(placekey,measure_date=date_range_start,starts_with("raw"))

plot_dat <- park_subset %>%
  select(code,placekey,sample_period) %>%
  inner_join(mobile_dat,by=join_by(placekey)) %>%
  arrange(code,measure_date) %>%
  inner_join(nps_dat,by=c("code"="park_code","measure_date"))



#Time series plots
plot_rescaled <- plot_dat %>%
  group_by(code) %>%
  mutate(across(c(raw_visit_counts,nps_visits),scale)) %>%
  ungroup() %>%
  select(code,measure_date,Mobile=raw_visit_counts,NPS=nps_visits) %>%
  pivot_longer(c(Mobile,NPS)) 

cor_coefs <- plot_dat %>%
  group_by(code) %>%
  mutate(across(c(raw_visit_counts,nps_visits),scale)) %>%
  drop_na() %>%
  summarize(corr = cor(raw_visit_counts,nps_visits))

plot_rescaled %>%
  inner_join(cor_coefs) %>%
  mutate(code=paste0(code,"  (",round(corr,2),")")) %>%
  ggplot(aes(x=measure_date,y=value,color=name)) +
  geom_line() +
  scale_color_discrete(name=NULL) +
  facet_wrap(~code) +
  theme_minimal() +
  labs(x=NULL,y="Scaled Visits") +
  theme(legend.position = "inside",
        legend.position.inside = c(.9,.1))

ggsave("analysis/outputs/visit_corr.pdf",width = 10,height = 8,units = "in")
  

plot_dat %>%
  filter(code=="ROMO") %>%
  #select(code,measure_date,mobile=raw_visit_counts,nps=nps_visits) %>%
  #pivot_longer(c(mobile,nps)) %>%
  ggplot(aes(x=scale(nps_visits),y=scale(raw_visit_counts))) +
  geom_point() +
  geom_smooth()

#May need to normalize by device counts