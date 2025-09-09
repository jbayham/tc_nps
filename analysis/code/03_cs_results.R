#Script to read in and process results

#Preliminaries
library(pacman)
p_load(tidyverse,progress,modelsummary,janitor,readxl)

source("project_init.R")

##############################
park_attributes <- read_excel("build/inputs/park_attributes.xlsx")

park_subset <- readRDS("build/cache/park_subset.rds") %>%
  filter(primary==1) %>%
  filter(!(code_dest %in% c("FOLA_02","GRTE_03","GUMO_02")))

sem_prelim <- read_csv("build/inputs/results_goldsilverbronze.csv") %>%
  select(park,starts_with(c("gold_CStrip","silver_CStrip","bronze_CStrip"))) 

sem_prelim_day <- read_csv("build/inputs/results_goldsilverbronze.csv") %>%
  select(park,starts_with(c("gold_CStrip","silver_CStrip","bronze_CStrip"))) %>%
  inner_join(park_attributes,by = join_by(park)) %>%
  mutate(across(c(gold_CStrip:bronze_CStrip_ub),~./ndays))


visit_ratio <- readRDS("build/cache/visit_ratio.rds")

cs_res_dem <- readRDS("analysis/cache/cs_results_dem.rds") %>% 
  select(code_dest,cs,cs_l,cs_u) %>%
  rename_with(~paste0("dem_",.x),starts_with("cs")) %>%
  rename_with(~paste0(.x,"b"),ends_with(c("_u","_l")))

cs_res_nopred <- readRDS("analysis/cache/cs_results_regs_nopred.rds") %>% 
  select(code_dest,cs,cs_l,cs_u) %>%
  rename_with(~paste0("nopred_",.x),starts_with("cs")) %>%
  rename_with(~paste0(.x,"b"),ends_with(c("_u","_l")))

cs_results <- inner_join(cs_res_dem,cs_res_nopred,by = join_by(code_dest)) %>%
  inner_join(select(park_subset,code,code_dest,park),by="code_dest") %>%
  inner_join(sem_prelim,.,by=c("park"="code"))


cs_results_day <- inner_join(cs_res_dem,cs_res_nopred,by = join_by(code_dest)) %>%
  inner_join(select(park_subset,code,code_dest,placekey,park_name=park),by="code_dest") %>%
  inner_join(visit_ratio,by="placekey") %>%
  mutate(across(starts_with(c("dem","nopred")),~./vr)) %>%
  inner_join(sem_prelim_day,.,by=c("park"="code")) %>%
  rename(code=park,park=park_name)

write_csv(cs_results,"analysis/outputs/cs_results_all.csv")
write_csv(cs_results_day,"analysis/outputs/cs_results_day_all.csv")
################################
#The remainder of the code is written based on a dataframe called cs_results, so deciding which one here:
cs_results=cs_results_day %>%
  filter(!(code %in% c("GARI","TUAI"))) %>%
  mutate(new_name = paste0(short_names," (",code,")"))


long_plot <- cs_results %>%
  select(new_name,
         `NPS Ind` = gold_CStrip,
         `NPS Ind. Agg.` = silver_CStrip,
         `NPS Zonal` = bronze_CStrip,
         `Mobile Aug.` = dem_cs,
         `Mobile` = nopred_cs) %>%
  pivot_longer(-new_name) %>% 
  mutate(name = factor(name, levels = c("NPS Ind", "NPS Ind. Agg.", "NPS Zonal", "Mobile Aug.", "Mobile"))) %>%
  distinct()

long_plot_lb <- cs_results %>%
  select(new_name,
         `NPS Ind` = gold_CStrip_lb,
         `NPS Ind. Agg.` = silver_CStrip_lb,
         `NPS Zonal` = bronze_CStrip_lb,
         `Mobile Aug.` = dem_cs_lb,
         `Mobile` = nopred_cs_lb) %>%
  pivot_longer(-new_name,values_to = "lb") %>% 
  mutate(name = factor(name, levels = c("NPS Ind", "NPS Ind. Agg.", "NPS Zonal", "Mobile Aug.", "Mobile"))) %>%
  distinct()

long_plot_ub <- cs_results %>%
  select(new_name,
         `NPS Ind` = gold_CStrip_ub,
         `NPS Ind. Agg.` = silver_CStrip_ub,
         `NPS Zonal` = bronze_CStrip_ub,
         `Mobile Aug.` = dem_cs_ub,
         `Mobile` = nopred_cs_ub) %>%
  pivot_longer(-new_name,values_to = "ub") %>% 
  mutate(name = factor(name, levels = c("NPS Ind", "NPS Ind. Agg.", "NPS Zonal", "Mobile Aug.", "Mobile"))) %>%
  distinct()

long_plot_ci <- long_plot %>%
  inner_join(long_plot_lb) %>%
  inner_join(long_plot_ub)

order_var <- long_plot %>%
  group_by(new_name) %>%
  summarize(sort_val = sd(value)) %>%
  ungroup() %>%
  arrange(desc(sort_val))
# order_var <- inner_join(park_subset,park_attributes,by=c("code"="park")) %>%
#   filter(code %in% unique(cs_results$code)) %>%
#   mutate(new_name = paste0(park," (",code,")")) %>%


#dotwhisker style
trunc_level = 300
long_plot_ci %>%
  #inner_join(order_var) %>%
  mutate(elip = ifelse(ub>trunc_level,"*",""),
         ub = ifelse(ub>trunc_level,trunc_level,ub)) %>%
  ggplot(aes(y=name,x=value,xmin=lb,xmax=ub,color=name)) +
  geom_point() +
  geom_errorbar(alpha=.7) +
  geom_text(aes(x=ub+5,label = elip),show.legend = F) +
  #geom_pointrange(size=.1) +
  scale_color_discrete(name="Model",guide = guide_legend(reverse = TRUE)) +
  #scale_shape_manual(values = c(21:25)) +
  xlim(0,trunc_level+30) +
  labs(x="CS/Trip ($2023)",y=NULL) +
  theme_minimal(base_size = 13) +
  facet_grid(rows = vars(new_name), switch = "y", scales = "free_y") +
  theme(
    strip.text.y.left = element_text( angle = 0,hjust=1),  # Keep facet labels horizontal
    axis.text.y = element_blank(),  # Remove y-axis tick labels
    axis.ticks.y = element_blank()  # Remove y-axis ticks
  )

ggsave("analysis/outputs/dw_compare_plot.png",width = 8.5,height = 12,units = "in")
ggsave("analysis/outputs/dw_compare_plot.pdf",width = 8.5,height = 12,units = "in")

#####################################################
#Plot comparing two sets of results
library(ggrepel)

cs_results %>%
  # ggplot(aes(x=gold_CStrip,y=bronze_CStrip,label=code,color=code)) +
  ggplot(aes(x=dem_cs,y=bronze_CStrip,label=code,color=code)) +
  geom_text_repel(size=3,show.legend = F) +
  geom_point(show.legend = F) +
  geom_abline(intercept = 0,slope=1,alpha=.5,linetype="dashed") +
  coord_equal() +
  xlim(0,250) +
  ylim(0,250) +
  labs(x="Mobile Aug ($CS/day)",y="NPS Zonal ($CS/day)") +
  theme_classic(base_size = 13)

ggsave("analysis/outputs/cs_2d.png",width = 5,height = 5,units = "in")
ggsave("analysis/outputs/cs_2d.pdf",width = 5,height = 5,units = "in")


#######################################################
#Creating a table of results
cs_results=cs_results_day %>%
  mutate(new_name = paste0(park," (",code,")"))

mn = c("gold_CStrip","silver_CStrip","bronze_CStrip","dem_cs","nopred_cs")

shell <- matrix(0,nrow = nrow(cs_results)*2,ncol=6) %>% 
  as.data.frame() %>%
  mutate(V1="")
dig=0
cntr=1
for(i in 1:nrow(cs_results)){
  shell[cntr,1] = cs_results[i,"new_name"]
  for(j in 2:6){
    shell[cntr,j] = round(as.numeric(cs_results[i,mn[j-1]]),digits = dig)
    shell[cntr+1,j] = paste0("[",round(cs_results[i,paste0(mn[j-1],"_lb")],digits = dig),", ",round(cs_results[i,paste0(mn[j-1],"_ub")],digits = dig),"]")
  }
  cntr=cntr+2
}


write_csv(shell,"analysis/outputs/restab1.csv")

#####################################################
#CS over time

visit_ratio <- readRDS("build/cache/visit_ratio.rds") %>%
  inner_join(select(park_subset,code,code_dest,placekey,park_name=park),by="placekey") 

yr=2019

main_dat <- list.files(paste0("analysis/cache/date_",yr,"-08-01"),full.names = T) %>%
  map(function(x){
    readRDS(x) %>%
      c(.,"model"=str_extract(x,"(?<=/)[^/]+(?=\\.rds)"))
    }) %>%
  bind_rows()

bs_dat <- map(c(2019:2023),function(yr){
  list.files(paste0("analysis/cache/bs_runs/date_",yr,"-08-01"),full.names = T) %>%
    map(function(x){
      readRDS(x) %>%
        c(.,
          "model"=str_extract(x,"(?<=/)[^/]+(?=\\.rds)"),
          "measure_date"=str_extract(x,"(?<=date_)[0-9]{4}-[0-9]{2}-[0-9]{2}"))
    }) %>%
    bind_rows()
}) %>%
  bind_rows() %>%
  mutate(measure_date=as_date(measure_date))
  

bs_dat %>%
  filter(!(model %in% c("GEGR","MORR_02","DEWA"))) %>%
  inner_join(visit_ratio,by=c("model"="code_dest")) %>%
  mutate(cs_day=(-1/cost_total_weighted)/vr) %>%
  group_by(code,measure_date) %>%
  summarise(cs_day_med = median(cs_day),
            cs_day_ub = quantile(cs_day,.75),
            cs_day_lb = quantile(cs_day,.25)) %>%
  ggplot(aes(x=measure_date,y=cs_day_med,ymin=cs_day_lb,ymax=cs_day_ub)) +
  geom_pointrange(size=.41) +
  geom_line() +
  facet_wrap(~code,scales = "free_y") +
  theme_minimal(base_size = 12) +
  labs(x="Year",y="CS/day ($2023)")

ggsave("analysis/outputs/cs_day_time_series.pdf",width = 10,height = 8,units = "in")

bs_dat %>%
  filter((model %in% c("DINO","EVER","GRSA","ROMO"))) %>%
  inner_join(visit_ratio,by=c("model"="code_dest")) %>%
  mutate(cs_day=(-1/cost_total_weighted)/vr) %>%
  group_by(model,measure_date) %>%
  summarise(cs_day_med = median(cs_day),
            cs_day_ub = quantile(cs_day,.75),
            cs_day_lb = quantile(cs_day,.25)) %>%
  ggplot(aes(x=measure_date,y=cs_day_med,ymin=cs_day_lb,ymax=cs_day_ub,color=model)) +
  geom_pointrange(size=.41,show.legend = F) +
  geom_line(show.legend = F) +
  facet_wrap(~model,ncol = 1,scales = "free_y") +
  theme_minimal(base_size = 12) +
  labs(x="Year",y="CS/day ($2023)")

ggsave("analysis/outputs/cs_day_time_series_subset.pdf",width = 4,height = 5,units = "in")
  
# bs_dat %>%
#   #filter(model %in% c("AZRU","BADL","ROMO","EVER")) %>%
#   inner_join(visit_ratio,by=c("model"="code_dest")) %>%
#   mutate(cs_day=(-1/cost_total_weighted)/vr) %>%
#   ggplot(aes(x=measure_date,y=cs_day,group = measure_date)) +
#     geom_boxplot(outliers = F) +
#     facet_wrap(~model,scales = "free_y")
# 


