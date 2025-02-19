#Script to read in and process results

#Preliminaries
library(pacman)
p_load(tidyverse,progress,modelsummary,janitor,readxl)

source("project_init.R")

##############################
sem_prelim <- read_csv("build/inputs/sem_prelim.csv") %>%
  mutate(across(starts_with("cs"),~as.numeric(str_remove(.,"\\$"))))

park_subset <- readRDS("build/cache/park_subset.rds") %>%
  filter(primary==1)

cs_results <- readRDS("analysis/cache/cs_results.rds") %>%
  inner_join(select(park_subset,code,code_dest,location_name),by="code_dest") %>%
  inner_join(sem_prelim,.,by="code")

cs_results %>%
  ggplot(aes(y=reorder(location_name,cs_day),x=cs_day,xmin=cs_day_l,xmax=cs_day_u)) +
  geom_pointrange()

cs_results %>%
  ggplot(aes(x=cs_day,y=cs_per_day,label=code,color=code)) +
  geom_text(nudge_x = 20,size = 3,show.legend = F) +
  geom_point() +
  geom_abline(intercept = 0,slope=1,alpha=.5) +
  coord_equal() +
  xlim(0,250) +
  ylim(0,250) +
  labs(x="Mobile",y="SEM",title = "CS/Day Estimates ($2023)") +
  theme_classic()

ggsave("analysis/outputs/cs_2d.png",width = 6,height = 4,units = "in")
ggsave("analysis/outputs/cs_2d.pdf",width = 6,height = 4,units = "in")

omitted_est <- cs_results %>%
  filter(if_any(ends_with("day"),~.>250))

cs_results %>%
  select(code,location_name,cs_per_day,starts_with("cs_day")) %>%
  View()

cs_results %>% 
  mutate(across(where(is.numeric),~round(.,2))) %>%
  write_csv("analysis/cache/cs_results.csv")
