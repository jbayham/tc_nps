### Compare the observed devices to census populations for all tracts

#Preliminaries
library(pacman)
p_load(tidyverse,progress,modelsummary,janitor,readxl,fixest)

source("project_init.R")
###########################################################
#Subset to origin tracts by joining with analysis data (already subset by time and travel distance)

#Read in devices residing in each tract
tract_devices <- read_csv("build/inputs/tract_devices.csv") %>%
  mutate(measure_date = as_date(paste0(year,"-",mon,"-01")))

#Read in census income and xwalk. Rowbinding and letting xwalk data have precedent
census_data <- readRDS("build/cache/census_data_2023.rds") %>%
  rename(tract=geoid)

xwalk_data <- readRDS("build/cache/xwalk_data_2023.rds")

census_xwalk <- bind_rows(xwalk_data,census_data) %>%
  distinct(tract,.keep_all = TRUE) %>%
  select(tract,total_pop,age=med_age,income=med_inc,householdsize=hh_size) 


#Analysis dataset
reg_dat <- list.files("analysis/inputs/regs_dem",full.names = TRUE) %>%
  map(function(x){
    readRDS(x)
  })  

rep_dat <- reg_dat %>%
  bind_rows() %>%
  distinct(tract,residing,income,age,householdsize) %>%
  inner_join(select(census_xwalk,tract,total_pop),by="tract") %>%
  mutate(across(c(income),~./1000),
         total_pop = total_pop/100,
         st_fips = factor(str_sub(tract,1,2)))

#Subsetting entire tract metrics for each site
rep_dat_all <- reg_dat %>%
  map(function(df){
    tract_sub <- tract_devices %>%
      filter(measure_date %within% df$drange[1]) %>%
      group_by(tract) %>%
      summarize(residing=mean(residing,na.rm = T)) %>%
      ungroup() %>%
      mutate(code_dest = df$code_dest[1],
             year = year(int_end(df$drange[1])))
    
   return(tract_sub)
  }) %>%
  bind_rows() %>%
  distinct(tract,year,residing) %>%
  inner_join(census_xwalk,by="tract") %>%
  mutate(across(c(income),~./1000),
         total_pop = total_pop/100,
         st_fips = factor(str_sub(tract,1,2))) 

#2022 and 2023 summer rep
rep_all <- tract_devices %>%
  filter(year %in% c(2022,2023),
         mon %in% c(6,7)) %>%
  group_by(tract) %>%
  summarize(residing=mean(residing,na.rm = T)) %>%
  ungroup() %>%
  inner_join(census_xwalk,by="tract") %>%
  mutate(across(c(income),~./1000),
         total_pop = total_pop/100,
         st_fips = factor(str_sub(tract,1,2))) 



#######################################


datasummary(residing + total_pop + age + income + householdsize ~ Mean + SD + Min + Max,
            data = reg_dat,
            #add_rows = data.frame("Obs",nrow(tract_devices_sub)),
            fmt=0,
            #output = 'table1.pptx'
            output = "default"
            )

#Is there any correlation with demographics
# ols_sample <- feols(residing ~ total_pop + age + income + householdsize,
#                    data = rep_dat)

fe_sample <- feols(residing ~ total_pop + age + income + householdsize,
                  data = rep_dat)

fe_all <- feols(residing ~ total_pop + age + income + householdsize,
                  data = rep_all)

etable(fe_sample,fe_all)
tab_out
write_csv(tab_out,"analysis/outputs/rep_reg.csv")
modelsummary(list(fe_sample,fe_all),stars = TRUE,
             output = "analysis/outputs/rep_reg.xlsx")







############################################
#modelsummary parameters

#Plot of mobile device representativeness  
overall_mean = rep_dat %>%
  mutate(frac = residing/(total_pop*100)) %>%
  filter(between(frac,0,.2)) %>%
  summarize(frac_sum=mean(frac,na.rm = TRUE)) %>%
  pull()

rep_dat %>%
  mutate(frac = residing/(total_pop*100),
         state_code = str_sub(tract,1,2)) %>% 
  left_join(tigris::fips_codes %>%  distinct(state_code,state_name),by="state_code") %>% 
  #left_join(rep_dat %>% count(state_code=str_sub(tract,1,2))) %>%
  ggplot(aes(x=frac,y=reorder(state_name,frac))) +
  geom_boxplot(size=.2,outlier.alpha = .2) +
  #geom_violin() +
  #geom_label(aes(label = n,x=-.01),label.size=NA,size=3,family = "serif") +
  geom_vline(xintercept = overall_mean,linetype="dashed") +
  xlim(0,.2) +
  theme_bw(base_size = 10) +
  theme(text = element_text(family = "serif")) +
  labs(y=NULL,x="Devices/Population")

ggsave("analysis/outputs/mobile_rep_boxplot.pdf",width = 4.2,height = 5.5,units = "in")



#########################
#Calculate survey respondents income percentile based on zipcode distribution
zip_census <- read_csv("build/cache/census_raw_zcta.csv") %>% clean_names()

hhinc_totals <- zip_census %>%
  filter(variable=="B19001_001") %>%
  select(geoid,total=estimate)

hhinc_med <- zip_census %>%
  filter(variable=="B19013_001")

hhinc_labels <- read_csv("build/cache/hhinc_labels_mod.csv") 

hhinc_labels <- hhinc_labels %>%
  mutate(label=factor(label,levels=hhinc_labels$label))


ae_survey <- read_csv("build/inputs/survey_data/survey_final.csv") %>%
  select(zipcode,age,med_income,trav_dist) %>%
  fuzzy_left_join(hhinc_labels,by=c("med_income"="lower","med_income"="upper"),match_fun = list(`>=`,`<=`)) %>%
  select(zipcode:name)



hhinc_dist <- zip_census %>%
  select(geoid,variable,estimate) %>%
  inner_join(hhinc_labels,by=c("variable"="name")) %>%
  filter(variable!="B19001_001") %>%
  inner_join(hhinc_totals,by="geoid") %>%
  mutate(frac=estimate/total) %>%
  group_by(geoid) %>%
  mutate(cdf=cumsum(frac)*100) %>%
  ungroup() %>%
  inner_join(ae_survey,.,by=c("zipcode"="geoid","name"="variable"))



#Histogram of income percentile of visitors
hhinc_dist %>%
  ggplot(aes(x=cdf)) +
  geom_histogram(binwidth = 10,boundary = 0, closed = "left") +
  geom_vline(xintercept = mean(hhinc_dist$cdf,na.rm = T),linetype="dashed") +
  scale_x_continuous(name="Income Percentile",breaks = seq(0,100,20)) +
  theme_bw(base_size = 12,base_family = "serif") +
  labs(y="Frequency")

ggsave("analysis/outputs/inc_percentile.pdf",width = 5,height = 4.5,units = "in")

##########################
##########################
#Calculate mean of percentiles where survey reported income falls - used to adjust mobile income data
summary(hhinc_dist$cdf)















# for html
  # theme_vader() |> 
  # hline(i = 14, part = "body",
  #       border = fp_border(color = "red", width = 1) ) |>
  # autofit() |>
  # save_as_html(path="analysis/presentation/includes/device_reg_table.html")

# datasummary(total_pop + med_age + med_hh_inc + bach_degree_perc + white_perc ~ Mean,
#             data = zip_census,
#             add_rows = data.frame("Obs",1998),
#             fmt=0,
#             output = 'table2.pptx')

#plot of device residing over census population over time for 9 select tracts
tract_devices_sub %>%
  filter(tract %in% sample(parks_home_tract$tract,9)) %>%
  mutate(device_per_cap = residing/total_pop) %>% 
  ggplot(aes(x=measure_date,y=device_per_cap,group=tract)) +
  geom_line() +
  facet_wrap(~tract)

#plot the density across all observations
tract_devices_sub %>%
  mutate(device_per_cap = residing/total_pop) %>%
  ggplot(aes(x=device_per_cap)) +
  geom_histogram(binwidth = .01) +
  xlim(0,.2) +
  scale_y_continuous(labels=scales::unit_format(unit = "K",scale = 1e-3)) +
  labs(x="Devices/Population",y="Frequency") +
  theme_bw(base_size = 15)

ggsave("analysis/presentation/includes/device_per_cap.png")  





#need to attach residing to analysis ds

#####################
#Compare survey reported data to census median - used in first NCU talk
zip_census <- read_csv("build/cache/census_data_zcta.csv")
ae_survey <- read_csv("build/inputs/survey_data/survey_final.csv") %>%
  select(zipcode,age,med_income,race,educ) %>%
  mutate(white_survey = ifelse(race=="White",1,0),
         bach_survey = ifelse(educ=="College (College graduate)",1,0)) %>%
  group_by(zipcode) %>%
  summarize(across(c(age,med_income,white_survey,bach_survey),~mean(.,na.rm=TRUE))) %>%
  ungroup() %>%
  left_join(zip_census,by=c("zipcode"="zcta")) %>%
  filter(zipcode!=55126)

ae_survey %>%
  ggplot(aes(x=(med_income-med_hh_inc)/1000)) +
  geom_histogram(binwidth = 20) +
  scale_x_continuous(breaks=seq.int(-150,150,50),
                     labels = paste0(seq.int(-150,150,50)," K")) +
  labs(x="Survey - Census",y="Obs. Count") +
  theme_bw(base_size = 15)

ggsave("analysis/outputs/compare_income.png",width = 5.5,height = 4.5,units = "in")

ae_survey %>%
  ggplot(aes(x=age-med_age)) +
  geom_histogram(binwidth = 5) +
  xlim(-25,25) +
  labs(x="Survey - Census",y="Obs. Count") +
  theme_bw(base_size = 15)

ggsave("analysis/outputs/compare_age.png",width = 5.5,height = 4.5,units = "in")


#########################
#########################


#Is there variation with distance? Not really. It hovers around mid 60s
hhinc_dist %>%
  ggplot(aes(x=trav_dist,y=cdf)) +
  geom_point(alpha=.3) +
  geom_smooth(method = "lm") +
  xlim(0,750) +
  #scale_x_continuous(name="Percentile",breaks = seq(0,1,.2)) +
  theme_bw(base_size = 15)
