#Script to read in and process results

#Preliminaries
library(pacman)
p_load(tidyverse,progress,modelsummary)

source("project_init.R")
########################################
#Material for tables
visit_ratio <- read_csv("build/cache/visit_ratio.csv")



#######################################

mlist = c("20191-20196_arches_national_park") #model choice - corresponds to the full reg name and folder of bs runs
mc=mlist[1]
mod_results <- map(mlist,function(mc){
  #Call up all results
  results <- list.files(paste0("analysis/cache/bs_runs/",mc), full.names = TRUE) %>%
    lapply(function(x){
      readRDS(x) 
    }) %>%
    bind_rows() %>%
    select(travel_total_cost:residing,constant)
  
  ################
  #View histogram
  hist(results$travel_total_cost)
  
  #Read in cached full reg coefficients
  reg_coef = readRDS(paste0("analysis/cache/",mc,".rds")) %>%
    janitor::clean_names()
  
  #Generate table of class modelsummary - you can extract the table of coefs, se, pvals, cis from the list if needed
  mod <- bs_table(reg_coef = reg_coef[names(results)],
                  results,
                  nobs = 657) #manually entering this for now - it is 657 for all mobility regs
  
  
})

saveRDS(mod_results,"analysis/cache/mob_mod_results.rds")



##############################################################
tf_names <- list.files("analysis/inputs",full.names = TRUE) %>%
  str_subset("[:digit:]{5}") %>%
  str_extract("[:digit:]{5}-[:digit:]{5,6}")

#Call up all results
results <- map(tf_names,
               function(x){
                 list.files(paste0("analysis/cache/bs_runs/",x), full.names = TRUE) %>%
                   map(readRDS) %>%
                   bind_rows() %>%
                   select(travel_total_cost:residing,constant)
               }) 

reg_coefs <- list.files("analysis/cache",full.names = TRUE) %>%
  str_subset("[:digit:]{5}") %>% 
  map(function(x){
    readRDS(x) 
  })

reg_coefs <- map(results,~apply(.,2,mean))
out <- map2(reg_coefs,results,bs_table)

modelsummary(out,stars = TRUE,gof_map = gm)

#########################

# Define the start and end dates
start_date <- as.Date("2019-01-01")
end_date <- as.Date("2023-12-31")

# Generate a sequence of dates in 6-month intervals
date_seq <- seq(from = start_date, to = end_date, by = "6 months")

# Create labels in the form "Jan-Jun \nYYYY"
date_labels <- sapply(date_seq, function(date) {
  if (format(date, "%m") == "01") {
    return(paste0("Jan-Jun \n", format(date, "%Y")))
  } else {
    return(paste0("Jul-Dec \n", format(date, "%Y")))
  }
})

results <- map(tf_names,
               function(x){
                 list.files(paste0("analysis/cache/bs_runs/",x), full.names = TRUE) %>%
                   map(readRDS) %>%
                   bind_rows() %>%
                   transmute(cs = -1/travel_total_cost/1.45) %>%
                   mutate(date_range = x)
               }) %>%
  bind_rows()

results %>%
  filter(date_range != "20237-202312") %>%
  ggplot(aes(x=date_range,y=cs)) +
  geom_boxplot(outliers = FALSE,width=.1) +
  # stat_summary(
  #   aes(
  #     y = stage(cs, after_stat = 8),
  #     label = after_stat(paste(mean))
  #   ),
  #   geom = "text",
  #   fun.data = ~ round(data.frame(mean = mean(.x), sd = sd(.x)), 2)
  # )  +
  #geom_hline(yintercept = 20.34,linetype = "dashed") +
  scale_x_discrete(labels = date_labels) +
  ylim(0,NA) +
  labs(x="Sample Date Range",y="Consumer Surplus per day ($)") +
  theme_bw(base_size = 12,base_family = "serif")

ggsave("analysis/outputs/cs_over_time.pdf",width = 6,height = 5,units = "in")

