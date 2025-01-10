#This script estimates the censored-truncated poisson regression on the mobile device data

#Preliminaries
library(pacman)
p_load(tidyverse,progress,modelsummary,readxl,janitor)

source("project_init.R")

###################################
#Parks reference
park_subset <- read_excel("build/inputs/park_ref.xlsx") %>%
  mutate(location_name = make_clean_names(location_name))

tf_names <- list.files("analysis/inputs",full.names = TRUE) %>%
  str_subset("[:digit:]{5}") 

#Read in the data
f_names <- c(tf_names)

m_names <- c(str_extract(tf_names,"[:digit:]{5}-[:digit:]{5,6}"))


df_list <- map(f_names,function(x){
  read_csv(x)
}) 


park_list <- park_subset %>%
  select(park,placekey,location_name)

#Create dir to hold obs counts for each model
if(!dir.exists("analysis/cache/nobs")) dir.create("analysis/cache/nobs")


###########
#Start
df_temp=df_list[[1]]
nm=m_names[1]
walk2(df_list,
      m_names,
      function(df_temp,nm){
        
        for(i in 1:nrow(park_list)){
          
          coef_fname = paste0("analysis/cache/",nm,"_",park_list$location_name[i],".rds")
          if(any(coef_fname %in% list.files("analysis/cache",pattern = ".rds",full.names = TRUE))) next
          
          pk = park_list$placekey[i]
          
          df = df_temp %>%
            filter(placekey == pk)
          
          nobs = nrow(df)
         
        
          
          if(nobs<100){
            message("Too few obs to run model.")
            next
          } 
          
          saveRDS(nobs,paste0("analysis/cache/nobs/",nm,"_",park_list$location_name[i],".rds"))
          
          #Define covariate vector
          c_names <- c("travel_total_cost","white_perc","bach_degree_perc","med_age","residing")
          
          xmat = as.matrix(cbind(1, df[,c_names]))
          yvec = df$visits 
          start_beta <- c(0, rep(0,length(c_names))) # Starting values for the optimization
          
          
          # Fit the full model
          model_fit <- optim(par = start_beta, 
                             fn = poi_cen_trunc_ll, 
                             y = yvec, 
                             X = xmat, 
                             lower_trunc = 1,
                             lower_censor = 4,
                             method = "Nelder-Mead",
                             control = list(maxit = 3000),
                             hessian = F)
          
          if(model_fit$convergence %in% c(0,10)){
            coefs = model_fit$par
            names(coefs) <- c("constant",c_names)
            saveRDS(coefs,paste0("analysis/cache/",nm,"_",park_list$location_name[i],".rds"))
          } 
          
          # Run bootstrap for standard errors
          bootstrap_estimation_par(X = xmat,
                                   y = yvec, 
                                   max_iter = 2000,
                                   start_boot = 1,
                                   n_boot = 500,
                                   output_dir = paste0("analysis/cache/bs_runs/",nm,"_",park_list$location_name[i]))
        }
        
      })


###################


