#This script estimates the censored-truncated poisson regression on the mobile device data

#Preliminaries
library(pacman)
p_load(tidyverse,progress,modelsummary,readxl,janitor)

source("project_init.R")

###################################
#Parks reference
park_subset <- readRDS("build/cache/park_subset.rds") %>%
  filter(primary==1)
#m_names <- park_subset$code_dest

yr_list = c(2019:2023)
yr=yr_list[1]

for(yr in yr_list){

#f_names <- list.files("analysis/inputs/compare_reg/nopred",full.names = TRUE) 
f_names <- list.files(paste0("analysis/inputs/date_",yr,"-08-01"),full.names = TRUE) 

# Read in all park data
df_list <- map(f_names,function(x){
  readRDS(x)
}) 

#Get model names
m_names <- df_list %>%
  map_chr(function(x){
    if(nrow(x)==0) return(NA)
    unique(x$code_dest)
  }) %>%
  na.omit()

#Create dir to hold obs counts for each model
#coef_dir_name=paste0("analysis/cache/regs_nopred")
#bs_dir_name=paste0("analysis/cache/bs_runs/regs_nopred")
coef_dir_name=paste0("analysis/cache/date_",yr,"-08-01")
bs_dir_name=paste0("analysis/cache/bs_runs/date_",yr,"-08-01")

dir_ifnot(coef_dir_name)
dir_ifnot(bs_dir_name)

###########
#Start
df=df_list[[31]]
nm=m_names[31]
walk2(df_list,
      m_names,
      function(df,nm){
        
        coef_fname = paste0(coef_dir_name,"/",nm,".rds")
        bs_run_dir = paste0(bs_dir_name,"/",nm)
        
        #if(any(coef_fname %in% list.files("analysis/cache",pattern = ".rds",full.names = TRUE))) next
        
        #pk = park_list$placekey[i]
        
        nobs = nrow(df)

        if(nobs<100){
          message("Too few obs to run model.")
          return(NULL)
        } 
        
        #saveRDS(nobs,paste0("analysis/cache/nobs/",nm,".rds"))
        
        #Define covariate vector
        c_names <- c("cost_total_weighted","income","age","householdsize","residing")

        xmat = as.matrix(cbind(1, df[,c_names[c_names %in% names(df)]]))
        yvec = df$visits 
        start_beta <- c(0, rep(0,length(c_names)),1) # Starting values for the optimization
        
        
        # Fit the full model
        model_fit <- optim(par = start_beta, 
                           fn = nb_cen_trunc_ll, 
                           y = yvec, 
                           X = xmat, 
                           lower_trunc = 1,
                           lower_censor = 4,
                           method = "Nelder-Mead",
                           control = list(maxit = 3000),
                           hessian = F)
        
        
        if(model_fit$convergence %in% c(0,10)){
          coefs = model_fit$par
          if(length(coefs)==(length(c_names)+1)){
            names(coefs) <- c("constant",c_names)
          } else if(length(coefs)==(length(c_names)+2)){
            names(coefs) <- c("constant",c_names,"ltheta")
          }          
          saveRDS(coefs,coef_fname)
        } 
        
        
        # Run bootstrap for standard errors
        bootstrap_estimation(X = xmat,
                             y = yvec, 
                             max_iter = 2000,
                             start_boot = 1,
                             n_boot = 500,
                             output_dir = bs_run_dir,
                             nm = nm,
                             c_names = c_names,
                             parallel = T,
                             workers = 4
                             )
        
        #Read in all runs, row_bind and cache
        bs_runs <- list.files(bs_run_dir,full.names = T) %>%
          map(readRDS) %>%
          bind_rows()
        
        if(nrow(bs_runs)>=500) dir_delete(bs_run_dir)
        
        saveRDS(bs_runs,paste0(bs_dir_name,"/",nm,".rds"))
        
      })

}
###################


hist(bs_runs[,2])

check_mean = summarize(bs_runs,across(everything(),mean))

check_mean
1/check_mean[1,2]
