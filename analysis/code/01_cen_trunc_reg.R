#This script estimates the censored-truncated poisson regression on the mobile device data

#Preliminaries
library(pacman)
p_load(tidyverse,progress,modelsummary,readxl,janitor)

source("project_init.R")

###################################
#Parks reference
park_subset <- readRDS("build/cache/park_subset.rds") %>%
  filter(primary==1)

f_names <- list.files("analysis/inputs/compare_reg/nopred",full.names = TRUE) 
m_names <- park_subset$code_dest

df_list <- map(f_names,function(x){
  readRDS(x)
}) 


#Create dir to hold obs counts for each model
coef_dir_name="analysis/cache/compare_regs/nopred"
dir_ifnot(coef_dir_name)
bs_dir_name="analysis/cache/bs_runs/nopred"
dir_ifnot(bs_dir_name)

###########
#Start
df=df_list[[1]]
nm=m_names[1]
walk2(df_list[c(1:31)],
      m_names[c(1:31)],
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
        #c_names <- c("cost_total_weighted","age","householdsize","residing")
        
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
          saveRDS(coefs,coef_fname)
        } 
        
        # Run bootstrap for standard errors
        bootstrap_estimation_par(X = xmat,
                                 y = yvec,
                                 max_iter = 2000,
                                 start_boot = 1,
                                 n_boot = 500,
                                 output_dir = bs_run_dir)
        
        #Read in all runs, row_bind and cache
        bs_runs <- list.files(bs_run_dir,full.names = T) %>%
          map(readRDS) %>%
          bind_rows()
        
        if(nrow(bs_runs)>=500) dir_delete(bs_run_dir)
        
        saveRDS(bs_runs,paste0(bs_dir_name,"/",nm,".rds"))
        
      })


###################

check <- list.files("analysis/cache/bs_runs/EVER",full.names = T) %>%
  map(readRDS) %>%
  bind_rows() 

hist(check[,2])

check_mean = summarize(check,across(everything(),mean))

check
1/check[1,2]
