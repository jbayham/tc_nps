#This script estimates the censored-truncated poisson regression on the mobile device data

#Preliminaries
library(pacman)
p_load(tidyverse,progress,modelsummary,readxl,janitor,here,furrr,numDeriv,glue)

source("project_init.R")
here()
###################################


#yr_list = c(2019:2023)
#yr=yr_list[1]

visit_ratio = readRDS("build/cache/visit_ratio.rds")

d_names <- list.files("analysis/inputs",full.names = TRUE)[8] 
d=d_names[1]

for(d in d_names){

f_names <- list.files(d,full.names = TRUE) 

# Read in all park data
df_list <- map(f_names,function(x){
  readRDS(x)
}) %>% 
  keep(function(x) nrow(x)>0)

#Get model names
m_names <- df_list %>%
  map_chr(function(x){
    unique(x$code_dest)
  }) 

#Create dir to hold obs counts for each model
d_end = fs::path_split(d)[[1]][3]
coef_dir_name=paste0("analysis/cache/",d_end)
bs_dir_name=paste0("analysis/cache/bs_runs/",d_end)
#coef_dir_name=paste0("analysis/cache/date_",yr,"-08-01")
#bs_dir_name=paste0("analysis/cache/bs_runs/date_",yr,"-08-01")

dir_ifnot(coef_dir_name)
dir_ifnot(bs_dir_name)

###########
#Start
df=df_list[[3]]
nm=m_names[3]


all_results <- map2(
  df_list,
  m_names,
  function(df,nm){
    
    df <- df %>%
      mutate(income = income/1000,
             residing = residing/100)
    
    vr = visit_ratio %>%
      filter(placekey==df$placekey[1]) %>%
      pull(vr)
    
    nobs = nrow(df)

    if(nobs<100){
      message("Too few obs to run model.")
      return(NULL)
    } 
    

    #Define covariate vector
    c_names <- c("cost_total_weighted","income","age","householdsize","residing")

    xmat = as.matrix(cbind(1, df[,c_names[c_names %in% names(df)]]))
    yvec = df$visits 
    
    start_beta <- glm(yvec~xmat[,-1],family=poisson(link="log"))$coefficients
    start_beta <- c(start_beta,1) # Starting values for the optimization
    names(start_beta) <- c("constant",c_names,"ltheta")

    #check if starting values are valid
    check_start = nb_cen_trunc_ll(start_beta, y = yvec, X = xmat, lower_trunc = 1, lower_censor = 4)
    if(is.nan(check_start) | is.infinite(check_start)){ 
      message("Starting values not valid. Resetting to 0s and 1.1 for ltheta.")
      start_beta = c(rep(0,length(start_beta)-1),1.1)
      names(start_beta) <- c("constant",c_names,"ltheta")
    }
    
    # Fit the full model
    model_fit_gr <- optim(par = start_beta, 
                         fn = nb_cen_trunc_ll, 
                         gr = nb_cen_trunc_grad_vec,
                         y = yvec, 
                         X = xmat, 
                         lower_trunc = 1,
                         lower_censor = 4,
                         method = "BFGS",
                         control = list(maxit = 3000),
                         hessian = T)
    
    if(model_fit_gr$convergence!=0){
      message("Model did not converge with BFGS.")
      return(NULL)
    }
    
    estimates <- model_fit_gr$par

    ####
    # 1. Score matrix: each row = gradient at obs i
    S <- nb_score_matrix(estimates, X = xmat, y = yvec)
    
    # 2. Outer product of gradients
    G <- crossprod(S)
    
    # 3. Hessian from numDeriv
    H <- hessian(func = nb_cen_trunc_ll, x = estimates, method.args = list(eps = 1e-8), X = xmat, y = yvec)
    
    # 4. Sandwich estimator
    vcov_robust <- solve(H) %*% G %*% solve(H)
    se_robust <- sqrt(diag(vcov_robust))
    z = estimates / se_robust
    
    # 5. Summary table
    ti <- data.frame(
      estimate = estimates,
      std.error = se_robust,
      p.value = 2 * pnorm(-abs(estimates / se_robust)),
      conf.low = estimates - 1.96 * se_robust,
      conf.high = estimates + 1.96 * se_robust
    ) %>%
      rownames_to_column(var = "term") %>%
      as_tibble() 


    cs = -1/ti$estimate[ti$term=="cost_total_weighted"]
    cs_hi = -1/ti$conf.high[ti$term=="cost_total_weighted"]
    cs_lo = -1/ti$conf.low[ti$term=="cost_total_weighted"]
    cs_day = cs/vr
    cs_day_hi = cs_hi/vr
    cs_day_lo = cs_lo/vr
    
    gl <- data.frame(
      nobs = nobs,
      cs = glue("{round(cs)} [{round(cs_lo)}, {round(cs_hi)}]"),
      cs_day = glue("{round(cs_day)} [{round(cs_day_lo)}, {round(cs_day_hi)}]")
    )
    
    mod <- list(tidy=ti,glance=gl)
    class(mod) <- "modelsummary_list"
    
   
    

    ###################################
    
    
    
    #saveRDS(bs_runs,paste0(bs_dir_name,"/",nm,".rds"))
    
    return(mod)
  })

}
###################
names(all_results) <- m_names
mod_print <- keep(all_results,function(x) !is.null(x))
modelsummary(mod_print,star=T)

# hist(bs_runs[,2])
# 
# check_mean = summarize(bs_runs,across(everything(),mean))
# 
# check_mean
# 1/check_mean[1,2]
