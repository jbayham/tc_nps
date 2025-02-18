#Analysis functions
#Define the log likelihood function
poi_cen_trunc_ll <- function(beta, #vector of coefficients
                             X,    #Design matrix of covariates
                             y,    #Dependent var (trips)
                             lower_trunc = 1, #Lower truncation point
                             lower_censor = 4) { #Lower censoring point
  
  # Compute lambda for each observation: lambda_i = exp(X_i * beta)
  if(!is.matrix(X)){
    X=as.matrix(X)
  }
  
  lambda <- exp(X %*% beta)
  
  # Log-likelihood components
  ll <- numeric(length(y))
  
  for (i in seq_along(y)) {
    if (y[i] > lower_censor) {
      # Fully observed data (y > 4)
      ll[i] <- dpois(y[i], lambda[i], log = TRUE) - log(1 - ppois(lower_trunc, lambda[i]))
    } else if (y[i] == lower_censor) {
      # Censored data (2 <= y <= 4)
      censored_prob <- ppois(lower_censor, lambda[i]) - ppois(lower_trunc, lambda[i])
      ll[i] <- log(censored_prob) - log(1 - ppois(lower_trunc, lambda[i]))
    }
  }
  
  # Return the sum of log-likelihood
  return(-sum(ll))  # Return negative log-likelihood for minimization
}

# Function to perform bootstrap
bootstrap_estimation <- function(X, #Design matrix
                                 y, #dependent variable (e.g., trips)
                                 start_boot=1, #Index to start bootstrap since it controls rng seed and file names
                                 n_boot = 10, #Number of bootstrap runs
                                 output_dir = "analysis/cache/bs_runs", #directory to save bs runs
                                 ...) { #Pass in additional arguments
  require(progress) #for printing progress bar
  require(fs) #for fast access to file system
  
  #Create dir for caching results
  if(!dir.exists(output_dir)) dir.create(output_dir,recursive = TRUE)
  
  #progress bar
  pb <- progress_bar$new(
    format = " [:bar] :percent eta: :eta",
    total = length(start_boot:(start_boot+n_boot)), clear = FALSE, width= 60)
  
  #Not all bs runs converge and we only save those that do. This outer loop checks how many have converged and enters a while loop.
  #num_runs <- length(dir_ls(output_dir, type = "file", recurse = FALSE))
  #while(n_runs < n_boot)
  
  # Perform bootstrap iterations
  for (b in start_boot:(start_boot+n_boot)) {
    pb$tick()
    # Resample indices
    set.seed(b) #set seed for reproducibility
    boot_indices <- sample(1:length(y), size = length(y), replace = TRUE)
    
    # Resample data
    X_boot <- X[boot_indices, , drop = FALSE]
    y_boot <- y[boot_indices]
    
    # Refit the model using Nelder-Mead
    fit <- optim(
      par = start_beta,
      fn = poi_cen_trunc_ll,
      X = as.matrix(X_boot),
      y = y_boot,
      method = "Nelder-Mead",
      control = list(maxit = 2000)
    )
    
    # Store the coefficients if model converges
    if (fit$convergence == 0) {
      result <- data.frame(t(fit$par))
      colnames(result) <- c("constant",c_names)
      saveRDS(result, file = file.path(output_dir, paste0("bootstrap_iter_", b, ".rds")))
    }
  }
}

# Function to perform bootstrap
bootstrap_estimation_par <- function(X, #Design matrix
                                     y, #dependent variable (e.g., trips)
                                     max_iter = 2000, #most converge in 2000
                                     start_boot=1, #Index to start bootstrap since it controls rng seed and file names
                                     n_boot = 10, #Number of bootstrap runs
                                     output_dir = "analysis/cache/bs_runs", #directory to save bs runs
                                     ...) { #Pass in additional arguments
  require(progress) #for printing progress bar
  require(fs) #for fast access to file system
  require(furrr) #for parallel estimation
  
  #Create dir for caching results
  if(!dir.exists(output_dir)) dir.create(output_dir,recursive = TRUE)
  
  #progress bar
  # pb <- progress_bar$new(
  #   format = " [:bar] :percent eta: :eta",
  #   total = length(start_boot:(start_boot+n_boot)), clear = FALSE, width= 60)
  # 
  
  plan(multisession(workers = 4))
  
  #Not all bs runs converge and we only save those that do. This outer loop checks how many have converged and enters a while loop.
  num_runs=0
  while(num_runs < n_boot){
  
  # Perform bootstrap iterations
  future_walk(.x = start_boot:(start_boot+n_boot),
              .options = furrr_options(seed = TRUE),
              .f = function(b){
                #pb$tick()
                # Resample indices
                set.seed(b) #set seed for reproducibility
                boot_indices <- sample(1:length(y), size = length(y), replace = TRUE)
                
                # Resample data
                c_names <- c("cost_total_weighted","income","age","householdsize","residing")
                #c_names <- c("cost_total_weighted","age","householdsize","residing")
                start_beta <- rep(0,ncol(X))
                X_boot <- X[boot_indices, , drop = FALSE]
                y_boot <- y[boot_indices]
                
                # Refit the model using Nelder-Mead
                fit <- optim(
                  par = start_beta,
                  fn = poi_cen_trunc_ll,
                  X = as.matrix(X_boot),
                  y = y_boot,
                  method = "Nelder-Mead",
                  control = list(maxit = max_iter)
                )
                
                # Store the coefficients if model converges
                if (fit$convergence %in% c(0)) {
                  result <- data.frame(t(fit$par))
                  colnames(result) <- c("constant",c_names)
                  saveRDS(result, file = file.path(output_dir, paste0("bootstrap_iter_", b, ".rds")))
                }
              },.progress = TRUE)
    
    file_list <- dir_ls(output_dir, type = "file", recurse = FALSE)
    num_runs = length(file_list)
    
    start_boot=start_boot+n_boot
    
  }
}


bs_p_val <- function(coefs,boot_se,boot_mat){
  
  observed_t = coefs / boot_se
  temp_t = matrix(0, nrow = nrow(boot_mat), ncol = length(coefs))
  for(i in 1:length(coefs)){
    temp_t[,i] = (boot_mat[,i] - coefs[i])/boot_se[i]
  }
  
  p_val = rep(0,length(coefs))
  for(i in 1:length(coefs)){
    p_val[i] = mean(abs(temp_t[,i]) >= abs(observed_t[i]))
  }
  return(p_val)
}


bs_table <- function(reg_coef, #coefficients from the full model
                     results, #bootstrap runs
                     nobs){ #number of obs for table
  
  require(glue)
  
  boot_se = apply(results,2,sd)  #bootstrap standard errors
  observed_t = reg_coef / boot_se #observed t values
  
  p_values = bs_p_val(coefs = reg_coef, #
                      boot_se = boot_se,
                      boot_mat = as.matrix(results))
  
  bootstrap_ci <- apply(results, 2, quantile, probs = c(0.025, 0.975))
  
  
  cs = -1/reg_coef["travel_total_cost"]
  cs_se = sd(-1/results[,"travel_total_cost"],na.rm = TRUE)
  cs_ci = quantile(-1/results[,"travel_total_cost"],probs = c(0.025, 0.975),na.rm = TRUE)
  cs_day = cs/1.45
  cs_day_ci = quantile(-1/results[,"travel_total_cost"]/1.45,probs = c(0.025, 0.975),na.rm = TRUE)
  
  ti <- data.frame(
    term = c("Travel Cost","Race","Education","Age","Devices","Constant"),
    estimate = reg_coef,
    std.error = boot_se,
    p.value = p_values,
    conf.low = bootstrap_ci[1,],
    conf.high = bootstrap_ci[2,])
  
  gl <- data.frame(
    nobs = nobs,
    cs = glue("{round(cs)} [{round(cs_ci[1])}, {round(cs_ci[2])}]"),
    cs_day = glue("{round(cs_day)} [{round(cs_day_ci[1])}, {round(cs_day_ci[2])}]")
  )
  
  
  mod <- list(tidy=ti,glance=gl)
  class(mod) <- "modelsummary_list"
  
  return(mod)
}


#Krinsky Robb standard errors for
kr_std_er <- function(est,est_err,bs_num=1000,seed_pick=20){
  
  #set seed for reproducibility
  set.seed(seed = seed_pick)
  
  #Simulate draws from normal distribution around parameter estimate
  est_sim = rnorm(n=bs_num,mean = est,sd = est_err)
  
  #Calculate cs for each sample
  cs_temp = -1/est_sim
  
  return(cs_temp)
}
