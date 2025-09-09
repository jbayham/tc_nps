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

# Negative Binomial log-likelihood with truncation and censoring
nb_cen_trunc_ll <- function(par,        # c(beta, log_theta)
                            X,          # Design matrix
                            y,          # Observed (possibly censored) outcomes
                            lower_trunc = 1,
                            lower_censor = 4) {
  
  # Separate coefficients and dispersion
  p <- ncol(X)
  beta <- par[1:p]
  log_theta <- par[p + 1]
  theta <- exp(log_theta)  # Ensure positivity
  
  mu <- exp(X %*% beta)  # Mean of NB distribution
  
  ll <- numeric(length(y))
  
  for (i in seq_along(y)) {
    # Probability that true y > lower_trunc (for truncation correction)
    p_trunc <- pnbinom(lower_trunc, size = theta, mu = mu[i])
    
    if (y[i] > lower_censor) {
      # Fully observed: standard NB pmf
      ll[i] <- dnbinom(y[i], size = theta, mu = mu[i], log = TRUE) - log(1 - p_trunc)
    } else if (y[i] == lower_censor) {
      # Censored: sum probability from (lower_trunc+1):lower_censor
      p_cens <- pnbinom(lower_censor, size = theta, mu = mu[i]) - p_trunc
      ll[i] <- log(p_cens) - log(1 - p_trunc)
    }
  }
  
  return(-sum(ll))  # Negative log-likelihood for minimization
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
      fn = nb_cen_trunc_ll,
      X = as.matrix(X_boot),
      y = y_boot,
      method = "Nelder-Mead",
      control = list(maxit = 2000)
    )
    
    # Store the coefficients if model converges
    if (fit$convergence == 0) {
      result <- data.frame(t(fit$par))
      colnames(result) <- c("constant",c_names,"ltheta")
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
  
  plan(multisession(workers = 10))
  
  #Not all bs runs converge and we only save those that do. This outer loop checks how many have converged and enters a while loop.
  num_runs=0
  target_runs=n_boot
  while(num_runs < target_runs){
  
  # Perform bootstrap iterations
  future_walk(.x = start_boot:(start_boot+n_boot),
              .options = furrr_options(seed = TRUE),
              .f = function(b,out_dir){
                #pb$tick()
                # Resample indices
                set.seed(b) #set seed for reproducibility
                boot_indices <- sample(1:length(y), size = length(y), replace = TRUE)
                
                # Resample data
                c_names <- c("cost_total_weighted","income","age","householdsize","residing")
                #c_names <- c("cost_total_weighted","age","householdsize","residing")
                start_beta <- rep(0,ncol(X),1)
                X_boot <- X[boot_indices, , drop = FALSE]
                y_boot <- y[boot_indices]
                
                # Refit the model using Nelder-Mead
                fit <- optim(
                  par = start_beta,
                  fn = nb_cen_trunc_ll,
                  X = as.matrix(X_boot),
                  y = y_boot,
                  method = "Nelder-Mead",
                  control = list(maxit = max_iter)
                )
                
                # Store the coefficients if model converges
                if (fit$convergence %in% c(0,10)) {
                  result <- data.frame(t(fit$par))
                  colnames(result) <- c("constant",c_names)
                  saveRDS(result, file = file.path("/data/jbuser/git_projects/tc_nps",out_dir, paste0("bootstrap_iter_", b, ".rds")))
                }
              },.progress = TRUE,
              out_dir = output_dir)
    
    file_list <- dir_ls(output_dir, type = "file", recurse = FALSE)
    num_runs = length(file_list)
    
    start_boot=start_boot+n_boot
    
    suc_rate = num_runs/n_boot
    n_boot = (target_runs - num_runs)/suc_rate
    
  }
}


bootstrap_estimation <- function(X, 
                                 y, 
                                 start_boot = 1, 
                                 n_boot = 10, 
                                 output_dir = "analysis/cache/bs_runs", 
                                 nm = NULL, 
                                 c_names = NULL, 
                                 max_iter = 2000, 
                                 parallel = FALSE, 
                                 workers = 4
                                 ) {
  # Dependencies
  require(fs)
  require(progress)
  if (parallel) {
    require(furrr)
    plan("multisession", workers = workers)
  }
  
  # Input checks
  stopifnot(is.matrix(X), is.numeric(y), length(y) == nrow(X))
  if (is.null(c_names)) {
    c_names <- colnames(X)
    if (is.null(c_names)) stop("Please provide `c_names` or use a matrix with named columns.")
  }
  
  dir_ifnot(output_dir)

  
  # Define single bootstrap iteration
  bootstrap_loop <- function(b) {
    file_path <- file.path(output_dir, paste0("bootstrap_iter_", b, ".rds"))
    if (file.exists(file_path)) return(NULL)
    
    set.seed(b)
    boot_indices <- sample(seq_along(y), size = length(y), replace = TRUE)
    X_boot <- X[boot_indices, , drop = FALSE]
    y_boot <- y[boot_indices]
    
    start_beta <- c(0, rep(0,length(c_names)),1) # Starting values for the optimization
    
    
    model_fit <- try(
      optim(par = start_beta,
            fn = nb_cen_trunc_ll,
            y = y_boot,
            X = X_boot,
            lower_trunc = 1,
            lower_censor = 4,
            method = "Nelder-Mead",
            control = list(maxit = max_iter),
            hessian = FALSE),
      silent = TRUE
    )
    
    if (!inherits(model_fit, "try-error") && model_fit$convergence == 0) {
      result <- data.frame(t(model_fit$par))
      colnames(result) <- if (length(model_fit$par) == length(c_names) + 1) {
        c("constant", c_names)
      } else {
        c("constant", c_names, "ltheta")
      }
      saveRDS(result, file = file_path)
    }
  }
  
  # Execute bootstrap loop
  boots <- start_boot:(start_boot + n_boot)
  if (parallel) {
    furrr::future_walk(boots, bootstrap_loop, .options = furrr_options(seed = TRUE), .progress = TRUE)
  } else {
    pb <- progress_bar$new(format = " [:bar] :percent eta: :eta", total = length(boots), clear = FALSE, width = 60)
    for (b in boots) {
      bootstrap_loop(b)
      pb$tick()
    }
  }
  
  invisible(NULL)
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
                     bs_dat, #bootstrap runs
                     nobs, #number of obs for table
                     tc_coef_name = "cost_total_weighted",
                     label_names = c("Travel Cost"="cost_total_weighted","Income"="income","Age"="age","Household Size"="householdsize","Devices"="residing","Constant"="constant"),
                     visit_ratio
                    ){
  
  require(glue)
  
  #Ensuring data is in the correct order
  reg_coef = reg_coef[label_names]
  bs_dat = bs_dat[,label_names]
  
  boot_se = apply(bs_dat,2,sd)  #bootstrap standard errors
  #observed_t = reg_coef / boot_se #observed t values
  
  p_values = bs_p_val(coefs = reg_coef, #
                      boot_se = boot_se,
                      boot_mat = as.matrix(bs_dat))
  
  bootstrap_ci <- apply(bs_dat, 2, quantile, probs = c(0.025, 0.975))
  
  
  cs = -1/reg_coef[tc_coef_name]
  cs_se = sd(-1/bs_dat[,tc_coef_name],na.rm = TRUE)
  cs_ci = quantile(-1/bs_dat[,tc_coef_name],probs = c(0.025, 0.975),na.rm = TRUE)
  cs_day = cs/visit_ratio
  cs_day_ci = quantile(-1/bs_dat[,tc_coef_name]/visit_ratio,probs = c(0.025, 0.975),na.rm = TRUE)
  
  ti <- data.frame(
    term = names(label_names),
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
  
  cs_dat <- data.frame(
    cs = cs,
    cs_l = cs_ci[1],
    cs_u = cs_ci[2],
    cs_day = cs_day,
    cs_day_l = cs_day_ci[1],
    cs_day_u = cs_day_ci[2]
  )
  
  
  mod <- list(tidy=ti,glance=gl)
  class(mod) <- "modelsummary_list"
  
  return(list(mod,cs_dat))
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
