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



nb_cen_trunc_ll <- function(par, X, y, lower_trunc = 1, lower_censor = 4) {
  
  # Unpack parameters
  p <- ncol(X)
  beta <- par[1:p]
  log_theta <- par[p + 1]
  theta <- exp(log_theta)
  
  # Linear predictor and mean
  mu <- exp(X %*% beta)
  
  # Precompute truncation probabilities
  p_trunc <- pnbinom(lower_trunc, size = theta, mu = mu)
  log_denom <- log1p(-p_trunc)  # log(1 - p_trunc) = log1p(-p_trunc) for numerical stability
  
  # Logical indexing
  is_observed <- y > lower_censor
  is_censored <- y == lower_censor
  
  # Initialize log-likelihood
  ll <- numeric(length(y))
  
  # Observed (not censored)
  if (any(is_observed)) {
    ll[is_observed] <- dnbinom(y[is_observed], size = theta, mu = mu[is_observed], log = TRUE) - 
      log_denom[is_observed]
  }
  
  # Censored: sum P(y in (lower_trunc+1):lower_censor)
  if (any(is_censored)) {
    p_cens <- pnbinom(lower_censor, size = theta, mu = mu[is_censored]) - 
      pnbinom(lower_trunc, size = theta, mu = mu[is_censored])
    ll[is_censored] <- log(p_cens) - log_denom[is_censored]
  }
  
  # Return negative log-likelihood for minimization
  return(-sum(ll))
}

nb_cen_trunc_grad_vec <- function(par, X, y, lower_trunc = 1, lower_censor = 4) {
  p <- ncol(X)
  beta <- par[1:p]
  log_theta <- par[p + 1]
  theta <- exp(log_theta)
  
  mu <- drop(exp(X %*% beta))  # vector of means
  
  # Logical indexing
  is_obs <- y > lower_censor
  is_cens <- y == lower_censor
  
  ### Finite diff epsilon
  eps <- 1e-6
  
  ### Central diff d loglik / d mu
  mu_up <- mu + eps
  mu_dn <- mu - eps
  
  # f(y | mu)
  ll_obs_up <- dnbinom(y[is_obs], mu = mu_up[is_obs], size = theta, log = TRUE)
  ll_obs_dn <- dnbinom(y[is_obs], mu = mu_dn[is_obs], size = theta, log = TRUE)
  dlogf_dmu_obs <- (ll_obs_up - ll_obs_dn) / (2 * eps)
  
  # censoring numerator
  p_cens_up <- pnbinom(lower_censor, mu = mu_up[is_cens], size = theta)
  p_cens_dn <- pnbinom(lower_censor, mu = mu_dn[is_cens], size = theta)
  p_trunc_up <- pnbinom(lower_trunc, mu = mu_up[is_cens], size = theta)
  p_trunc_dn <- pnbinom(lower_trunc, mu = mu_dn[is_cens], size = theta)
  num_up <- p_cens_up - p_trunc_up
  num_dn <- p_cens_dn - p_trunc_dn
  # Add guards here:
  num_up <- pmax(num_up, 1e-12)
  num_dn <- pmax(num_dn, 1e-12)
  dlogf_dmu_cens <- (log(num_up) - log(num_dn)) / (2 * eps)
  
  # denom (for all)
  p_trunc_up_all <- pnbinom(lower_trunc, mu = mu + eps, size = theta)
  p_trunc_dn_all <- pnbinom(lower_trunc, mu = mu - eps, size = theta)
  # Guard p close to 1
  p_trunc_up_all <- pmin(p_trunc_up_all, 1 - 1e-8)
  p_trunc_dn_all <- pmin(p_trunc_dn_all, 1 - 1e-8)
  dlogdenom_dmu <- (log1p(-p_trunc_up_all) - log1p(-p_trunc_dn_all)) / (2 * eps)
  
  # Combine dloglik/dmu
  dloglik_dmu <- numeric(length(y))
  dloglik_dmu[is_obs] <- dlogf_dmu_obs - dlogdenom_dmu[is_obs]
  dloglik_dmu[is_cens] <- dlogf_dmu_cens - dlogdenom_dmu[is_cens]
  
  ### Gradient w.r.t beta: chain rule dloglik/dmu * dmu/dbeta = dloglik/dmu * mu * X
  grad_beta <- crossprod(X, dloglik_dmu * mu)  # p x 1
  
  ### Gradient w.r.t. log(theta)
  theta_up <- theta * exp(eps)
  theta_dn <- theta * exp(-eps)
  
  grad_log_theta <- numeric(length(y))
  
  # observed
  ll_obs_up <- dnbinom(y[is_obs], mu = mu[is_obs], size = theta_up, log = TRUE)
  ll_obs_dn <- dnbinom(y[is_obs], mu = mu[is_obs], size = theta_dn, log = TRUE)
  dlogf_dth_obs <- (ll_obs_up - ll_obs_dn) / (2 * eps)
  
  denom_up <- pnbinom(lower_trunc, mu = mu[is_obs], size = theta_up)
  denom_dn <- pnbinom(lower_trunc, mu = mu[is_obs], size = theta_dn)
  # Guard
  denom_up <- pmin(denom_up, 1 - 1e-8)
  denom_dn <- pmin(denom_dn, 1 - 1e-8)
  dlogdenom_dth_obs <- (log1p(-denom_up) - log1p(-denom_dn)) / (2 * eps)
  
  grad_log_theta[is_obs] <- (dlogf_dth_obs - dlogdenom_dth_obs) * theta
  
  # censored
  pc_up <- pnbinom(lower_censor, mu = mu[is_cens], size = theta_up)
  pc_dn <- pnbinom(lower_censor, mu = mu[is_cens], size = theta_dn)
  pt_up <- pnbinom(lower_trunc, mu = mu[is_cens], size = theta_up)
  pt_dn <- pnbinom(lower_trunc, mu = mu[is_cens], size = theta_dn)
  num_up <- pc_up - pt_up
  num_dn <- pc_dn - pt_dn
  num_up <- pmax(num_up, 1e-12)
  num_dn <- pmax(num_dn, 1e-12)
  dlogf_dth_cens <- (log(num_up) - log(num_dn)) / (2 * eps)
  
  denom_up <- pnbinom(lower_trunc, mu = mu[is_cens], size = theta_up)
  denom_dn <- pnbinom(lower_trunc, mu = mu[is_cens], size = theta_dn)
  denom_up <- pmin(denom_up, 1 - 1e-8)
  denom_dn <- pmin(denom_dn, 1 - 1e-8)
  dlogdenom_dth_cens <- (log1p(-denom_up) - log1p(-denom_dn)) / (2 * eps)
  
  grad_log_theta[is_cens] <- (dlogf_dth_cens - dlogdenom_dth_cens) * theta
  
  ### Final gradient
  grad <- -c(grad_beta, sum(grad_log_theta))  # NEGATIVE log-likelihood gradient
  
  return(as.numeric(grad))
  
  if (any(!is.finite(grad))) {
    warning("Non-finite values in gradient")
  }
}

# Score matrix function
nb_score_matrix <- function(par, X, y, lower_trunc = 1, lower_censor = 4) {
  n <- nrow(X)
  k <- length(par)
  S <- matrix(NA_real_, nrow = n, ncol = k)
  
  for (i in seq_len(n)) {
    # Gradient contribution for individual i
    grad_i <- nb_cen_trunc_grad_vec(par, X = X[i, , drop = FALSE], y = y[i],
                                    lower_trunc = lower_trunc, lower_censor = lower_censor)
    S[i, ] <- -grad_i  # remove the negative because your grad is -loglik
  }
  
  return(S)
}



# Bootstrap estimation function
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
  message(paste0("writing to ",output_dir))
  
  local({
  # Dependencies
  require(fs)
  require(progress)
  if (parallel) {
    require(furrr)
    # check if running rstudio in interactive mode
    if (interactive() && Sys.getenv("RSTUDIO") == "1") {
      plan("multisession", workers = workers)
    } else {
      plan("multicore", workers = workers)
    }
  }
  
  # Input checks
  stopifnot(is.matrix(X), is.numeric(y), length(y) == nrow(X))
  if (is.null(c_names)) {
    c_names <- colnames(X)
    if (is.null(c_names)) stop("Please provide `c_names` or use a matrix with named columns.")
  }
  
  dir_ifnot(output_dir)

  
  # Define single bootstrap iteration
  bootstrap_loop <- function(b,output_dir) {
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
    rm(modelfit)
    return(NULL)
  }
  
  # Execute bootstrap loop
  boots <- start_boot:(start_boot + n_boot)
  if (parallel) {
    furrr::future_walk(boots, bootstrap_loop, path_abs(output_dir), .options = furrr_options(seed = TRUE), .progress = F)
  } else {
    #pb <- progress_bar$new(format = " [:bar] :percent eta: :eta", total = length(boots), clear = FALSE, width = 60)
    for (b in boots) {
      bootstrap_loop(b,path_abs(output_dir))
      #pb$tick()
    }
  }
  
  })
  
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
  #Improved bootstrap function with parallel processing and error handling 
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
