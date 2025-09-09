# ------------------------------------------------------------
# Turnkey: Poisson & NB2 with left truncation and left censoring
#   - trunc_pt = 1 (kept if y* >= 1)
#   - cens_pt  = 4 (reported as 4 if 1 <= y* <= 4)
# Robust SEs via OPG (sandwich). Model-based SEs via Hessian.
# Optional starts from glmmTMB.
# ------------------------------------------------------------

fit_cen_trunc_counts <- function(
    formula, data,
    trunc_pt = 1, cens_pt = 4,
    family_list = c("poisson", "nb2"),
    method = "BFGS",
    method_nb2 = c("BFGS","L-BFGS-B","nm_then_bfgs"),
    use_glmmtmb_starts = TRUE,
    sim_check = 500,
    verbose = TRUE
) {
  method_nb2 <- match.arg(method_nb2)
  stopifnot(trunc_pt >= 0, cens_pt >= trunc_pt)
  
  mf  <- model.frame(formula, data = data, na.action = na.omit)
  y   <- model.response(mf)
  X   <- model.matrix(attr(mf, "terms"), data = mf)
  off <- model.offset(mf); if (is.null(off)) off <- rep(0, length(y))
  n <- length(y); p <- ncol(X)
  
  .safe_log1m <- function(p, eps = 1e-12) { p <- pmin(pmax(p, 0), 1); log1p(-pmin(p, 1 - eps)) }
  .safe_log   <- function(x, eps = 1e-12) log(pmax(x, eps))
  
  ll_poisson_i <- function(eta_i, y_i, trunc_pt, cens_pt) {
    mu <- exp(eta_i); mu <- pmin(pmax(mu, 1e-12), 1e12)
    p_trunc <- ppois(trunc_pt, lambda = mu)
    if (y_i > cens_pt) dpois(y_i, lambda = mu, log = TRUE) - .safe_log1m(p_trunc)
    else if (y_i == cens_pt) { p_cens <- ppois(cens_pt, lambda = mu) - p_trunc; .safe_log(p_cens) - .safe_log1m(p_trunc) }
    else NA_real_
  }
  
  ll_nb2_i <- function(eta_i, log_theta, y_i, trunc_pt, cens_pt) {
    mu <- exp(eta_i); mu <- pmin(pmax(mu, 1e-12), 1e12)
    theta <- exp(log_theta); theta <- pmin(pmax(theta, 1e-8), 1e8)
    p_trunc <- pnbinom(trunc_pt, size = theta, mu = mu)
    if (y_i > cens_pt) dnbinom(y_i, size = theta, mu = mu, log = TRUE) - .safe_log1m(p_trunc)
    else if (y_i == cens_pt) { p_cens <- pnbinom(cens_pt, size = theta, mu = mu) - p_trunc; .safe_log(p_cens) - .safe_log1m(p_trunc) }
    else NA_real_
  }
  
  nll_poisson <- function(beta) {
    eta <- as.vector(X %*% beta + off); val <- 0.0
    for (i in seq_len(n)) { li <- ll_poisson_i(eta[i], y[i], trunc_pt, cens_pt); if (!is.finite(li)) return(1e20); val <- val - li }
    val
  }
  
  nll_nb2 <- function(par) {
    beta <- par[1:p]; log_theta <- par[p+1]
    eta <- as.vector(X %*% beta + off); val <- 0.0
    for (i in seq_len(n)) { li <- ll_nb2_i(eta[i], log_theta, y[i], trunc_pt, cens_pt); if (!is.finite(li)) return(1e20); val <- val - li }
    val
  }
  
  get_starts <- function(fam) {
    glm_poisson_starts <- function() {
      g <- glm(formula, data = data, family = poisson(link = "log"))
      b <- coef(g); b_full <- setNames(rep(0, ncol(X)), colnames(X)); b_full[names(b)] <- b; b_full
    }
    if (!use_glmmtmb_starts || !requireNamespace("glmmTMB", quietly = TRUE)) {
      if (fam == "poisson") return(list(par = glm_poisson_starts()))
      if (fam == "nb2")     return(list(par = c(glm_poisson_starts(), 0)))
    }
    safe_tmb <- function(famobj) tryCatch(glmmTMB::glmmTMB(formula, data = data, family = famobj), error = function(e) NULL)
    if (fam == "poisson") {
      fit <- safe_tmb(poisson(link = "log")); if (is.null(fit)) return(list(par = glm_poisson_starts()))
      b <- tryCatch(glmmTMB::fixef(fit)$cond, error = function(e) NULL)
      if (is.null(b) || any(!is.finite(b))) return(list(par = glm_poisson_starts()))
      b_full <- setNames(rep(0, ncol(X)), colnames(X)); b_full[names(b)] <- b; return(list(par = b_full))
    }
    if (fam == "nb2") {
      fit <- safe_tmb(glmmTMB::nbinom2(link = "log")); if (is.null(fit)) return(list(par = c(glm_poisson_starts(), 0)))
      b <- tryCatch(glmmTMB::fixef(fit)$cond, error = function(e) NULL)
      if (is.null(b) || any(!is.finite(b))) return(list(par = c(glm_poisson_starts(), 0)))
      b_full <- setNames(rep(0, ncol(X)), colnames(X)); b_full[names(b)] <- b
      return(list(par = c(b_full, 0)))
    }
  }
  
  requireNamespace("numDeriv", quietly = TRUE)
  
  compute_ses <- function(opt, score_i, nll_fun) {
    J <- crossprod(score_i)
    bad_H <- function(H) !is.matrix(H) || any(!is.finite(H)) || nrow(H)!=ncol(H)
    H <- opt$hessian
    if (bad_H(H)) {
      H_num <- try(numDeriv::hessian(nll_fun, opt$par), silent = TRUE)
      if (!inherits(H_num, "try-error")) H <- H_num
    }
    H <- 0.5 * (H + t(H))
    eig <- try(eigen(H, symmetric = TRUE, only.values = TRUE)$values, silent = TRUE)
    if (!inherits(eig, "try-error")) {
      min_ev <- suppressWarnings(min(eig, na.rm = TRUE))
      if (!is.finite(min_ev) || min_ev <= 1e-8) H <- H + diag((if (is.finite(min_ev)) abs(min_ev) else 1) + 1e-6, nrow(H))
    }
    Hinv <- try(solve(H), silent = TRUE)
    if (inherits(Hinv, "try-error")) { if (!requireNamespace("MASS", quietly = TRUE)) stop("Need MASS::ginv()"); Hinv <- MASS::ginv(H) }
    
    V_model  <- Hinv
    V_robust <- Hinv %*% J %*% Hinv
    se_model  <- sqrt(pmax(diag(V_model),  0))
    se_robust <- sqrt(pmax(diag(V_robust), 0))
    
    if (any(!is.finite(se_robust))) {
      Jinv <- try(solve(J), silent = TRUE)
      if (inherits(Jinv, "try-error")) { if (!requireNamespace("MASS", quietly = TRUE)) stop("Need MASS::ginv()"); Jinv <- MASS::ginv(J) }
      se_robust <- sqrt(pmax(diag(Jinv), 0))
    }
    list(se_model = se_model, se_robust = se_robust)
  }
  
  fit_one <- function(fam) {
    if (fam == "poisson") {
      st <- get_starts("poisson")$par
      if (length(st) != p || any(!is.finite(st))) {
        g_start <- glm(formula, data = data, family = poisson(link="log"))
        b_start <- coef(g_start); b_aligned <- setNames(rep(0, ncol(X)), colnames(X)); b_aligned[names(b_start)] <- b_start; st <- b_aligned
      }
      opt <- optim(st, nll_poisson, method = method, hessian = TRUE, control = list(maxit = 2000))
      beta_hat <- opt$par
      
      score_i <- matrix(NA_real_, nrow = n, ncol = p)
      for (i in seq_len(n)) {
        score_i[i, ] <- numDeriv::grad(function(b) ll_poisson_i((X[i, , drop=FALSE] %*% b) + off[i], y[i], trunc_pt, cens_pt), beta_hat)
      }
      ses <- compute_ses(opt, score_i, nll_poisson)
      
      ll_value <- -nll_poisson(beta_hat); k <- p
      list(family="poisson", par=beta_hat, se_model=ses$se_model, se_robust=ses$se_robust,
           logLik=ll_value, AIC=-2*ll_value+2*k, BIC=-2*ll_value+log(n)*k,
           converged = opt$convergence == 0, message = opt$message)
      
    } else if (fam == "nb2") {
      st <- get_starts("nb2")$par
      if (length(st) != p + 1 || any(!is.finite(st))) st <- c(rep(0, p), 0)
      
      if (method_nb2 == "L-BFGS-B") {
        lower <- c(rep(-Inf, p), -10); upper <- c(rep( Inf, p), 10)
        opt <- optim(st, nll_nb2, method = "L-BFGS-B", lower = lower, upper = upper, hessian = TRUE, control = list(maxit = 4000))
      } else if (method_nb2 == "nm_then_bfgs") {
        opt_nm <- optim(st, nll_nb2, method = "Nelder-Mead", control = list(maxit = 800))
        opt    <- optim(opt_nm$par, nll_nb2, method = "BFGS", hessian = TRUE, control = list(maxit = 4000))
      } else {
        opt <- optim(st, nll_nb2, method = method, hessian = TRUE, control = list(maxit = 3000))
      }
      
      par_hat <- opt$par; beta_hat <- par_hat[1:p]; log_theta_hat <- par_hat[p+1]; theta_hat <- exp(log_theta_hat)
      
      score_i <- matrix(NA_real_, nrow = n, ncol = p + 1)
      for (i in seq_along(y)) {
        score_i[i, ] <- numDeriv::grad(function(pp) { b <- pp[1:p]; lt <- pp[p+1]; ll_nb2_i((X[i, , drop=FALSE] %*% b) + off[i], lt, y[i], trunc_pt, cens_pt) }, par_hat)
      }
      ses <- compute_ses(opt, score_i, nll_nb2)
      
      ll_value <- -nll_nb2(par_hat); k <- p + 1
      collapse_flag <- is.finite(theta_hat) && theta_hat > 1e6
      
      list(family="nb2", par=c(beta_hat, log_theta_hat), theta=theta_hat,
           collapse_to_poisson = collapse_flag,
           se_model=ses$se_model, se_robust=ses$se_robust,
           logLik=ll_value, AIC=-2*ll_value+2*k, BIC=-2*ll_value+log(n)*k,
           converged = opt$convergence == 0, message = opt$message)
    }
  }
  
  fits <- lapply(family_list, fit_one); names(fits) <- family_list
  if (all(c("poisson","nb2") %in% names(fits))) {
    if (!is.null(fits$nb2$AIC) && !is.null(fits$poisson$AIC)) {
      if (abs(fits$nb2$AIC - fits$poisson$AIC) < 1e-6) fits$nb2$collapse_to_poisson <- TRUE
    }
  }
  
  comp <- do.call(rbind, lapply(fits, function(m) {
    data.frame(family = m$family, logLik = m$logLik, AIC = m$AIC, BIC = m$BIC,
               converged = m$converged, collapse_to_poisson = isTRUE(m$collapse_to_poisson))
  }))
  rownames(comp) <- NULL
  
  simulate_one <- function(fam_fit, R = sim_check) {
    if (R <= 0) return(data.frame())
    if (fam_fit$family == "poisson") {
      beta <- fam_fit$par; eta <- as.vector(X %*% beta + off); mu <- exp(eta)
      sim_stats <- replicate(R, { ystar <- rpois(n, mu); keep <- (ystar >= trunc_pt); ytr <- ystar[keep]; yobs <- ifelse(ytr <= cens_pt, cens_pt, ytr)
      c(mean = mean(yobs), var = var(yobs), p_cens = mean(yobs == cens_pt)) })
    } else {
      beta <- fam_fit$par[1:p]; theta <- exp(fam_fit$par[p+1]); eta <- as.vector(X %*% beta + off); mu <- exp(eta)
      sim_stats <- replicate(R, { ystar <- rnbinom(n, mu = mu, size = theta); keep <- (ystar >= trunc_pt); ytr <- ystar[keep]; yobs <- ifelse(ytr <= cens_pt, cens_pt, ytr)
      c(mean = mean(yobs), var = var(yobs), p_cens = mean(yobs == cens_pt)) })
    }
    as.data.frame(t(sim_stats))
  }
  
  sims <- lapply(fits, simulate_one, R = sim_check); names(sims) <- names(fits)
  
  out <- list(call = match.call(), formula = formula,
              trunc_pt = trunc_pt, cens_pt = cens_pt,
              families_fit = family_list, fits = fits,
              compare = comp, sims = sims, X = X, y = y, offset = off)
  class(out) <- "cenTruncCountFit"
  if (verbose) message("Fit complete. Use `$compare` for AIC/BIC; inspect `$fits[['nb2']]` for theta/flags.")
  out
}


# --------- Pretty print
print.cenTruncCountFit <- function(x, ...) {
  cat("Censored/Truncated Count Fits (trunc >=", x$trunc_pt, ", cens =", x$cens_pt, ")\n")
  print(x$compare, row.names = FALSE)
  invisible(x)
}


reg_dat <- readRDS("analysis/inputs/regs_dem/reg_dat_CARE_2021-06-01-2022-05-01.rds")
reg_dat <- readRDS("analysis/inputs/regs_dem/reg_dat_ROMO_2021-07-01-2022-06-01.rds")
reg_dat$residing <- pmax(reg_dat$residing, 1e-8)  # avoid log(0)

test <- fit_cen_trunc_counts(data=reg_dat,formula = visits ~ cost_total_weighted + income + age + offset(log(residing)),
                             use_glmmtmb_starts = FALSE)
test
test$fits$nb2$theta


test_nb <- fit_cen_trunc_counts(
  visits ~ cost_total_weighted + income + age + offset(log(residing)),
  data = reg_dat,
  family_list = c("poisson","nb2"),
  method = "BFGS",
  method_nb2 = "L-BFGS-B",   # <—
  use_glmmtmb_starts = FALSE,
  sim_check = 0,
  verbose = FALSE
)

test_nb
test_nb$fits$nb2$theta

test$fits$nb2$se_model
test_nb$fits$nb2$se_model


########################

# files to process
files <- list.files("analysis/inputs/regs_dem", full.names = TRUE)

# helper: parse 4-letter capital park code from your filenames
# e.g., "analysis/.../reg_dat_AZRU_2022-07-01-2023-06-01.rds" -> "AZRU"
parse_park <- function(path) {
  m <- str_match(path, "reg_dat_([A-Z]{4})_")[,2]
  if (is.na(m)) NA_character_ else m
}

# loop, fit, and collect
fits_list <- map(files, function(path) {
  reg_dat <- readRDS(path)
  reg_dat$residing <- pmax(reg_dat$residing, 1e-8)  # avoid log(0) in offset
  
  fit <- fit_cen_trunc_counts(
    data    = reg_dat,
    formula = visits ~ cost_total_weighted + income + age + offset(log(residing)),
    use_glmmtmb_starts = FALSE,   # can switch to TRUE once stable
    family_list = c("poisson","nb2"),
    method = "BFGS",
    sim_check = 0,                # skip sims here for speed; turn on later if you want
    verbose = FALSE
  )
  
  list(
    park = parse_park(path),
    fit  = fit
  )
})

# 1) Model comparison table (easy to read)
compare_tbl <- map_dfr(fits_list, function(x) {
  cmp <- x$fit$compare
  cmp$park <- x$park
  cmp
}) %>%
  relocate(park) %>%
  arrange(BIC, AIC)

# 2) Coefficients table with robust SEs
coef_tbl <- map_dfr(fits_list, function(x) {
  fit <- x$fit
  park <- x$park
  Xnames <- colnames(fit$X)
  
  map_dfr(names(fit$fits), function(fam) {
    m <- fit$fits[[fam]]
    if (fam == "poisson") {
      tibble(
        park    = park,
        family  = fam,
        term    = Xnames,
        estimate = as.numeric(m$par),
        se_model = if (length(m$se_model)==length(Xnames)) as.numeric(m$se_model) else NA_real_,
        se_robust= if (length(m$se_robust)==length(Xnames)) as.numeric(m$se_robust) else NA_real_
      )
    } else if (fam == "nb2") {
      # NB2 has beta and log_theta
      terms_nb2 <- c(Xnames, "log_theta")
      est       <- as.numeric(m$par)
      se_mod    <- if (length(m$se_model)==length(terms_nb2)) as.numeric(m$se_model) else rep(NA_real_, length(terms_nb2))
      se_rob    <- if (length(m$se_robust)==length(terms_nb2)) as.numeric(m$se_robust) else rep(NA_real_, length(terms_nb2))
      
      out <- tibble(
        park     = park,
        family   = fam,
        term     = terms_nb2,
        estimate = est,
        se_model = se_mod,
        se_robust= se_rob
      )
      
      # add theta + delta-method SE for theta (optional but handy)
      out %>%
        mutate(
          theta     = ifelse(term == "log_theta", exp(estimate), NA_real_),
          theta_seR = ifelse(term == "log_theta" & is.finite(se_robust),
                             exp(estimate) * se_robust, NA_real_)
        )
    }
  })
})

# optional: best model per park (by BIC or AIC)
best_by_bic <- compare_tbl %>%
  group_by(park) %>%
  slice_min(BIC, n = 1, with_ties = FALSE) %>%
  ungroup()

best_by_aic <- compare_tbl %>%
  group_by(park) %>%
  slice_min(AIC, n = 1, with_ties = FALSE) %>%
  ungroup()

# print a quick view
print(compare_tbl, n = 20)
print(best_by_bic, n = 50)
print(best_by_aic, n = 50)


library(dplyr)
library(knitr)

# Build best-by tables, pretty-print, and copy to clipboard (Mac/Win/Linux)
email_ready_best_tables <- function(compare_tbl, digits = 2,
                                    copy = TRUE,
                                    save_csv = FALSE,
                                    out_dir = "analysis/outputs") {
  
  fmt_tbl <- function(df) {
    df %>%
      mutate(across(c(logLik, AIC, BIC), ~ round(.x, digits))) %>%
      arrange(park) %>%
      select(park, family, logLik, AIC, BIC, converged)
  }
  
  best_by_bic <- compare_tbl %>%
    group_by(park) %>%
    slice_min(BIC, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    fmt_tbl()
  
  best_by_aic <- compare_tbl %>%
    group_by(park) %>%
    slice_min(AIC, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    fmt_tbl()
  
  # Markdown tables for easy pasting into email
  md_bic <- knitr::kable(best_by_bic, format = "pipe")
  md_aic <- knitr::kable(best_by_aic, format = "pipe")
  
  if (copy) {
    copy_to_clipboard <- function(txt) {
      os <- Sys.info()[["sysname"]]
      if (os == "Darwin") {                       # macOS
        con <- pipe("pbcopy", "w"); writeLines(txt, con); close(con)
      } else if (os == "Windows") {               # Windows
        utils::writeClipboard(txt)
      } else {                                    # Linux (needs xclip or xsel)
        if (nzchar(Sys.which("xclip"))) {
          con <- pipe("xclip -selection clipboard", "w")
          writeLines(txt, con); close(con)
        } else if (nzchar(Sys.which("xsel"))) {
          con <- pipe("xsel --clipboard --input", "w")
          writeLines(txt, con); close(con)
        } else {
          message("No clipboard tool found (xclip/xsel). Skipping copy.")
        }
      }
    }
    message("Copied best_by_bic to clipboard. Paste into your email.")
    copy_to_clipboard(md_bic)
    # If you also want AIC copied, uncomment:
    # copy_to_clipboard(md_aic)
  }
  
  if (save_csv) {
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    write.csv(best_by_bic, file.path(out_dir, "best_by_bic.csv"), row.names = FALSE)
    write.csv(best_by_aic, file.path(out_dir, "best_by_aic.csv"), row.names = FALSE)
  }
  
  list(
    best_by_bic = best_by_bic,
    best_by_aic = best_by_aic,
    md_bic = md_bic,
    md_aic = md_aic
  )
}

res <- email_ready_best_tables(compare_tbl, digits = 2, copy = TRUE, save_csv = T)



####################################


# ------------------------------------------------------------
# Profile NB2 dispersion (theta) under left truncation/censoring
#   - Holds log_theta fixed on a grid
#   - Optimizes beta for each grid point
#   - Returns a tidy table you can inspect/plot
# ------------------------------------------------------------
profile_nb2_theta <- function(formula, data,
                              trunc_pt = 1, cens_pt = 4,
                              theta_grid = exp(seq(log(0.1), log(100), length.out = 25)),
                              method_beta = "BFGS",
                              verbose = TRUE) {
  # Build model pieces
  mf  <- model.frame(formula, data = data, na.action = na.omit)
  y   <- model.response(mf)
  X   <- model.matrix(attr(mf, "terms"), data = mf)
  off <- model.offset(mf); if (is.null(off)) off <- rep(0, length(y))
  n <- length(y); p <- ncol(X)
  
  # Safe helpers
  .safe_log1m <- function(p, eps = 1e-12) { p <- pmin(pmax(p, 0), 1); log1p(-pmin(p, 1 - eps)) }
  .safe_log   <- function(x, eps = 1e-12) log(pmax(x, eps))
  
  # Per‑obs NB2 loglik with trunc/cens
  ll_nb2_i <- function(eta_i, log_theta, y_i) {
    mu <- exp(eta_i); mu <- pmin(pmax(mu, 1e-12), 1e12)
    theta <- exp(log_theta); theta <- pmin(pmax(theta, 1e-8), 1e8)
    p_trunc <- pnbinom(trunc_pt, size = theta, mu = mu)
    if (y_i > cens_pt) {
      dnbinom(y_i, size = theta, mu = mu, log = TRUE) - .safe_log1m(p_trunc)
    } else if (y_i == cens_pt) {
      p_cens <- pnbinom(cens_pt, size = theta, mu = mu) - p_trunc
      .safe_log(p_cens) - .safe_log1m(p_trunc)
    } else NA_real_
  }
  
  # NLL for beta given log_theta
  nll_beta_given_lt <- function(beta, log_theta) {
    eta <- as.vector(X %*% beta + off)
    s <- 0.0
    for (i in seq_len(n)) {
      li <- ll_nb2_i(eta[i], log_theta, y[i])
      if (!is.finite(li)) return(1e20)
      s <- s - li
    }
    s
  }
  
  # GLM Poisson starts for beta
  g <- glm(formula, data = data, family = poisson(link="log"))
  b_start <- coef(g)
  beta0 <- setNames(rep(0, p), colnames(X)); beta0[names(b_start)] <- b_start
  if (any(!is.finite(beta0))) beta0[] <- 0
  
  # Loop over theta grid
  out <- lapply(theta_grid, function(th) {
    lt <- log(th)
    opt <- optim(beta0, nll_beta_given_lt, log_theta = lt, method = method_beta,
                 control = list(maxit = 3000))
    beta_hat <- opt$par
    ll_val <- -nll_beta_given_lt(beta_hat, lt)
    list(theta = th, log_theta = lt, logLik = ll_val, beta = beta_hat,
         conv = (opt$convergence == 0))
  })
  
  # Tidy table
  logLik_vec <- vapply(out, `[[`, numeric(1), "logLik")
  conv_vec   <- vapply(out, `[[`, logical(1), "conv")
  theta_vec  <- vapply(out, `[[`, numeric(1), "theta")
  lt_vec     <- vapply(out, `[[`, numeric(1), "log_theta")
  
  # Pick best theta by logLik (k = p + 1 parameters for AIC/BIC)
  best_idx <- which.max(logLik_vec)
  best_beta <- out[[best_idx]]$beta
  k <- p + 1; n_obs <- length(y)
  
  prof <- data.frame(
    theta = theta_vec,
    log_theta = lt_vec,
    logLik = logLik_vec,
    AIC = -2 * logLik_vec + 2 * k,
    BIC = -2 * logLik_vec + log(n_obs) * k,
    converged_beta = conv_vec
  )
  
  if (verbose) {
    message(sprintf("Best theta ≈ %.4g (logLik = %.3f).", theta_vec[best_idx], logLik_vec[best_idx]))
    rng <- range(prof$logLik[is.finite(prof$logLik)])
    message(sprintf("LogLik range across grid: [%.3f, %.3f].", rng[1], rng[2]))
  }
  
  list(
    profile = prof[order(prof$theta), ],
    best = list(theta = theta_vec[best_idx], log_theta = lt_vec[best_idx], beta = best_beta,
                logLik = logLik_vec[best_idx]),
    coef_names = colnames(X)
  )
}

reg_dat <- readRDS("analysis/inputs/regs_dem/reg_dat_GRSM_2021-09-01-2022-08-01.rds")

reg_dat$residing <- pmax(reg_dat$residing, 1e-8)

prof <- profile_nb2_theta(
  visits ~ cost_total_weighted + income + age + offset(log(residing)),
  data = reg_dat,
  trunc_pt = 1, cens_pt = 4,
  theta_grid = exp(seq(log(0.001), log(300), length.out = 30)),  # widen if needed
  method_beta = "BFGS",
  verbose = TRUE
)

# Inspect the profile
head(prof$profile)
prof$best$theta
prof$best$logLik

# Optional: quick plot
plot(prof$profile$theta, prof$profile$logLik, type = "b", xlab = "theta", ylab = "logLik")
abline(v = prof$best$theta, lty = 2)

