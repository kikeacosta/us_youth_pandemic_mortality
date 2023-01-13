library(tidyverse)
library(readxl)
# library(here)
library(lubridate)
# library(ungroup)
# library(scales)
library(mgcv)
# library(countrycode)
# library(HMDHFDplus)
library(ggh4x)
# library(cowplot)

options(scipen=999)
options(scipen=3)

copy_this <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,file = paste0("clipboard-", object.size(x)),sep="\t",row.names=row.names,col.names=col.names,...)
}


# estimation of baseline dts with bootstrapped prediction intervals
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
est_baseline_pi <- function(chunk){
  
  chunk2 <- 
    chunk %>% 
    arrange(year) %>% 
    mutate(w = ifelse(year >= 2020, 0, 1),
           t = 1:n())
  
  model <- glm(dts ~ year + offset(log(exposure)), 
               weights = w,
               family = "quasipoisson",
               data = chunk2)
  
  test <- 
    try(
      res <- 
        predict(model, 
                newdata = chunk2,
                type = "response", 
                se.fit = TRUE)
    )
  
  try(
    chunk3 <- 
      chunk2 %>% 
      mutate(bsn = res$fit,
             bsn_lc = bsn - 1.96 * res$se.fit,
             bsn_uc = bsn + 1.96 * res$se.fit) %>% 
      left_join(simul_intvals(model, 
                              model_type = "glm", 
                              db = chunk2, 
                              nsim = 1000,
                              p = 0.95),
                by = "t")
  )
  
  if(class(test) == "try-error"){
    chunk3 <- 
      chunk2 %>% 
      mutate(bsn = NA,
             bsn_lp = NA,
             bsn_up = NA,
             bsn_lc = NA,
             bsn_uc = NA)
  }
  
  return(chunk3)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function for bootstrapping ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# bootstrapping process, adapted from Jonas schoeley's method 
# https://github.com/jschoeley/rbx2020
simul_intvals <- 
  function(
    # fitted model 
    model, 
    # either GLM or GAM (needed for model matrix extraction step)
    model_type, 
    # prediction data
    db, 
    # number of iterations
    nsim, 
    # prediction intervals' uncertainty level (between 0 and 1)
    p
  ){
    
    # defining upper and lower prediction quantiles
    lp <- (1 - p) / 2
    up <- 1 - lp
    
    # matrix model extraction
    if(model_type == "glm"){
      X_prd <- model.matrix(model, data = db, na.action = na.pass)
    }
    if(model_type == "gam"){
      X_prd <- predict(model, newdata = db, type = 'lpmatrix')
    }
    
    # estimated coefficients
    beta <- coef(model)
    
    # offsets extracted directly from the prediction data
    offset_prd <- matrix(log(db$exposure))
    # model.offset(x)
    
    # extracting variance covariance matrix
    beta_sim <- MASS::mvrnorm(nsim, 
                              coef(model), 
                              suppressWarnings(vcov(model)))
    
    # simulation process
    Ey_sim <- apply(beta_sim, 1, FUN = function (b) exp(X_prd %*% b + offset_prd))
    
    y_sim <- apply(Ey_sim, 2, FUN = function (Ey) {
      y <- mu <- Ey
      # NA's can't be passed to the simulation functions, so keep them out
      idx_na <- is.na(mu) 
      mu_ <- mu[!idx_na] 
      N <- length(mu_)
      phi <- suppressWarnings(summary(model)$dispersion)
      # in case of under-dispersion, sample from Poisson
      if (phi < 1) { phi = 1 }
      y[!idx_na] <- rnbinom(n = N, mu = mu_, size = mu_/(phi-1))      
      return(y)
    })
    
    # from wide to tidy format
    ints_simul <- 
      db %>% 
      select(t)
    
    colnames_y_sim <- paste0('dts_sim', 1:nsim)
    
    ints_simul[,colnames_y_sim] <- y_sim
    
    # prediction intervals output
    ints_simul <-
      ints_simul %>%
      pivot_longer(cols = starts_with('dts_sim'),
                   names_to = 'sim_id', values_to = 'dts_sim') %>%
      group_by(t) %>%
      summarise(
        bsn_lp = quantile(dts_sim, lp, na.rm = TRUE),
        bsn_up = quantile(dts_sim, up, na.rm = TRUE), 
        .groups = 'drop'
      ) 
    
    return(ints_simul)
  }

