# Shared packages, options and functions for all scripts.
# Every script starts with source("code/00_functions.R"); run from the project root.

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
})

options(scipen = 999, dplyr.summarise.inform = FALSE)

# Output folders
dir.create("data_inter", showWarnings = FALSE)
dir.create("figures",    showWarnings = FALSE)
dir.create("tables",     showWarnings = FALSE)
dir.create("output",     showWarnings = FALSE)

# Age-group labels used throughout (lower bound -> label)
age_labs <- c(`0` = "0-1", `1` = "1-4", `5` = "5-9", `10` = "10-14",
              `15` = "15-19", `20` = "20-24")

# Standardise CDC WONDER exports: WONDER appends a "---" footer with query
# metadata; those rows have NA in the grouping columns and are dropped by the
# drop_na() calls in each script.

# Estimation of baseline deaths with confidence and bootstrapped prediction intervals
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Quasi-Poisson GLM, log-linear in calendar year with log(exposure) offset,
# fitted to pre-pandemic years (weights 0 for year >= 2020) and projected
# over the whole period.
#   bsn    : fitted baseline deaths
#   lc, uc : 95% confidence interval of the baseline (bsn -/+ 1.96 * SE)
#   lp, up : 95% prediction interval from simul_intvals() (nsim draws)
est_baseline_pi <- function(chunk, nsim = 200){

  chunk2 <-
    chunk %>%
    arrange(year) %>%
    mutate(w = ifelse(year >= 2020, 0, 1),
           t = 1:n())

  model <- glm(dts ~ year + offset(log(exposure)),
               weights = w,
               family = "quasipoisson",
               data = chunk2)

  res <- try(predict(model,
                     newdata = chunk2,
                     type = "response",
                     se.fit = TRUE),
             silent = TRUE)

  if (inherits(res, "try-error")) {
    return(chunk2 %>% mutate(bsn = NA_real_, lp = NA_real_, up = NA_real_,
                             lc = NA_real_, uc = NA_real_))
  }

  chunk2 %>%
    mutate(bsn = res$fit,
           lc = bsn - 1.96 * res$se.fit,
           uc = bsn + 1.96 * res$se.fit) %>%
    left_join(simul_intvals(model,
                            model_type = "glm",
                            db = chunk2,
                            nsim = nsim,
                            p = 0.95),
              by = "t")
}


# Bootstrapped prediction intervals
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# adapted from Jonas Schoeley's method https://github.com/jschoeley/rbx2020
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

    # offsets extracted directly from the prediction data
    offset_prd <- matrix(log(db$exposure))

    # draws of the coefficients from their sampling distribution
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
        lp = quantile(dts_sim, lp, na.rm = TRUE),
        up = quantile(dts_sim, up, na.rm = TRUE),
        .groups = 'drop'
      )

    return(ints_simul)
  }

# Shared ggplot theme for the bar figures (Figures 1 and 3)
theme_bars <- function(){
  theme_bw() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.spacing      = unit(4, "pt"),
      strip.placement    = "outside",
      strip.background   = element_blank(),
      strip.text.x       = element_text(size = 9, hjust = 0, margin = margin(b = 2)),
      strip.text.y.left  = element_text(angle = 0, vjust = 1, size = 9,
                                        face = "bold", margin = margin(r = 3)),
      axis.text          = element_text(size = 8),
      plot.margin        = margin(2, 2, 2, 2)
    )
}
