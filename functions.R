Standardize <- function(y, fit.range = NULL) {
  # This function is copied from CausalImpact package
  y.fit <- y[fit.range[1] : fit.range[2]]
  y.mu <- mean(y.fit, na.rm = TRUE)
  if (is.nan(y.mu)) {
    y.mu <- NA_real_
  }
  y.sd <- sd(y.fit, na.rm = TRUE)
  y <- y - y.mu
  if (!is.na(y.sd) && (y.sd > 0)) {
    y <- y / y.sd
  }
  UnStandardize <- function(y) {
    if (!is.na(y.sd) && (y.sd > 0)) {
      y <- y * y.sd
    }
    y <- y + y.mu
    return(y)
  }
  return(list(y = y, UnStandardize = UnStandardize))
}


StandardizeAllVariables <- function(data, fit.range = NULL) {
  # This function is copied from CausalImpact package
  if (!is.null(ncol(data))) {
    for (j in ncol(data) : 1) {
      tmp <- Standardize(data[, j], fit.range)
      data[, j] <- tmp$y
      UnStandardize <- tmp$UnStandardize
    }
  } else {
    tmp <- Standardize(data, fit.range)
    data <- tmp$y
    UnStandardize <- tmp$UnStandardize
  }
  return(list(data = data, UnStandardize = UnStandardize))
}


control_candidates <- function(test, control, match_period_start, match_period_end, n_candidates){
  distances <- data.frame(matrix(nrow=ncol(control)-1, ncol=4))
  names(distances) <- c('test', 'control', 'relative_distance', 'correlation')
  distances$test <- setdiff(colnames(test), "date")

  test <- dplyr::filter(test, date >= match_period_start & date <= match_period_end) %>% dplyr::select(-date) %>% unlist()
  control <- dplyr::filter(control, date >= match_period_start & date <= match_period_end) %>% dplyr::select(-date)
  for (i in 1:ncol(control)) {
    distances$control[i] <- colnames(control)[i]
    if (var(control[, i]) == 0)  next
    distances$relative_distance[i] <- dtw(test, control[, i], window.type=sakoeChibaWindow, window.size=1)$normalizedDistance
    distances$correlation[i] <- cor(test, control[, i])
  }

  # output selected control series
  dtw_all <- dplyr::arrange(distances, relative_distance) %>%
    head(n_candidates) %>%
    .$control
  dtw_hiwiki <- dplyr::filter(distances, grepl("_hi.wikipedia$", control)) %>%
    dplyr::arrange(relative_distance) %>%
    head(n_candidates) %>%
    .$control
  corr_all <- dplyr::arrange(distances, desc(abs(correlation))) %>%
    head(n_candidates) %>%
    .$control
  corr_hiwiki <- dplyr::filter(distances, grepl("_hi.wikipedia$", control)) %>%
    dplyr::arrange(desc(abs(correlation)))%>%
    head(n_candidates) %>%
    .$control

  return(list(dtw_all = dtw_all, dtw_hiwiki = dtw_hiwiki, corr_all = corr_all, corr_hiwiki = corr_hiwiki))
}


run_bsts_model <- function(x, y, train_start, train_end, validation_start, validation_end, selected_controls,
                           control_group, trend, autoAR, seasonality, holiday, dynamic_regression = FALSE,
                           prior_level_sd = 0.01, niter, ping = 0) {

  # prepare data
  if (as.character(control_group) == "none") {
    train_data <- data.frame(
      y = dplyr::filter(y, date >= train_start & date <= validation_end) %>% dplyr::select(-date) %>% unlist()
      ) %>%
      xts::xts(order.by = seq.Date(train_start, validation_end, "day"))
  } else {
    train_data <- data.frame(
      y = dplyr::filter(y, date >= train_start & date <= validation_end) %>% dplyr::select(-date) %>% unlist(),
      dplyr::filter(x, date >= train_start & date <= validation_end)[, match(unlist(selected_controls[as.character(control_group)]), colnames(x))]
    ) %>%
      xts::xts(order.by = seq.Date(train_start, validation_end, "day"))
  }

  # standardize all variables
  sd.results <- StandardizeAllVariables(train_data, c(1, train_end - train_start + 1))
  train_data <- sd.results$data
  UnStandardize <- sd.results$UnStandardize
  post.period.response <- train_data$y[seq.Date(validation_start, validation_end, "day")]
  train_data$y[seq.Date(validation_start, validation_end, "day")] <- NA

  # state specification
  ss <- list()
  ## trend
  sdy <- sd(train_data$y, na.rm = TRUE)
  sd.prior <- SdPrior(sigma.guess = prior_level_sd * sdy,
                upper.limit = sdy,
                sample.size = 32)
  ss <- switch(
    as.character(trend),
    local_level = {
      AddLocalLevel(ss, train_data$y, sigma.prior = sd.prior)
    },
    local_linear = {
      AddLocalLinearTrend(ss, train_data$y)
    },
    semi_local = {
      AddSemilocalLinearTrend(ss, train_data$y)
    },
    static = {
      AddStaticIntercept(ss, train_data$y)
    }
  )
  ## AR
  if (autoAR) {
    ss <- AddAutoAr(ss, train_data$y, lags = 1)
  }
  ## seasonality
  if (seasonality) {
    ss <- AddSeasonal(ss, train_data$y, nseasons = 7) # Weekly seasonality
    ss <- AddMonthlyAnnualCycle(ss, train_data$y) # Yearly seasonality
  }
  ## holiday
  if (holiday) {
    ss <- AddRegressionHoliday(ss, train_data$y,
                               holiday.list = list(Diwali, Raksha_Bandhan, Holi, Dussehra, Newyear))
  }

  # model
  cat("Fitting BSTS model...\n")
  if (as.character(control_group) == "none") {
    bsts.model <- bsts(train_data$y, state.specification = ss, family = 'gaussian',
                       niter = niter, seed = seed, ping = ping)
  } else {
    formula <- "y ~ ."

    if (!dynamic_regression) {
      bsts.model <- bsts(formula, state.specification = ss, family = 'gaussian', data = train_data,
                         niter = niter, seed = seed, ping = ping, expected.r2 = 0.8, prior.df = 50,
                         expected.model.size = min(ncol(train_data)*0.1, 5)) # Passed to SpikeSlabPrior, no need for a prior distribution if this presents
    } else { # dynamic regression
      sigma.mean.prior <- GammaPrior(prior.mean = 1, a = 4)
      ss <- AddDynamicRegression(ss, formula, data = train_data,
                                 sigma.mean.prior = sigma.mean.prior)
      sd.prior <- SdPrior(sigma.guess = prior_level_sd * sdy,
                          upper.limit = 0.1 * sdy,
                          sample.size = 32)
      bsts.model <- bsts(train_data$y, state.specification = ss, niter = niter,
                         expected.model.size = min(ncol(train_data)*0.1, 5), ping = ping, seed = seed,
                         prior = sd.prior)
    }
  }

  return(list(model = bsts.model, post.period.response = post.period.response, UnStandardize = UnStandardize))
}


bsts_cv_loop <- function(x, y, cv_start, cv_end, horizon, nfold, control_group, log_transformed = FALSE, trend,
                         autoAR, seasonality, holiday, dynamic_regression = FALSE, prior_level_sd = 0.01,
                         niter, n_control_candidates = 20, preselect_controls = NULL) {
  rmse_v <- c()
  mape_v <- c()
  rsquare_v <- c()
  AbsEffect <- c()
  AbsEffect_CI_width <- c()
  AbsEffect_sd <- c()
  for (fold in 1:nfold) {
    cat(paste("Round =", i, "fold =", fold, "start! \n"))

    train_start <- cv_start
    train_end <- cv_end - horizon*fold

    if (control_group == "none") {
      selected_controls <- NULL
    } else if (control_group == "hiwiki_all") {
      selected_controls <- list(hiwiki_all = grep("_hi.wikipedia$", colnames(x), value = TRUE))
    } else {
      cat("Finding matched series...\n")
      selected_controls <- control_candidates(
        test = y, control = x,
        match_period_start = train_start, match_period_end = train_end,
        n_candidates = n_control_candidates)
      selected_controls <- c(
        selected_controls,
        list(
          mixed = unique(c(head(selected_controls$dtw_all, 10), head(selected_controls$dtw_hiwiki, 10),
                  head(selected_controls$corr_all, 10), head(selected_controls$corr_hiwiki, 10), preselect_controls
                  )),
          best_all = unique(c(selected_controls$dtw_all, selected_controls$corr_all))
          )
        )
      selected_controls[1:4] <- NULL
    }

    # fit model
    this_model <- run_bsts_model(x, y, train_start, train_end, train_end + 1, train_end + horizon, selected_controls,
    control_group, trend, autoAR, seasonality, holiday, dynamic_regression, prior_level_sd, niter)
    post.period.response <- this_model$UnStandardize(as.numeric(this_model$post.period.response))
    if (log_transformed) {
      post.period.response <- exp(post.period.response)
    }
    this_impact <- CausalImpact(bsts.model = this_model$model,
                                post.period.response = post.period.response,
                                UnStandardize = this_model$UnStandardize, log_transformed = log_transformed)

    # evaluation
    errors <- tail(this_impact$series$response - this_impact$series$point.pred, horizon)
    rmse <- sqrt(mean(errors^2))
    rmse_v <- c(rmse_v, rmse)
    mape <- mean(abs(errors)/tail(this_impact$series$response, horizon))
    mape_v <- c(mape_v, mape)
    rsquare_v <- c(rsquare_v, summary(this_model$model)$rsquare)
    AbsEffect <- c(AbsEffect, this_impact$summary$AbsEffect[1])
    AbsEffect_CI_width <- c(AbsEffect_CI_width, this_impact$summary$AbsEffect.upper[1]-this_impact$summary$AbsEffect.lower[1])
    AbsEffect_sd <- c(AbsEffect_sd, this_impact$summary$AbsEffect.sd[1])
  }

  return(data.frame(rmse=rmse_v, mape=mape_v, rsquare=rsquare_v,
                    AbsEffect=AbsEffect, AbsEffect_CI_width=AbsEffect_CI_width, AbsEffect_sd=AbsEffect_sd))
}

