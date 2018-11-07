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

  test <- filter(test, date >= match_period_start & date <= match_period_end) %>% select(-date) %>% unlist()
  control <- filter(control, date >= match_period_start & date <= match_period_end) %>% select(-date)
  for (i in 1:ncol(control)) {
    distances$control[i] <- colnames(control)[i]
    if (var(control[, i]) == 0)  next
    distances$relative_distance[i] <- dtw(test, control[, i], window.type=sakoeChibaWindow, window.size=1)$normalizedDistance
    distances$correlation[i] <- cor(test, control[, i])
  }

  # output selected control series
  dtw_all <- arrange(distances, relative_distance) %>%
    head(n_candidates) %>%
    .$control
  dtw_hiwiki <- filter(distances, grepl("_hi.wikipedia$", control)) %>%
    arrange(relative_distance) %>%
    head(n_candidates) %>%
    .$control
  corr_all <- arrange(distances, desc(abs(correlation))) %>%
    head(n_candidates) %>%
    .$control
  corr_hiwiki <- filter(distances, grepl("_hi.wikipedia$", control)) %>%
    arrange(desc(abs(correlation)))%>%
    head(n_candidates) %>%
    .$control

  return(list(dtw_all = dtw_all, dtw_hiwiki = dtw_hiwiki, corr_all = corr_all, corr_hiwiki = corr_hiwiki))
}

run_bsts_model <- function(x, y, train_start, train_end, validation_start, validation_end,
                           selected_controls, control_condidates, trend, autoAR, seasonality, holiday, niter) {

  # prepare data
  if (as.character(control_condidates) == "none") {
    train_data <- data.frame(
      y = filter(y, date >= train_start & date <= validation_end) %>% select(-date) %>% unlist()
      ) %>%
      xts::xts(order.by = seq.Date(train_start, validation_end, "day"))
  } else {
    train_data <- data.frame(
      y = filter(y, date >= train_start & date <= validation_end) %>% select(-date) %>% unlist(),
      filter(x, date >= train_start & date <= validation_end)[, match(unlist(selected_controls[control_condidates]), colnames(x))]
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
  ss <- switch(
    as.character(trend),
    local_level_0.01 = {
      prior.level.sd <- 0.01
      sd.prior <- SdPrior(sigma.guess = prior.level.sd * sdy, # sigma.guess: standard deviation of the random walk of the level
                    upper.limit = sdy,
                    sample.size = 32)
      AddLocalLevel(ss, train_data$y, sigma.prior = sd.prior)
    },
    local_level_0.1 = {
      prior.level.sd <- 0.1
      sd.prior <- SdPrior(sigma.guess = prior.level.sd * sdy, # sigma.guess: standard deviation of the random walk of the level
                    upper.limit = sdy,
                    sample.size = 32)
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
                               holiday.list = list(Diwali, Raksha_Bandhan, Holi_validation, Dussehra, Newyear))
  }

  # model
  cat("Fitting BSTS model...\n")
  if (as.character(control_condidates) != "none") {
     formula <- "y ~ ."
  } else {
    formula <- "train_data$y"
  }
  bsts_func <- paste0("bsts(",
                      formula,
                      ", state.specification = ss,
                      family = 'gaussian',
                      data = train_data,
                      niter = niter,
                      seed = seed,
                      expected.model.size = min(ncol(train_data)*0.1, 5), # Passed to SpikeSlabPrior, no need for a prior distribution if this presents
                      expected.r2 = 0.8,
                      prior.df = 50)")
  cat(bsts_func)
  bsts.model <- eval(parse(text=bsts_func))

  return(list(model = bsts.model, post.period.response = post.period.response, UnStandardize = UnStandardize))
}
