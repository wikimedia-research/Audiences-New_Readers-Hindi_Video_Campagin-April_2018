# devtools::install_github("google/CausalImpact", force = TRUE)
# install.packages("dtw")

library(CausalImpact)
library(dtw)

source("refine.R")
source("functions.R")

# hiwiki pv from Madhya Pradesh

cat("Data preparation...\n")
test_series <- pageviews %>%
  filter(country_code == 'IN', subdivision == 'Madhya Pradesh', project == 'hi.wikipedia') %>%
  group_by(date) %>%
  summarize(Madhya.Pradesh_hi.wikipedia = sum(pageviews)) %>%
  ungroup() %>%
  complete(date, fill = list(Madhya.Pradesh_hi.wikipedia = 0))
test_series_log <- mutate(test_series, Madhya.Pradesh_hi.wikipedia = log(Madhya.Pradesh_hi.wikipedia)) %>%
  mutate(Madhya.Pradesh_hi.wikipedia = ifelse(Madhya.Pradesh_hi.wikipedia == -Inf, 0, Madhya.Pradesh_hi.wikipedia))
control_series <- pageviews %>%
  filter(country_code == 'IN', subdivision != 'Madhya Pradesh') %>%
  group_by(date, subdivision, project) %>%
  summarize(pageviews = sum(pageviews)) %>%
  bind_rows(
    {pageviews %>%
       filter(country_code == 'IN', subdivision != 'Madhya Pradesh') %>%
       group_by(date, project) %>%
       summarize(pageviews = sum(pageviews)) %>%
       mutate(subdivision = 'other states')},
    {pageviews %>%
       filter(country_code == 'IN', subdivision != 'Madhya Pradesh', !(project %in% c('en.wikipedia', 'hi.wikipedia'))) %>%
       group_by(date, subdivision) %>%
       summarize(pageviews = sum(pageviews)) %>%
       mutate(project = 'other wikis')},
    {pageviews %>%
       filter(country_code == 'IN', subdivision != 'Madhya Pradesh', !(project %in% c('en.wikipedia', 'hi.wikipedia'))) %>%
       group_by(date) %>%
       summarize(pageviews = sum(pageviews)) %>%
       mutate(subdivision = 'other states', project = 'other wikis')}
  ) %>%
  ungroup() %>%
  complete(date, nesting(subdivision, project), fill = list(pageviews = 0)) %>%
  unite(metrics, subdivision, project) %>%
  mutate(metrics = gsub(" ", ".", metrics, fixed = TRUE)) %>%
  spread(metrics, pageviews)
control_series_log <- data.frame(date = control_series$date, {
   mutate_all(control_series[, -1], log) %>%
     apply(2, function(x) replace(x, is.infinite(x), 0))
})

train_start <- min(test_series$date)
train_end <- online_start - 60 - 1
validation_start <- online_start - 60
validation_end <- online_start - 1
model_spec_online <- expand.grid(
  controls = c("hiwiki_all", "mixed", "best_all", "none"),
  trend = c("local_level_0.01", "local_level_0.1", "local_linear", "semi_local", "static"),
  ar = c(TRUE, FALSE),
  seasonality = c(TRUE, FALSE),
  holiday = c(TRUE, FALSE),
  log = c(TRUE, FALSE),
  train_start = as.Date(c("2016-01-01", "2017-01-01", "2017-10-01"))
)

n_iters <- 2e3
burn_in <- 2e2
prev_param_log <- NULL
prev_param_start <- NULL
# model_list <- list()
# post.period.response.matrix <- NULL
model_compare <- data.frame(matrix(nrow=nrow(model_spec_online), ncol=3))
names(model_compare) <- c('ID', 'MAPE', 'rsquare')
for (i in 216:nrow(model_spec_online)){
  param <- model_spec_online[i, ]
  cat(paste("Round =", i, "start! log =", param$log, "train_start =", param$train_start,
            "controls =", param$controls, "trend =", param$trend, "autoAR =", param$ar,
            "seasonality =", param$seasonality, "holiday =", param$holiday,
            "\n"))

  train_start <- param$train_start

  if (param$log) {
    y <- test_series_log
    x <- control_series_log
  } else {
    y <- test_series
    x <- control_series
  }

  if (any(param$log != prev_param_log, param$train_start != prev_param_start, i == 1)) {
    cat("Finding matched series...\n")
    selected_controls <- control_candidates(test = y, control = x,
                                            match_period_start = train_start, match_period_end = train_end,
                                            n_candidates = 30)
    selected_controls <- c(
      selected_controls,
      list(
        hiwiki_all = grep("_hi.wikipedia$", colnames(x), value = TRUE),
        mixed = unique(c(head(selected_controls$dtw_all, 10), head(selected_controls$dtw_hiwiki, 10),
                head(selected_controls$corr_all, 10), head(selected_controls$corr_hiwiki, 10),
                paste(c("Himachal.Pradesh", "Chandigarh", "Uttarakhand", "Haryana", "National.Capital.Territory.of.Delhi",
                             "Rajasthan", "Uttar.Pradesh", "Bihar", "Jharkhand", "Chhattisgarh"), "hi.wikipedia", sep = "_") # more than 50% population speak hindi
                )),
        best_all = unique(c(selected_controls$dtw_all, selected_controls$corr_all))
        )
      )
    selected_controls[1:4] <- NULL
  }

  # prepare data
  if (as.character(param$controls) == "none") {
    train_data <- data.frame(
      y = filter(y, date >= train_start & date <= validation_end) %>% select(-date) %>% unlist()
      ) %>%
      xts::xts(order.by = seq.Date(train_start, validation_end, "day"))
  } else {
    train_data <- data.frame(
      y = filter(y, date >= train_start & date <= validation_end) %>% select(-date) %>% unlist(),
      filter(x, date >= train_start & date <= validation_end)[, match(unlist(selected_controls[param$controls]), colnames(x))]
    ) %>%
      xts::xts(order.by = seq.Date(train_start, validation_end, "day"))
  }
  # Standardize all variables
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
    as.character(param$trend),
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
  if (param$ar) {
    ss <- AddAutoAr(ss, train_data$y, lags = 1)
  }
  ## seasonality
  if (param$seasonality) {
    ss <- AddSeasonal(ss, train_data$y, nseasons = 7) # Weekly seasonality
    ss <- AddMonthlyAnnualCycle(ss, train_data$y) # Yearly seasonality
  }
  ## holiday
  if (param$holiday) {
    ss <- AddRegressionHoliday(ss, train_data$y,
                               holiday.list = list(Diwali, Raksha_Bandhan, Holi_validation, Dussehra, Newyear))
  }

  # model
  cat("Fitting BSTS model...\n")
  if (as.character(param$controls) != "none") {
     formula <- "y ~ ."
  } else {
    formula <- "train_data$y"
  }
  bsts_func <- paste0("bsts(",
                      formula,
                      ", state.specification = ss,
                      family = 'gaussian',
                      data = train_data,
                      niter = burn_in + n_iters,
                      seed = seed,
                      expected.model.size = min(ncol(train_data)*0.1, 5), # Passed to SpikeSlabPrior, no need for a prior distribution if this presents
                      expected.r2 = 0.8,
                      prior.df = 50)")
  this_model <- tryCatch(
    eval(parse(text=bsts_func)),
    error = function(e) {
      cat("Error in fitting BSTS...\n")
      model_compare$ID[i] <- i
      # model_list <- c(model_list, list("error"))
      # post.period.response.matrix <- rbind(post.period.response.matrix, rep(NA, validation_end - validation_start + 1))
      prev_param_log <- param$log
      prev_param_start <- param$train_start
      next
    }
  )
  this_impact <- CausalImpact(bsts.model = this_model,
                              post.period.response = as.numeric(post.period.response))
  this_impact$series <- apply(this_impact$series, 2, UnStandardize)
  if (param$log) {
    this_impact$series <- apply(this_impact$series, 2, exp)
  }

  model_compare$ID[i] <- i
  model_compare$MAPE[i] <- this_impact$series[as.character(seq.Date(validation_start, validation_end, by = "day")), ] %>%
    as.tibble %>%
    summarise(MAPE=mean(abs(response-point.pred)/response))
  model_compare$rsquare[i] <- summary(this_model, burn = burn_in)$rsquare

  # model_list <- c(model_list, this_model)
  # post.period.response.matrix <- rbind(post.period.response.matrix, as.numeric(post.period.response))
  prev_param_log <- param$log
  prev_param_start <- param$train_start
}
model_compare <- cbind(model_compare, model_spec_online)
model_compare$MAPE <- unlist(model_compare$MAPE)
save(model_compare, file = "data/hiwiki_pv_model_compare.RData")








## model evaluation
pred1 <- predict.bsts(model1,
                      newdata = filter(x, date >= validation_start & date <= validation_end)[, match(selected_controls[[7]], colnames(x))],
                      timestamps = seq.Date(validation_start, validation_end, "day"),
                      horizon = 60, burn = burn_in, quantiles = c(.025, .975), seed = seed)

### Actual versus predicted
d2 <- data.frame(
    # fitted values and predictions
    c(exp(as.numeric(-colMeans(model1$one.step.prediction.errors[-(1:burn_in),])+train_data$y)),
    exp(as.numeric(pred1$mean))),
    # actual data and dates
    unlist(exp(filter(y, date <= validation_end) %>% select(-date))),
    filter(y, date <= validation_end)$date)
names(d2) <- c("Fitted", "Actual", "Date")

### MAPE (mean absolute percentage error)
# can also compute it with output from CausalImpact
MAPE <- filter(d2, Date >= validation_start) %>% summarise(MAPE=mean(abs(Actual-Fitted)/Actual))
rsquare <- summary(model1, burn = burn_in)$rsquare

### 95% forecast credible interval
posterior.interval <- cbind.data.frame(
  exp(pred1$interval[1,]),
  exp(pred1$interval[2,]),
  subset(d2, Date >= validation_start)$Date)
names(posterior.interval) <- c("LL", "UL", "Date")

### Join intervals to the forecast
d3 <- left_join(d2, posterior.interval, by="Date")

### Plot actual versus predicted with credible intervals for the holdout period
ggplot(data=d3, aes(x=Date)) +
  geom_line(aes(y=Actual, colour = "Actual"), size=1.2) +
  geom_line(aes(y=Fitted, colour = "Fitted"), size=1.2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("") + xlab("") +
  geom_vline(xintercept=as.numeric(as.Date("1959-12-01")), linetype=2) +
  geom_ribbon(aes(ymin=LL, ymax=UL), fill="grey", alpha=0.5) +
  ggtitle(paste0("BSTS -- Holdout MAPE = ", round(100*MAPE,2), "%")) +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))





# Plot the selected control series to make sure they didn't affected by the intervention
x[, c(1, match(dtw_controls, colnames(x)))] %>%
  filter(date >= as.Date("2018-01-01")) %>%
  gather(source, pageviews, -date) %>%
  ggplot(aes(x=date, y=pageviews, color=source)) +
  geom_line() +
  geom_vline(xintercept = online_start) +
  geom_vline(xintercept = tv_start, linetype = "dashed")











# names(model1)
burn <- SuggestBurn(0.1, model1) # Get a suggested number of burn-ins, to be used in predict.bsts

plot(model1)
plot(model1, "components")
PlotBstsCoefficients(model1)
PlotBstsResiduals(model1)
PlotBstsSize(model1)
bsts.prediction.errors(model1) # one-step-ahead prediction errors
PlotBstsPredictionErrors(model1)

### Set up the priors
prior <- SpikeSlabPrior(x=model.matrix(iclaimsNSA ~ ., data=initial.claims),
                        y=initial.claims$iclaimsNSA,
                        prior.information.weight = 200,
                        prior.inclusion.probabilities = prior.spikes,
                        optional.coefficient.estimate = prior.mean)
bsts.reg.priors <- bsts(iclaimsNSA ~ ., state.specification = ss,
                        data = initial.claims,
                        niter = 500,
                        prior=prior,
                        ping=0, seed=seed)

# test causal impact on validation period
impact <- CausalImpact(train_data, c(train_start, train_end), c(validation_start, validation_end),
                       model.args = list(niter = 10000, nseasons = 7, season.duration = 1))
plot(impact)

