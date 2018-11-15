# devtools::install_github("chelsyx/CausalImpact", force = TRUE)
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
rm(gsc_hiwiki, gsc_hiwiki_country, gsc_m_hiwiki_country, hiwiki_main_pv, pageviews, unique_devices, mp_gtrend, india_gtrend)

# Choose models
model_spec <- expand.grid(
  controls = c("hiwiki_all", "mixed", "best_all", "none"),
  trend = c("local_level", "local_linear", "semi_local", "static"),
  prior_level_sd = c(0.01, 0.1),
  ar = c(TRUE, FALSE),
  seasonality = c(TRUE, FALSE),
  holiday = c(TRUE, FALSE),
  dynamic_regression = c(TRUE, FALSE),
  log = c(TRUE, FALSE),
  train_start = as.Date(c("2016-01-01", "2017-01-01", "2017-10-01"))
)
model_spec %<>% filter(!(model_spec$prior_level_sd != 0.01 & model_spec$trend != "local_level"))
model_spec %<>% filter(!(model_spec$dynamic_regression & model_spec$controls == "none"))

# Model selection (run on stat1007)

library(foreach)
library(doParallel)

cl <- makeCluster(25)
registerDoParallel(cl)

model_compare <- foreach(i=1:nrow(model_spec), .combine=rbind,
                         .packages = c("dplyr", "magrittr", "CausalImpact", "dtw"),
                         .export=ls(envir=globalenv())) %dopar% {
  param <- model_spec[i, ]
  cat(paste("Round =", i, "start! log =", param$log, "train_start =", param$train_start,
            "controls =", param$controls, "trend =", param$trend, "autoAR =", param$ar,
            "seasonality =", param$seasonality, "holiday =", param$holiday, "dynamic regression =",
            param$dynamic_regression, "prior_level_sd =", param$prior_level_sd, "\n"))

  train_start <- param$train_start
  if (param$log) {
    y <- test_series_log
    x <- control_series_log
  } else {
    y <- test_series
    x <- control_series
  }
  preselect_controls <- paste(c("Himachal.Pradesh", "Chandigarh", "Uttarakhand", "Haryana", "National.Capital.Territory.of.Delhi",
                             "Rajasthan", "Uttar.Pradesh", "Bihar", "Jharkhand", "Chhattisgarh"), "hi.wikipedia", sep = "_") # more than 50% population speak hindi

  cv_results <- bsts_cv_loop(x, y, cv_start = train_start, cv_end = tv_start - 1, horizon = 7, nfold = 10, log_transformed = param$log,
                             control_group = param$controls, trend = param$trend, autoAR = param$ar, prior_level_sd = param$prior_level_sd,
                             seasonality = param$seasonality, holiday = param$holiday, dynamic_regression = param$dynamic_regression,
                             niter = 1000, n_control_candidates = 30, preselect_controls = preselect_controls)

  output <- c(
    i, mean(cv_results$rmse), sd(cv_results$rmse), mean(cv_results$mape), sd(cv_results$mape),
    mean(cv_results$rsquare), mean(cv_results$AbsEffect), mean(cv_results$AbsEffect_CI_width), mean(cv_results$AbsEffect_sd)
    )
  output
}

stopCluster(cl)
colnames(model_compare) <- c('ID', 'RMSE', 'RMSE_sd', 'MAPE', 'MAPE_sd', 'Rsquare', 'AbsEffect', 'AbsEffect_CI_width', 'AbsEffect_sd')
model_compare <- cbind(model_compare, model_spec)
save(model_compare, file = "data/hiwiki_pv_model_compare.RData")

# Check model compare results
load("data/hiwiki_pv_model_compare.RData")
model_rank <- model_compare %>%
  select(RMSE, RMSE_sd, MAPE, MAPE_sd, AbsEffect, AbsEffect_sd) %>%
  mutate_all(rank) %>%
  rowMeans() %>%
  cbind(ID = model_compare$ID, model_spec)
# best model is 579

# check the imaginary intervention 60 days before the online campaign start on model 579
n_iters <- 5e3
train_start <- as.Date("2017-01-01")
train_end <- online_start - 60 - 1
validation_start <- online_start - 60
validation_end <- online_start - 1
y <- test_series
x <- control_series

selected_controls <- control_candidates(test = y, control = x,
                                        match_period_start = train_start, match_period_end = train_end,
                                        n_candidates = 30)
selected_controls <- c(
  selected_controls,
  list(best_all = unique(c(selected_controls$dtw_all, selected_controls$corr_all)))
  )
selected_controls[1:4] <- NULL

model_579 <- run_bsts_model(x, y, train_start, train_end, validation_start, validation_end, selected_controls,
  control_group = "best_all", trend = "static", autoAR = TRUE, seasonality = TRUE,
  holiday = TRUE, dynamic_regression = FALSE, prior_level_sd = 0.01, niter = n_iters)
model_579_impact <- CausalImpact(bsts.model = model_579$model,
                                 post.period.response = model_579$UnStandardize(as.numeric(model_579$post.period.response)),
                                 UnStandardize = model_579$UnStandardize)
plot(model_579_impact)
# the estimates and actual data agree reasonabaly closely


# Use model 579 to compute causal impact (using hacked CausalImpact package)
train_start <- as.Date("2017-01-01")
train_end <- online_start - 1
test_start <- online_start
test_end <- online_start + 59
n_iters <- 1e4
y <- test_series
x <- control_series

selected_controls <- control_candidates(test = y, control = x,
                                        match_period_start = train_start, match_period_end = train_end,
                                        n_candidates = 30)
selected_controls <- c(
  selected_controls,
  list(best_all = unique(c(selected_controls$dtw_all, selected_controls$corr_all)))
  )
selected_controls[1:4] <- NULL

model_579 <- run_bsts_model(x, y, train_start, train_end, test_start, test_end, selected_controls,
  control_group = "best_all", trend = "static", autoAR = TRUE, seasonality = TRUE,
  holiday = TRUE, dynamic_regression = FALSE, prior_level_sd = 0.01, niter = n_iters)
model_579_impact <- CausalImpact(bsts.model = model_579$model,
                                 post.period.response = model_579$UnStandardize(as.numeric(model_579$post.period.response)),
                                 UnStandardize = model_579$UnStandardize)
# not significant
save(model_579_impact, file = "data/hiwiki_pv_online_impact.RData")
p <- plot(model_579_impact)
p <- p + labs(title = "Impact of the online campaign in 60 days on Hindi Wikipedia pageviews from Madhya Pradesh",
              subtitle = "Dashed line represents the start date of the online campaign on YouTube and Facebook, April 3rd")
ggsave("hiwiki_pv_online_impact.png", plot = p, path = 'docs/figures', units = "in", dpi = 300, height = 9, width = 18)
