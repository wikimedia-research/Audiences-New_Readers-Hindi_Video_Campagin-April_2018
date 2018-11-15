# devtools::install_github("chelsyx/CausalImpact", force = TRUE)
# install.packages("dtw")

library(CausalImpact)
library(dtw)

source("refine.R")
source("functions.R")

# hiwiki ud from India

cat("Data preparation...\n")
test_series <- unique_devices %>%
  filter(country_code == 'IN', project == 'hi.wikipedia', type == 'total') %>%
  group_by(date) %>%
  summarize(India_hi.wikipedia = sum(uniques)) %>%
  ungroup() %>%
  complete(date, fill = list(India_hi.wikipedia = 0))
test_series_log <- mutate(test_series, India_hi.wikipedia = log(India_hi.wikipedia)) %>%
  mutate(India_hi.wikipedia = ifelse(India_hi.wikipedia == -Inf, 0, India_hi.wikipedia))
control_series <- unique_devices %>%
  filter(country_code != 'IN', type == 'total') %>%
  group_by(date, country, project) %>%
  summarize(uniques = sum(uniques)) %>%
  bind_rows(
    {unique_devices %>%
       filter(country_code != 'IN', type == 'total') %>%
       group_by(date, project) %>%
       summarize(uniques = sum(uniques)) %>%
       mutate(country = 'other countries')},
    {unique_devices %>%
       filter(country_code != 'IN', !(project %in% c('en.wikipedia', 'hi.wikipedia')), type == 'total') %>%
       group_by(date, country) %>%
       summarize(uniques = sum(uniques)) %>%
       mutate(project = 'other wikis')},
    {unique_devices %>%
       filter(country_code != 'IN', !(project %in% c('en.wikipedia', 'hi.wikipedia')), type == 'total') %>%
       group_by(date) %>%
       summarize(uniques = sum(uniques)) %>%
       mutate(country = 'other countries', project = 'other wikis')}
  ) %>%
  ungroup() %>%
  complete(date, nesting(country, project), fill = list(uniques = 0)) %>%
  unite(metrics, country, project) %>%
  mutate(metrics = gsub(" ", ".", metrics, fixed = TRUE)) %>%
  spread(metrics, uniques)
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

  cv_results <- bsts_cv_loop(x, y, cv_start = train_start, cv_end = tv_start - 1, horizon = 7, nfold = 10, log_transformed = param$log,
                             control_group = param$controls, trend = param$trend, autoAR = param$ar, prior_level_sd = param$prior_level_sd,
                             seasonality = param$seasonality, holiday = param$holiday, dynamic_regression = param$dynamic_regression,
                             niter = 1000, n_control_candidates = 20, preselect_controls = NULL)

  output <- c(
    i, mean(cv_results$rmse), sd(cv_results$rmse), mean(cv_results$mape), sd(cv_results$mape),
    mean(cv_results$rsquare), mean(cv_results$AbsEffect), mean(cv_results$AbsEffect_CI_width), mean(cv_results$AbsEffect_sd)
    )
  output
}

stopCluster(cl)
colnames(model_compare) <- c('ID', 'RMSE', 'RMSE_sd', 'MAPE', 'MAPE_sd', 'Rsquare', 'AbsEffect', 'AbsEffect_CI_width', 'AbsEffect_sd')
model_compare <- cbind(model_compare, model_spec)
save(model_compare, file = "data/hiwiki_ud_tvmodel_compare.RData")

# Check model compare results
load("data/hiwiki_ud_tvmodel_compare.RData")
model_rank <- model_compare %>%
  select(RMSE, RMSE_sd, MAPE, MAPE_sd, AbsEffect, AbsEffect_sd) %>%
  mutate_all(rank) %>%
  rowMeans() %>%
  cbind(ID = model_compare$ID, model_spec)
# best model is 802

# check the imaginary intervention 60 days before the TV campaign start on model 802
n_iters <- 5e3
train_start <- as.Date("2017-10-01")
train_end <- tv_start - 60 - 1
validation_start <- tv_start - 60
validation_end <- tv_start - 1
y <- test_series
x <- control_series
selected_controls <- control_candidates(test = y, control = x,
                                        match_period_start = train_start, match_period_end = train_end,
                                        n_candidates = 20)
selected_controls <- c(
  selected_controls,
  list(mixed = unique(c(head(selected_controls$dtw_all, 10), head(selected_controls$dtw_hiwiki, 10),
            head(selected_controls$corr_all, 10), head(selected_controls$corr_hiwiki, 10))))
  )
selected_controls[1:4] <- NULL
model_802 <- run_bsts_model(x, y, train_start, train_end, validation_start, validation_end, selected_controls,
  control_group = "mixed", trend = "local_level", autoAR = TRUE, seasonality = TRUE,
  holiday = TRUE, dynamic_regression = FALSE, prior_level_sd = 0.01, niter = n_iters)
model_802_impact <- CausalImpact(bsts.model = model_802$model,
                                 post.period.response = model_802$UnStandardize(as.numeric(model_802$post.period.response)),
                                 UnStandardize = model_802$UnStandardize)
plot(model_802_impact)
# the estimates and actual data agree reasonabaly closely


# Use model 802 to compute causal impact (using hacked CausalImpact package)
train_start <- as.Date("2017-10-01")
train_end <- tv_start - 1
test_start <- tv_start
test_end <- tv_start + 59
n_iters <- 1e4
y <- test_series
x <- control_series
selected_controls <- control_candidates(test = y, control = x,
                                        match_period_start = train_start, match_period_end = train_end,
                                        n_candidates = 20)
selected_controls <- c(
  selected_controls,
  list(mixed = unique(c(head(selected_controls$dtw_all, 10), head(selected_controls$dtw_hiwiki, 10),
                head(selected_controls$corr_all, 10), head(selected_controls$corr_hiwiki, 10))))
  )
selected_controls[1:4] <- NULL
model_802 <- run_bsts_model(x, y, train_start, train_end, test_start, test_end, selected_controls,
  control_group = "mixed", trend = "local_level", autoAR = TRUE, seasonality = TRUE,
  holiday = TRUE, dynamic_regression = FALSE, prior_level_sd = 0.01, niter = n_iters)
model_802_impact <- CausalImpact(bsts.model = model_802$model,
                                 post.period.response = model_802$UnStandardize(as.numeric(model_802$post.period.response)),
                                 UnStandardize = model_802$UnStandardize)
# not significant
save(model_802_impact, file = "data/hiwiki_ud_tv_impact.RData")
p <- plot(model_802_impact)
p <- p + labs(title = "Impact of the TV campaign in 60 days on Hindi Wikipedia unique devices from India",
              subtitle = "Vertical dashed line represents the date of TV campaign, May 27th")
ggsave("hiwiki_ud_tv_impact.png", plot = p, path = 'docs/figures', units = "in", dpi = 300, height = 9, width = 18)
