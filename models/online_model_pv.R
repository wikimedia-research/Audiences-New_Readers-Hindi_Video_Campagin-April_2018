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
control_series <- pageviews %>%
  filter(country_code == 'IN', subdivision != 'Madhya Pradesh') %>%
  group_by(date, subdivision, project) %>%
  summarize(pageviews = sum(pageviews)) %>%
  ungroup() %>%
  complete(date, nesting(subdivision, project), fill = list(pageviews = 0)) %>%
  unite(metrics, subdivision, project) %>%
  mutate(metrics = gsub(" ", ".", metrics, fixed = TRUE)) %>%
  spread(metrics, pageviews)
control_series <- left_join(control_series, internet_users_mp, by = "date")
rm(gsc_hiwiki, gsc_hiwiki_country, gsc_m_hiwiki_country, hiwiki_main_pv, pageviews, unique_devices, mp_gtrend, india_gtrend)

holiday.list <- list(Diwali, Raksha_Bandhan, Holi, Dussehra, Newyear)
preselect_controls <- c(
  paste(c("Himachal.Pradesh", "Chandigarh", "Uttarakhand", "Haryana", "National.Capital.Territory.of.Delhi",
    "Rajasthan", "Uttar.Pradesh", "Bihar", "Jharkhand", "Chhattisgarh"), "hi.wikipedia", sep = "_"), # more than 50% population speak hindi
"internet_users"
)

# Choose models
model_spec <- expand.grid(
  trend = c("local_level", "local_linear", "semi_local", "static"),
  train_length = c(84, 126, 183, 400),
  ar = c(TRUE, FALSE)
)
n_control_candidates <- 20

# Model selection

library(foreach)
library(doParallel)

cl <- makeCluster(10)
registerDoParallel(cl)

model_compare <- foreach(i=1:nrow(model_spec), .combine=rbind,
                         .packages = c("dplyr", "magrittr", "CausalImpact", "dtw"),
                         .export=ls(envir=globalenv())) %dopar% {
  param <- model_spec[i, ]
  y <- test_series
  x <- control_series

  cv_results <- bsts_cv_loop(x, y, train_length = param$train_length, cv_end = online_start - 1, horizon = 42, nfold = 10, step = 14,
                             log_transformed = FALSE, trend = param$trend, autoAR = param$ar, prior_level_sd = 0.01, weekly_seasonality = TRUE,
                             yearly_seasonality = TRUE, holiday.list = holiday.list, dynamic_regression = FALSE,
                             niter = 1000, n_control_candidates = n_control_candidates, preselect_controls = preselect_controls)

  output <- c(
    i, mean(cv_results$rmse, na.rm = TRUE), sd(cv_results$rmse, na.rm = TRUE), mean(cv_results$mape, na.rm = TRUE), sd(cv_results$mape, na.rm = TRUE), mean(cv_results$rsquare, na.rm = TRUE),
    mean(abs(cv_results$AbsEffect), na.rm = TRUE), mean(cv_results$AbsEffect_CI_width, na.rm = TRUE), mean(cv_results$AbsEffect_sd, na.rm = TRUE), mean(cv_results$contain_zero, na.rm = TRUE)
    )
  output
}

stopCluster(cl)
colnames(model_compare) <- c('ID', 'RMSE', 'RMSE_sd', 'MAPE', 'MAPE_sd', 'Rsquare', 'AbsEffect', 'AbsEffect_CI_width', 'AbsEffect_sd', 'contain_zero')
model_compare <- cbind(model_compare, model_spec)
save(model_compare, file = "data/hiwiki_pv_model_compare.RData")

# Check model compare results
load("data/hiwiki_pv_model_compare.RData")
model_rank <- model_compare %>%
  dplyr::select(RMSE, RMSE_sd, MAPE, MAPE_sd, AbsEffect, AbsEffect_sd) %>%
  dplyr::mutate_all(rank) %>%
  rowMeans() %>%
  cbind(ID = model_compare$ID, model_compare$contain_zero, model_spec)
# best model is 29


# Visual Validation
n_iters <- 5e3
train_length <- 400
trend <- "local_level"
autoAR <- FALSE
validation_end <- online_start - 1
validation_start <- validation_end - 6*7 + 1
train_end <- validation_start - 1
train_start <- train_end - train_length + 1
y <- test_series
x <- control_series

selected_controls <- control_candidates(
  test = y, control = x,
  match_period_start = train_start, match_period_end = train_end,
  n_candidates = 30)
selected_controls <- unique(c(head(selected_controls$dtw_all, n_control_candidates), head(selected_controls$dtw_hiwiki, n_control_candidates),
  head(selected_controls$corr_all, n_control_candidates), head(selected_controls$corr_hiwiki, n_control_candidates), preselect_controls))
model_29 <- run_bsts_model(x, y, train_start, train_end, validation_start, validation_end,
  selected_controls, trend = trend, autoAR = autoAR, weekly_seasonality=TRUE, yearly_seasonality=TRUE,
  holiday.list = holiday.list, dynamic_regression = FALSE, prior_level_sd = 0.01, niter = n_iters)
model_29_impact <- CausalImpact(bsts.model = model_29$model,
                                post.period.response = model_29$UnStandardize(as.numeric(model_29$post.period.response)),
                                UnStandardize = model_29$UnStandardize)
plot(model_29$model, "comp")
plot(model_29_impact)

# Validation results for final model
cv_results <- bsts_cv_loop(x, y, train_length = train_length, cv_end = online_start - 1, horizon = 42, nfold = 10, step = 14,
                           log_transformed = FALSE, trend = trend, autoAR = autoAR, prior_level_sd = 0.01, weekly_seasonality = TRUE,
                           yearly_seasonality = TRUE, holiday.list = holiday.list, dynamic_regression = FALSE,
                           niter = n_iters, n_control_candidates = n_control_candidates, preselect_controls = preselect_controls)
output <- c(
  mean(cv_results$rmse, na.rm = TRUE), sd(cv_results$rmse, na.rm = TRUE), mean(cv_results$mape, na.rm = TRUE), sd(cv_results$mape, na.rm = TRUE), mean(cv_results$rsquare, na.rm = TRUE),
  mean(abs(cv_results$AbsEffect), na.rm = TRUE), mean(cv_results$AbsEffect_CI_width, na.rm = TRUE), mean(cv_results$AbsEffect_sd, na.rm = TRUE), mean(cv_results$contain_zero, na.rm = TRUE)
  )
names(output) <- c('RMSE', 'RMSE_sd', 'MAPE', 'MAPE_sd', 'Rsquare', 'AbsEffect', 'AbsEffect_CI_width', 'AbsEffect_sd', 'contain_zero')
      #         RMSE            RMSE_sd               MAPE            MAPE_sd
      # 1.838333e+04       9.079727e+03       8.791714e-02       4.579580e-02
      #      Rsquare          AbsEffect AbsEffect_CI_width       AbsEffect_sd
      # 9.505424e-01       1.175468e+04       2.645148e+04       6.900160e+03
      # contain_zero
      # 5.666667e-01


# Model Impact
test_end <- online_start + 6*7 - 1
test_start <- online_start
train_end <- online_start - 1
train_start <- train_end - train_length + 1
n_iters <- 1e4
y <- test_series
x <- control_series
selected_controls <- control_candidates(
  test = y, control = x,
  match_period_start = train_start, match_period_end = train_end,
  n_candidates = 30)
selected_controls <- unique(c(head(selected_controls$dtw_all, n_control_candidates), head(selected_controls$dtw_hiwiki, n_control_candidates),
  head(selected_controls$corr_all, n_control_candidates), head(selected_controls$corr_hiwiki, n_control_candidates), preselect_controls))
model_29 <- run_bsts_model(x, y, train_start, train_end, test_start, test_end,
  selected_controls, trend = trend, autoAR = autoAR, weekly_seasonality=TRUE, yearly_seasonality=TRUE,
  holiday.list = holiday.list, dynamic_regression = FALSE, prior_level_sd = 0.01, niter = n_iters)
model_29_impact <- CausalImpact(bsts.model = model_29$model,
                                post.period.response = model_29$UnStandardize(as.numeric(model_29$post.period.response)),
                                UnStandardize = model_29$UnStandardize)
p <- plot(model_29_impact) +
     labs(title = "Impact of the online campaign in 6 weeks on Hindi Wikipedia page views from Madhya Pradesh",
     subtitle = "Vertical dashed line represents the start date of the campaign 3 April 2018")
ggsave("hiwiki_pv_online_impact.png", plot = p, path = 'docs/figures', units = "in", dpi = 300, height = 6, width = 14)
summary(model_29_impact)
summary(model_29_impact, "report")
png('docs/figures/hiwiki_pv_online_inclusion_prob.png', units = "in", res = 300, height = 8, width = 10)
plot(model_29$model, "coef")
dev.off()

# inclusion prob
burn <- SuggestBurn(0.1, model_29)
sort(colMeans(model_29$model$coefficients[-(1:burn),] != 0))
                     # Chhattisgarh_hi.wikipedia
                     #                  0.95109511
                     #      Rajasthan_hi.wikipedia
                     #                  1.00000000

# average coefficient
apply(model_29$model$coefficients[-(1:burn),], 2, PositiveMean)
# Rajasthan_hi.wikipedia
#          0.24528754
# Chhattisgarh_hi.wikipedia
#              0.13431732
