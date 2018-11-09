# devtools::install_github("google/CausalImpact", force = TRUE)
# install.packages("dtw")

library(CausalImpact)
library(dtw)

source("refine.R")
source("functions.R")

# hiwiki external pv from India

cat("Data preparation...\n")
test_series <- pageviews %>%
  filter(country_code == 'IN', project == 'hi.wikipedia', referer_class == "external") %>%
  group_by(date) %>%
  summarize(India_hi.wikipedia = sum(pageviews)) %>%
  ungroup() %>%
  complete(date, fill = list(India_hi.wikipedia = 0))
test_series_log <- mutate(test_series, India_hi.wikipedia = log(India_hi.wikipedia)) %>%
  mutate(India_hi.wikipedia = ifelse(India_hi.wikipedia == -Inf, 0, India_hi.wikipedia))
control_series <- pageviews %>%
  filter(country_code != 'IN', referer_class == "external") %>%
  group_by(date, country, project) %>%
  summarize(pageviews = sum(pageviews)) %>%
  bind_rows(
    {pageviews %>%
       filter(country_code != 'IN', referer_class == "external") %>%
       group_by(date, project) %>%
       summarize(pageviews = sum(pageviews)) %>%
       mutate(country = 'other countries')},
    {pageviews %>%
       filter(country_code != 'IN', !(project %in% c('en.wikipedia', 'hi.wikipedia')), referer_class == "external") %>%
       group_by(date, country) %>%
       summarize(pageviews = sum(pageviews)) %>%
       mutate(project = 'other wikis')},
    {pageviews %>%
       filter(country_code != 'IN', !(project %in% c('en.wikipedia', 'hi.wikipedia')), referer_class == "external") %>%
       group_by(date) %>%
       summarize(pageviews = sum(pageviews)) %>%
       mutate(country = 'other countries', project = 'other wikis')}
  ) %>%
  ungroup() %>%
  complete(date, nesting(country, project), fill = list(pageviews = 0)) %>%
  unite(metrics, country, project) %>%
  mutate(metrics = gsub(" ", ".", metrics, fixed = TRUE)) %>%
  spread(metrics, pageviews)
control_series_log <- data.frame(date = control_series$date, {
   mutate_all(control_series[, -1], log) %>%
     apply(2, function(x) replace(x, is.infinite(x), 0))
})

# Choose models

train_start <- min(test_series$date)
train_end <- tv_start - 60 - 1
validation_start <- tv_start - 60
validation_end <- tv_start - 1
model_spec <- expand.grid(
  controls = c("hiwiki_all", "mixed", "best_all", "none"),
  trend = c("local_level_0.01", "local_level_0.1", "local_linear", "semi_local", "static"),
  ar = c(TRUE, FALSE),
  seasonality = c(TRUE, FALSE),
  holiday = c(TRUE, FALSE),
  log = FALSE,
  train_start = as.Date(c("2016-01-01", "2017-01-01", "2017-10-01"))
)

n_iters <- 2e3
prev_param_log <- NULL
prev_param_start <- NULL
model_compare <- data.frame(matrix(nrow=nrow(model_spec), ncol=3))
names(model_compare) <- c('ID', 'MAPE', 'rsquare')

for (i in 1:nrow(model_spec)){
  param <- model_spec[i, ]
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
                                            n_candidates = 20)
    selected_controls <- c(
      selected_controls,
      list(
        hiwiki_all = grep("_hi.wikipedia$", colnames(x), value = TRUE),
        mixed = unique(c(head(selected_controls$dtw_all, 10), head(selected_controls$dtw_hiwiki, 10),
                head(selected_controls$corr_all, 10), head(selected_controls$corr_hiwiki, 10)
                )),
        best_all = unique(c(selected_controls$dtw_all, selected_controls$corr_all))
        )
      )
    selected_controls[1:4] <- NULL
  }

  this_model <- run_bsts_model(x, y, train_start, train_end, validation_start, validation_end, selected_controls,
    control_condidates = param$controls, trend = param$trend, autoAR = param$ar, seasonality = param$seasonality,
    holiday = param$holiday, niter = n_iters)

  this_impact <- CausalImpact(bsts.model = this_model$model,
                              post.period.response = as.numeric(this_model$post.period.response))
  this_impact$series <- apply(this_impact$series, 2, this_model$UnStandardize)
  if (param$log) {
    this_impact$series <- apply(this_impact$series, 2, exp)
  }

  model_compare$ID[i] <- i
  model_compare$MAPE[i] <- this_impact$series[as.character(seq.Date(validation_start, validation_end, by = "day")), ] %>%
    as.tibble %>%
    summarise(MAPE=mean(abs(response-point.pred)/response)) %>% unlist()
  model_compare$rsquare[i] <- summary(this_model$model)$rsquare

  prev_param_log <- param$log
  prev_param_start <- param$train_start
}

model_compare <- cbind(model_compare, model_spec)
save(model_compare, file = "data/hiwiki_external_pv_tvmodel_compare.RData")


# Examine the top models

load("data/hiwiki_external_pv_tvmodel_compare.RData")
top_model_ID <- model_compare$ID[model_compare$MAPE < 0.061]

## Generate model objects

## Compare one step ahead prediction errors
## on several validation period: 60 days before tv_start, 60 days before online_start
CompareBstsModels(list("Model 4" = model_4$model,
                       "Model 84" = model_84$model)) # 4 is better than 84 because of holiday effect
CompareBstsModels(list("Model 324" = model_324$model,
                       "Model 340" = model_340$model,
                       "Model 364" = model_364$model,
                       "Model 400" = model_400$model,
                       "Model 480" = model_480$model)) # 324 and 340 is better than others

## Compare their imaginery causal impact
impact_4 <- CausalImpact(bsts.model = model_4$model,
                           post.period.response = as.numeric(model_4$post.period.response))
impact_84 <- CausalImpact(bsts.model = model_84$model,
                           post.period.response = as.numeric(model_84$post.period.response))
impact_324 <- CausalImpact(bsts.model = model_324$model,
                           post.period.response = as.numeric(model_324$post.period.response))
impact_340 <- CausalImpact(bsts.model = model_340$model,
                           post.period.response = as.numeric(model_340$post.period.response))
impact_364 <- CausalImpact(bsts.model = model_364$model,
                           post.period.response = as.numeric(model_364$post.period.response))
impact_400 <- CausalImpact(bsts.model = model_400$model,
                           post.period.response = as.numeric(model_400$post.period.response))
impact_480 <- CausalImpact(bsts.model = model_480$model,
                           post.period.response = as.numeric(model_480$post.period.response))
plot(impact_4); plot(impact_84); plot(impact_324); plot(impact_340);
plot(impact_364); plot(impact_400); plot(impact_480);
# 4&84 over predict and under predict cancel out;
# 364&400&480 is a straight line...
sort(c(
impact_4=impact_4$summary$AbsEffect.sd[1],
impact_84=impact_84$summary$AbsEffect.sd[1],
impact_324=impact_324$summary$AbsEffect.sd[1],
impact_340=impact_340$summary$AbsEffect.sd[1],
impact_364=impact_364$summary$AbsEffect.sd[1],
impact_400=impact_400$summary$AbsEffect.sd[1],
impact_480=impact_480$summary$AbsEffect.sd[1]
))
impact_4$summary$AbsEffect.upper-impact_4$summary$AbsEffect.lower;
impact_84$summary$AbsEffect.upper-impact_84$summary$AbsEffect.lower;
impact_324$summary$AbsEffect.upper-impact_324$summary$AbsEffect.lower;
impact_340$summary$AbsEffect.upper-impact_340$summary$AbsEffect.lower;
impact_364$summary$AbsEffect.upper-impact_364$summary$AbsEffect.lower;
impact_400$summary$AbsEffect.upper-impact_400$summary$AbsEffect.lower;
impact_480$summary$AbsEffect.upper-impact_480$summary$AbsEffect.lower;
# AbsEffect 4 84 400  480 364 324 340; 4  84 364 400 480 324 340
# AbsEffect CI 400 480 4 84 364 324 340; 400 480 4 84 364 340 324;
# AbsEffect.sd 400 480 4 84 364 324 340; 400 480 4 84 364 340 324;


## Plot the selected control series to make sure they didn't affected by the intervention
x[, c(1, match(unlist(selected_controls["best_all"]), colnames(x)))] %>%
  filter(date >= as.Date("2018-01-01")) %>%
  gather(source, pageviews, -date) %>%
  ggplot(aes(x=date, y=pageviews, color=source)) +
  geom_line() +
  geom_vline(xintercept = online_start) +
  geom_vline(xintercept = tv_start, linetype = "dashed")


# Use model 4 to compute causal impact (using hacked CausalImpact package)

train_start <- as.Date("2016-01-01")
train_end <- tv_start - 1
test_start <- tv_start
test_end <- tv_start + 59
n_iters <- 1e4 + 2e3
y <- test_series
x <- control_series

cat("Finding matched series...\n")
selected_controls <- control_candidates(test = y, control = x,
                                        match_period_start = train_start, match_period_end = train_end,
                                        n_candidates = 20)
selected_controls <- c(
  selected_controls,
  list(
    hiwiki_all = grep("_hi.wikipedia$", colnames(x), value = TRUE),
    mixed = unique(c(head(selected_controls$dtw_all, 10), head(selected_controls$dtw_hiwiki, 10),
            head(selected_controls$corr_all, 10), head(selected_controls$corr_hiwiki, 10)
            )),
    best_all = unique(c(selected_controls$dtw_all, selected_controls$corr_all))
    )
  )
selected_controls[1:4] <- NULL

model_4 <- run_bsts_model(x, y, train_start, train_end, test_start, test_end, selected_controls,
  control_condidates = "none", trend = "local_level_0.01", autoAR = TRUE, seasonality = TRUE,
  holiday = TRUE, niter = n_iters)
model_4_impact <- CausalImpact(bsts.model = model_4$model,
                               post.period.response = model_4$UnStandardize(as.numeric(model_4$post.period.response)),
                               UnStandardize = model_4$UnStandardize)
# positive not significant
save(model_4_impact, file = "data/hiwiki_external_pv_tv_impact.RData")
p <- plot(model_4_impact)
p <- p + labs(title = "Impact of the TV campaign in 60 days on Hindi Wikipedia external referred pageviews from India",
              subtitle = "Vertical dashed line represents the date of TV campaign, May 27th")
ggsave("hiwiki_external_pv_tv_impact.png", plot = p, path = 'figures', units = "in", dpi = 300, height = 9, width = 18)

# Try model 84 324 340
model_84 <- run_bsts_model(x, y, train_start, train_end, test_start, test_end, selected_controls,
  control_condidates = "none", trend = "local_level_0.01", autoAR = TRUE, seasonality = TRUE,
  holiday = FALSE, niter = n_iters)
model_84_impact <- CausalImpact(bsts.model = model_84$model,
                               post.period.response = model_84$UnStandardize(as.numeric(model_84$post.period.response)),
                               UnStandardize = model_84$UnStandardize)
plot(model_84_impact) # positive not significant

train_start <- as.Date("2017-10-01")
model_324 <- run_bsts_model(x, y, train_start, train_end, test_start, test_end, selected_controls,
  control_condidates = "none", trend = "local_level_0.01", autoAR = TRUE, seasonality = TRUE,
  holiday = TRUE, niter = n_iters)
model_324_impact <- CausalImpact(bsts.model = model_324$model,
                               post.period.response = model_324$UnStandardize(as.numeric(model_324$post.period.response)),
                               UnStandardize = model_324$UnStandardize)
plot(model_324_impact) # positive not significant

model_340 <- run_bsts_model(x, y, train_start, train_end, test_start, test_end, selected_controls,
  control_condidates = "none", trend = "static", autoAR = TRUE, seasonality = TRUE,
  holiday = TRUE, niter = n_iters)
model_340_impact <- CausalImpact(bsts.model = model_340$model,
                               post.period.response = model_340$UnStandardize(as.numeric(model_340$post.period.response)),
                               UnStandardize = model_340$UnStandardize)
plot(model_340_impact) # positive not significant

# Not use custom model
train_start <- as.Date("2017-10-01")
train_data <- data.frame(
  y = filter(y, date >= train_start & date <= test_end) %>% select(-date) %>% unlist(),
  filter(x, date >= train_start & date <= test_end)[, match(unlist(selected_controls["mixed"]), colnames(x))]
) %>%
  xts::xts(order.by = seq.Date(train_start, test_end, "day"))
impact <- CausalImpact(train_data, c(train_start, train_end), c(test_start, test_end),
                       model.args = list(niter = n_iters, nseasons = 12, season.duration = 30))
plot(impact) # 2016/2017: negative, not significant impact; 2017-10 positive but not significant
