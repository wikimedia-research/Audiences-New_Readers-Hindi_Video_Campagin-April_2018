# devtools::install_github("google/CausalImpact", force = TRUE)
# install.packages("dtw")

library(CausalImpact)
library(dtw)

source("refine.R")
source("functions.R")

# hiwiki first visit ud from India

cat("Data preparation...\n")
test_series <- unique_devices %>%
  filter(country_code == 'IN', project == 'hi.wikipedia', type == 'first_visit') %>%
  group_by(date) %>%
  summarize(India_hi.wikipedia = sum(uniques)) %>%
  ungroup() %>%
  complete(date, fill = list(India_hi.wikipedia = 0))
test_series_log <- mutate(test_series, India_hi.wikipedia = log(India_hi.wikipedia)) %>%
  mutate(India_hi.wikipedia = ifelse(India_hi.wikipedia == -Inf, 0, India_hi.wikipedia))
control_series <- unique_devices %>%
  filter(country_code != 'IN', type == 'first_visit') %>%
  group_by(date, country, project) %>%
  summarize(uniques = sum(uniques)) %>%
  bind_rows(
    {unique_devices %>%
       filter(country_code != 'IN', type == 'first_visit') %>%
       group_by(date, project) %>%
       summarize(uniques = sum(uniques)) %>%
       mutate(country = 'other countries')},
    {unique_devices %>%
       filter(country_code != 'IN', !(project %in% c('en.wikipedia', 'hi.wikipedia')), type == 'first_visit') %>%
       group_by(date, country) %>%
       summarize(uniques = sum(uniques)) %>%
       mutate(project = 'other wikis')},
    {unique_devices %>%
       filter(country_code != 'IN', !(project %in% c('en.wikipedia', 'hi.wikipedia')), type == 'first_visit') %>%
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
save(model_compare, file = "data/hiwiki_first_visit_ud_tvmodel_compare.RData")


# Examine the top models

top_model_ID <- model_compare %>% top_n(10, desc(MAPE)) %>% .$ID
## Generate model objects

## Compare one step ahead prediction errors
CompareBstsModels(list("Model 28" = model_28$model,
                       "Model 24" = model_24$model,
                       "Model 36" = model_36$model)) # 36 is slightly better
CompareBstsModels(list("Model 188" = model_188$model,
                       "Model 196" = model_196$model)) # the same
CompareBstsModels(list("Model 394" = model_394$model,
                       "Model 442" = model_442$model,
                       "Model 470" = model_470$model,
                       "Model 471" = model_471$model,
                       "Model 473" = model_473$model)) # 394 is much better than others

## Compare their imaginery causal impact
impact_24 <- CausalImpact(bsts.model = model_24$model,
                           post.period.response = as.numeric(model_24$post.period.response))
impact_28 <- CausalImpact(bsts.model = model_28$model,
                           post.period.response = as.numeric(model_28$post.period.response))
impact_36 <- CausalImpact(bsts.model = model_36$model,
                           post.period.response = as.numeric(model_36$post.period.response))
impact_188 <- CausalImpact(bsts.model = model_188$model,
                           post.period.response = as.numeric(model_188$post.period.response))
impact_196 <- CausalImpact(bsts.model = model_196$model,
                           post.period.response = as.numeric(model_196$post.period.response))
impact_394 <- CausalImpact(bsts.model = model_394$model,
                           post.period.response = as.numeric(model_394$post.period.response))
impact_442 <- CausalImpact(bsts.model = model_442$model,
                           post.period.response = as.numeric(model_442$post.period.response))
impact_470 <- CausalImpact(bsts.model = model_470$model,
                           post.period.response = as.numeric(model_470$post.period.response))
impact_471 <- CausalImpact(bsts.model = model_471$model,
                           post.period.response = as.numeric(model_471$post.period.response))
impact_473 <- CausalImpact(bsts.model = model_473$model,
                           post.period.response = as.numeric(model_473$post.period.response))
plot(impact_24); plot(impact_28); plot(impact_36); plot(impact_188);
plot(impact_196); plot(impact_394); plot(impact_442); plot(impact_470);
plot(impact_471); plot(impact_473);
# 470 471 better than 442 394
# 473 is almost a straight line
sort(abs(c(
impact_24=impact_24$summary$AbsEffect.sd[1],
impact_28=impact_28$summary$AbsEffect.sd[1],
impact_36=impact_36$summary$AbsEffect.sd[1],
impact_188=impact_188$summary$AbsEffect.sd[1],
impact_196=impact_196$summary$AbsEffect.sd[1],
impact_394=impact_394$summary$AbsEffect.sd[1],
impact_442=impact_442$summary$AbsEffect.sd[1],
impact_470=impact_470$summary$AbsEffect.sd[1],
impact_471=impact_471$summary$AbsEffect.sd[1],
impact_473=impact_473$summary$AbsEffect.sd[1]
)))
impact_24$summary$AbsEffect.upper-impact_24$summary$AbsEffect.lower;
impact_28$summary$AbsEffect.upper-impact_28$summary$AbsEffect.lower;
impact_36$summary$AbsEffect.upper-impact_36$summary$AbsEffect.lower;
impact_188$summary$AbsEffect.upper-impact_188$summary$AbsEffect.lower;
impact_196$summary$AbsEffect.upper-impact_196$summary$AbsEffect.lower;
impact_394$summary$AbsEffect.upper-impact_394$summary$AbsEffect.lower;
impact_442$summary$AbsEffect.upper-impact_442$summary$AbsEffect.lower;
impact_470$summary$AbsEffect.upper-impact_470$summary$AbsEffect.lower;
# AbsEffect 470 196 473  36  28  24 188 471 394 442;
# AbsEffect CI 24  28  36 188 442 196 394 471 470
# AbsEffect.sd 24  28  36 188 442 196 394 471 470


## Plot the selected control series to make sure they didn't affected by the intervention
x[, c(1, match(unlist(selected_controls["mixed"]), colnames(x)))] %>%
  filter(date >= as.Date("2018-01-01")) %>%
  gather(source, uniques, -date) %>%
  ggplot(aes(x=date, y=uniques, color=source)) +
  geom_line() +
  geom_vline(xintercept = online_start) +
  geom_vline(xintercept = tv_start, linetype = "dashed")


# Try model 470 394 36 to compute causal impact (using hacked CausalImpact package)

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

model_470 <- run_bsts_model(x, y, train_start, train_end, test_start, test_end, selected_controls,
  control_condidates = "mixed", trend = "local_linear", autoAR = FALSE, seasonality = FALSE,
  holiday = FALSE, niter = n_iters)
model_470_impact <- CausalImpact(bsts.model = model_470$model,
                                 post.period.response = model_470$UnStandardize(as.numeric(model_470$post.period.response)),
                                 UnStandardize = model_470$UnStandardize)
plot(model_470_impact) # No impact

model_394 <- run_bsts_model(x, y, train_start, train_end, test_start, test_end, selected_controls,
  control_condidates = "mixed", trend = "semi_local", autoAR = FALSE, seasonality = FALSE,
  holiday = TRUE, niter = n_iters)
model_394_impact <- CausalImpact(bsts.model = model_394$model,
                                 post.period.response = model_394$UnStandardize(as.numeric(model_394$post.period.response)),
                                 UnStandardize = model_394$UnStandardize)
plot(model_394_impact) # No impact

model_36 <- run_bsts_model(x, y, train_start, train_end, test_start, test_end, selected_controls,
  control_condidates = "none", trend = "semi_local", autoAR = FALSE, seasonality = TRUE,
  holiday = TRUE, niter = n_iters)
model_36_impact <- CausalImpact(bsts.model = model_36$model,
                                 post.period.response = model_36$UnStandardize(as.numeric(model_36$post.period.response)),
                                 UnStandardize = model_36$UnStandardize)
plot(model_36_impact) # No impact

p <- plot(model_470_impact)
p <- p + labs(title = "Impact of the TV campaign in 60 days on Hindi Wikipedia first visit unique devices from India",
              subtitle = "Vertical dashed line represents the date of TV campaign, May 27th")
ggsave("hiwiki_first_visit_ud_tv_impact.png", plot = p, path = 'figures', units = "in", dpi = 300, height = 9, width = 18)

save(model_470_impact, file = "data/hiwiki_first_visit_ud_tv_impact.RData")

# Not use custom model
train_start <- as.Date("2017-10-01")
train_data <- data.frame(
  y = filter(y, date >= train_start & date <= test_end) %>% select(-date) %>% unlist(),
  filter(x, date >= train_start & date <= test_end)[, match(unlist(selected_controls["mixed"]), colnames(x))]
) %>%
  xts::xts(order.by = seq.Date(train_start, test_end, "day"))
impact <- CausalImpact(train_data, c(train_start, train_end), c(test_start, test_end),
                       model.args = list(niter = n_iters, nseasons = 12, season.duration = 30))
plot(impact) # 17/10/1 positive but not significant; 17/1/1 no impact; 16/1/1 no impact

