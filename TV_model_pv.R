# devtools::install_github("google/CausalImpact", force = TRUE)
# install.packages("dtw")

library(CausalImpact)
library(dtw)

source("refine.R")
source("functions.R")

# hiwiki pv from India

cat("Data preparation...\n")
test_series <- pageviews %>%
  filter(country_code == 'IN', project == 'hi.wikipedia') %>%
  group_by(date) %>%
  summarize(India_hi.wikipedia = sum(pageviews)) %>%
  ungroup() %>%
  complete(date, fill = list(India_hi.wikipedia = 0))
test_series_log <- mutate(test_series, India_hi.wikipedia = log(India_hi.wikipedia)) %>%
  mutate(India_hi.wikipedia = ifelse(India_hi.wikipedia == -Inf, 0, India_hi.wikipedia))
control_series <- pageviews %>%
  filter(country_code != 'IN') %>%
  group_by(date, country, project) %>%
  summarize(pageviews = sum(pageviews)) %>%
  bind_rows(
    {pageviews %>%
       filter(country_code != 'IN') %>%
       group_by(date, project) %>%
       summarize(pageviews = sum(pageviews)) %>%
       mutate(country = 'other countries')},
    {pageviews %>%
       filter(country_code != 'IN', !(project %in% c('en.wikipedia', 'hi.wikipedia'))) %>%
       group_by(date, country) %>%
       summarize(pageviews = sum(pageviews)) %>%
       mutate(project = 'other wikis')},
    {pageviews %>%
       filter(country_code != 'IN', !(project %in% c('en.wikipedia', 'hi.wikipedia'))) %>%
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
# since the best models didn't include any controls, and there was a weird bump on 5/14
# we tried shift the validation period a bit early to train again
# train_start <- min(test_series$date)
# train_end <- tv_start - 90 - 1
# validation_start <- tv_start - 90
# validation_end <- as.Date("2018-05-10")

model_spec <- expand.grid(
  controls = c("hiwiki_all", "mixed", "best_all", "none"),
  trend = c("local_level_0.01", "local_level_0.1", "local_linear", "semi_local", "static"),
  ar = c(TRUE, FALSE),
  seasonality = c(TRUE, FALSE),
  holiday = c(TRUE, FALSE),
  log = c(TRUE, FALSE),
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
save(model_compare, file = "data/hiwiki_pv_tvmodel_compare.RData")


# Examine the top models

## run the models with MAPE<0.06 and top 5 models with controls, using larger iteration (1e4)
load("data/hiwiki_pv_tvmodel_compare.RData")
load("data/hiwiki_pv_top_tvmodel_compare_1e4.RData")
top_model_ID <- sort(top_model_compare %>% arrange(MAPE) %>% head(5) %>% .$ID)
# For the 60 days validation period, top 5 models are 164, 244, 408(log), 804, 820
# For the shifted 72 days validation period, top 5 models are 180, 260, 804, 820, 840
top_model_ID <- sort(c(164, 244, 804, 820, 180, 260, 840))

## Generate model objects

## Compare one step ahead prediction errors
CompareBstsModels(list("Model 164" = model_164$model,
                       "Model 244" = model_244$model)) # 164 better at the holiday
CompareBstsModels(list("Model 804" = model_804$model,
                       "Model 820" = model_820$model)) # almost no difference
#  "Model 408" = model_408$model, # can't compare because this is log transformed

## Compare imaginery causal impact for top_model_ID
## on several validation period: 60 days before tv_start, 60 days before online_start
impact_164 <- CausalImpact(bsts.model = model_164$model,
                           post.period.response = as.numeric(model_164$post.period.response))
impact_244 <- CausalImpact(bsts.model = model_244$model,
                           post.period.response = as.numeric(model_244$post.period.response))
impact_804 <- CausalImpact(bsts.model = model_804$model,
                           post.period.response = as.numeric(model_804$post.period.response))
impact_820 <- CausalImpact(bsts.model = model_820$model,
                           post.period.response = as.numeric(model_820$post.period.response))
impact_180 <- CausalImpact(bsts.model = model_180$model,
                           post.period.response = as.numeric(model_180$post.period.response))
impact_260 <- CausalImpact(bsts.model = model_260$model,
                           post.period.response = as.numeric(model_260$post.period.response))
impact_840 <- CausalImpact(bsts.model = model_840$model,
                           post.period.response = as.numeric(model_840$post.period.response))
plot(impact_164); plot(impact_244); plot(impact_804); plot(impact_820);
plot(impact_180); plot(impact_260); plot(impact_840);
# For 60 days before tv start, 164&244 find a average before&after the 5/14 bumps,
# 804, 820, 180, 260, 840 fit the before bumps part better
# For 60 days before online start, 840, 244 seems better
impact_164$summary$p
impact_244$summary$p
impact_804$summary$p
impact_820$summary$p
impact_180$summary$p
impact_260$summary$p
impact_840$summary$p
impact_164_tv$summary$AbsEffect.upper-impact_164_tv$summary$AbsEffect.lower;
impact_244_tv$summary$AbsEffect.upper-impact_244_tv$summary$AbsEffect.lower;
impact_804_tv$summary$AbsEffect.upper-impact_804_tv$summary$AbsEffect.lower;
impact_820_tv$summary$AbsEffect.upper-impact_820_tv$summary$AbsEffect.lower;
impact_180_tv$summary$AbsEffect.upper-impact_180_tv$summary$AbsEffect.lower;
impact_260_tv$summary$AbsEffect.upper-impact_260_tv$summary$AbsEffect.lower;
impact_840_tv$summary$AbsEffect.upper-impact_840_tv$summary$AbsEffect.lower;
# AbsEffect 164<244<180<840<260<804<820; 840<244<164<260<180<804<820
# AbsEffect CI 164<244<180<260<840<820<804; 164<244<180<260<840<804<820
# AbsEffect.sd 164<244<180<260<840<820<804; 164<244<180<260<840<820<804
# p-value 164>244>840>820>180>804>260; 840>164>244>260>180>840>820


# Use model 164 to compute causal impact (using hacked CausalImpact package)
## Not find any useful controls -- may be the result of noise in the controls

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

model_164 <- run_bsts_model(x, y, train_start, train_end, test_start, test_end, selected_controls,
  control_condidates = "none", trend = "local_level_0.01", autoAR = TRUE, seasonality = TRUE,
  holiday = TRUE, niter = n_iters)
model_164_impact <- CausalImpact(bsts.model = model_164$model,
                                 post.period.response = model_164$UnStandardize(as.numeric(model_164$post.period.response)),
                                 UnStandardize = model_164$UnStandardize)
# significant positive impact
save(model_164_impact, file = "data/hiwiki_pv_tv_impact.RData")
p <- plot(model_164_impact)
p <- p + labs(title = "Impact of the TV campaign in 60 days on Hindi Wikipedia pageviews from India",
              subtitle = "Vertical dashed line represents the date of TV campaign, May 27th")
ggsave("hiwiki_pv_tv_impact.png", plot = p, path = 'figures', units = "in", dpi = 300, height = 9, width = 18)


# Try model 244, 840 to see if there is any difference
model_244 <- run_bsts_model(x, y, train_start, train_end, test_start, test_end, selected_controls,
  control_condidates = "none", trend = "local_level_0.01", autoAR = TRUE, seasonality = TRUE,
  holiday = FALSE, niter = n_iters)
model_244_impact <- CausalImpact(bsts.model = model_244$model,
                                 post.period.response = model_244$UnStandardize(as.numeric(model_244$post.period.response)),
                                 UnStandardize = model_244$UnStandardize)
plot(model_244_impact) # significant positive impact

train_start <- as.Date("2017-10-01")
model_840 <- run_bsts_model(x, y, train_start, train_end, test_start, test_end, selected_controls,
  control_condidates = "none", trend = "static", autoAR = FALSE, seasonality = TRUE,
  holiday = TRUE, niter = n_iters)
model_840_impact <- CausalImpact(bsts.model = model_840$model,
                                 post.period.response = model_840$UnStandardize(as.numeric(model_840$post.period.response)),
                                 UnStandardize = model_840$UnStandardize)
plot(model_840_impact) # significant positive impact

# Not use custom model
train_start <- as.Date("2016-10-01")
train_data <- data.frame(
  y = filter(y, date >= train_start & date <= test_end) %>% select(-date) %>% unlist(),
  filter(x, date >= train_start & date <= test_end)[, match(unlist(selected_controls["mixed"]), colnames(x))]
) %>%
  xts::xts(order.by = seq.Date(train_start, test_end, "day"))
impact <- CausalImpact(train_data, c(train_start, train_end), c(test_start, test_end),
                       model.args = list(niter = n_iters, nseasons = 12, season.duration = 30))
plot(impact) # positive impact, but not significant
