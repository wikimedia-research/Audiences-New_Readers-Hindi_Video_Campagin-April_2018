# devtools::install_github("google/CausalImpact", force = TRUE)
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
save(model_compare, file = "data/hiwiki_ud_tvmodel_compare.RData")


# Examine the top models

load("data/hiwiki_ud_tvmodel_compare.RData")
top_model_ID <- model_compare$ID[model_compare$MAPE < 0.046]
## Generate model objects

## Compare imaginery causal impact for top_model_ID
## on several validation period: 60 days before tv_start, 60 days before online_start
impact_926 <- CausalImpact(bsts.model = model_926$model,
                           post.period.response = as.numeric(model_926$post.period.response))
impact_802 <- CausalImpact(bsts.model = model_802$model,
                           post.period.response = as.numeric(model_802$post.period.response))
impact_262 <- CausalImpact(bsts.model = model_262$model,
                           post.period.response = as.numeric(model_262$post.period.response))
impact_182 <- CausalImpact(bsts.model = model_182$model,
                           post.period.response = as.numeric(model_182$post.period.response))
impact_127 <- CausalImpact(bsts.model = model_127$model,
                           post.period.response = as.numeric(model_127$post.period.response))
impact_147 <- CausalImpact(bsts.model = model_147$model,
                           post.period.response = as.numeric(model_147$post.period.response))
impact_3 <- CausalImpact(bsts.model = model_3$model,
                           post.period.response = as.numeric(model_3$post.period.response))
plot(impact_926); plot(impact_802); plot(impact_262); plot(impact_182);
plot(impact_127); plot(impact_147); plot(impact_3);
# For 60 days before tv start, 926&802 seems better than 262&182 at the bump
# For 60 days before online start, 926&262&182 are very bad, 127&147 are bad too but better, 802&3 are good
impact_926$summary$p
impact_802$summary$p
impact_262$summary$p
impact_182$summary$p
impact_127$summary$p
impact_147$summary$p
impact_3$summary$p
impact_926$summary$AbsEffect.upper-impact_926$summary$AbsEffect.lower;
impact_802$summary$AbsEffect.upper-impact_802$summary$AbsEffect.lower;
impact_262$summary$AbsEffect.upper-impact_262$summary$AbsEffect.lower;
impact_182$summary$AbsEffect.upper-impact_182$summary$AbsEffect.lower;
impact_127$summary$AbsEffect.upper-impact_127$summary$AbsEffect.lower;
impact_147$summary$AbsEffect.upper-impact_147$summary$AbsEffect.lower;
impact_3$summary$AbsEffect.upper-impact_3$summary$AbsEffect.lower;
# AbsEffect 127<147<262<3<182<926<802; 3<127<147<802<182<262<926
# AbsEffect CI 182<262<147<127<3<926<802; 182<262<147<127<3<926<802;
# AbsEffect.sd 182<262<147<127<3<926<802; 182<262<147<127<3<926<802;
# p-value 926>127>147>3>262>802>182; 3>802>127>147


## Plot the selected control series to make sure they didn't affected by the intervention
x[, c(1, match(unlist(selected_controls["mixed"]), colnames(x)))] %>%
  filter(date >= as.Date("2018-01-01")) %>%
  gather(source, uniques, -date) %>%
  ggplot(aes(x=date, y=uniques, color=source)) +
  geom_line() +
  geom_vline(xintercept = online_start) +
  geom_vline(xintercept = tv_start, linetype = "dashed")


# Use model 802 to compute causal impact (using hacked CausalImpact package)

train_start <- as.Date("2017-10-01")
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

model_802 <- run_bsts_model(x, y, train_start, train_end, test_start, test_end, selected_controls,
  control_condidates = "mixed", trend = "local_level_0.01", autoAR = TRUE, seasonality = TRUE,
  holiday = TRUE, niter = n_iters)
model_802_impact <- CausalImpact(bsts.model = model_802$model,
                                 post.period.response = model_802$UnStandardize(as.numeric(model_802$post.period.response)),
                                 UnStandardize = model_802$UnStandardize)
# not significant positive impact
save(model_802_impact, file = "data/hiwiki_ud_tv_impact.RData")
p <- plot(model_802_impact)
p <- p + labs(title = "Impact of the TV campaign in 60 days on Hindi Wikipedia unique devices from India",
              subtitle = "Vertical dashed line represents the date of TV campaign, May 27th")
ggsave("hiwiki_ud_tv_impact.png", plot = p, path = 'figures', units = "in", dpi = 300, height = 9, width = 18)

# Try model 182,262,926
train_start <- as.Date("2016-01-01")
model_182 <- run_bsts_model(x, y, train_start, train_end, test_start, test_end, selected_controls,
  control_condidates = "mixed", trend = "local_level_0.01", autoAR = FALSE, seasonality = TRUE,
  holiday = TRUE, niter = n_iters)
model_182_impact <- CausalImpact(bsts.model = model_182$model,
                                 post.period.response = model_182$UnStandardize(as.numeric(model_182$post.period.response)),
                                 UnStandardize = model_182$UnStandardize)
plot(model_182_impact) # not significant positive impact

model_262 <- run_bsts_model(x, y, train_start, train_end, test_start, test_end, selected_controls,
  control_condidates = "mixed", trend = "local_level_0.01", autoAR = FALSE, seasonality = TRUE,
  holiday = FALSE, niter = n_iters)
model_262_impact <- CausalImpact(bsts.model = model_262$model,
                                 post.period.response = model_262$UnStandardize(as.numeric(model_262$post.period.response)),
                                 UnStandardize = model_262$UnStandardize)
plot(model_262_impact) # not significant positive impact

train_start <- as.Date("2017-10-01")
model_926 <- run_bsts_model(x, y, train_start, train_end, test_start, test_end, selected_controls,
  control_condidates = "mixed", trend = "local_level_0.1", autoAR = TRUE, seasonality = FALSE,
  holiday = FALSE, niter = n_iters)
model_926_impact <- CausalImpact(bsts.model = model_926$model,
                                 post.period.response = model_926$UnStandardize(as.numeric(model_926$post.period.response)),
                                 UnStandardize = model_926$UnStandardize)
plot(model_926_impact) # not significant positive impact


# Not use custom model
train_start <- as.Date("2017-10-01")
train_data <- data.frame(
  y = filter(y, date >= train_start & date <= test_end) %>% select(-date) %>% unlist(),
  filter(x, date >= train_start & date <= test_end)[, match(unlist(selected_controls["mixed"]), colnames(x))]
) %>%
  xts::xts(order.by = seq.Date(train_start, test_end, "day"))
impact <- CausalImpact(train_data, c(train_start, train_end), c(test_start, test_end),
                       model.args = list(niter = n_iters, nseasons = 12, season.duration = 30))
plot(impact) # similar to model 802, positive impact, but not significant

