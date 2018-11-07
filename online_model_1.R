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

# Choose models

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
prev_param_log <- NULL
prev_param_start <- NULL
model_compare <- data.frame(matrix(nrow=nrow(model_spec_online), ncol=3))
names(model_compare) <- c('ID', 'MAPE', 'rsquare')

for (i in 1:nrow(model_spec_online)){
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

model_compare <- cbind(model_compare, model_spec_online)
save(model_compare, file = "data/hiwiki_pv_model_compare.RData")


# Examine the top models

## run the models with MAPE<0.05 again with larger iteration (1e4)
load("data/hiwiki_pv_top_model_compare_1e4.RData")
top_model_ID <- top_model_compare %>% arrange(MAPE) %>% head(4) %>% .$ID

## Generate model objects

## Compare one step ahead prediction errors
CompareBstsModels(list("Model 499" = model_499$model,
                       "Model 579" = model_579$model,
                       "Model 483" = model_483$model,
                       "Model 563" = model_563$model)) # almost no difference in these 4 models

## Compare their imaginery causal impact
impact_499 <- CausalImpact(bsts.model = model_499$model,
                           post.period.response = as.numeric(model_499$post.period.response))
impact_579 <- CausalImpact(bsts.model = model_579$model,
                           post.period.response = as.numeric(model_579$post.period.response))
impact_483 <- CausalImpact(bsts.model = model_483$model,
                           post.period.response = as.numeric(model_483$post.period.response))
impact_563 <- CausalImpact(bsts.model = model_563$model,
                           post.period.response = as.numeric(model_563$post.period.response))
plot(impact_499); plot(impact_579); plot(impact_483); plot(impact_563); # their CI all increase fast
impact_499$summary; impact_579$summary; impact_483$summary; impact_563$summary
# AbsEffect 579<483<499<563
# AbsEffect CI 579<563<483<499
# AbsEffect.sd 579<563<483<499
# p-value 579>499>483>563

## Plot the selected control series to make sure they didn't affected by the intervention
x[, c(1, match(unlist(selected_controls["best_all"]), colnames(x)))] %>%
  filter(date >= as.Date("2018-01-01")) %>%
  gather(source, pageviews, -date) %>%
  ggplot(aes(x=date, y=pageviews, color=source)) +
  geom_line() +
  geom_vline(xintercept = online_start) +
  geom_vline(xintercept = tv_start, linetype = "dashed")


# Use model 579 to compute causal impact (using hacked CausalImpact package)

train_start <- as.Date("2017-01-01")
train_end <- online_start - 1
test_start <- online_start
test_end <- online_start + 59
n_iters <- 1e4 + 2e3
y <- test_series
x <- control_series

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

model_579 <- run_bsts_model(x, y, train_start, train_end, test_start, test_end, selected_controls,
  control_condidates = "best_all", trend = "static", autoAR = TRUE, seasonality = TRUE,
  holiday = FALSE, niter = n_iters)
model_579_impact <- CausalImpact(bsts.model = model_579$model,
                                 post.period.response = model_579$UnStandardize(as.numeric(model_579$post.period.response)),
                                 UnStandardize = model_579$UnStandardize)
save(model_579_impact, file = "data/hiwiki_pv_online_impact.RData")
p <- plot(model_579_impact)
p <- p + labs(title = "Impact of the online campaign in 60 days on Hindi Wikipedia pageviews from Madhya Pradesh",
              subtitle = "Dashed line represents the start date of the online campaign on YouTube and Facebook, April 3rd")
ggsave("hiwiki_pv_online_impact.png", plot = p, path = 'figures', units = "in", dpi = 300, height = 9, width = 18)


# Not use custom model
## In validation period we tried fit the data using CausalImpact directly on the same validation period (model 902),
## performance is ok, so we tried again here
train_start <- as.Date("2017-10-01")
train_data <- data.frame(
  y = filter(y, date >= train_start & date <= test_end) %>% select(-date) %>% unlist(),
  filter(x, date >= train_start & date <= test_end)[, match(unlist(selected_controls["mixed"]), colnames(x))]
) %>%
  xts::xts(order.by = seq.Date(train_start, test_end, "day"))
impact <- CausalImpact(train_data, c(train_start, train_end), c(test_start, test_end),
                       model.args = list(niter = n_iters, nseasons = 12, season.duration = 30))
plot(impact) # No significant impact

