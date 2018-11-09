# devtools::install_github("google/CausalImpact", force = TRUE)
# install.packages("dtw")

library(CausalImpact)
library(dtw)

source("refine.R")
source("functions.R")

# hiwiki external pv from Madhya Pradesh

cat("Data preparation...\n")
test_series <- pageviews %>%
  filter(country_code == 'IN', subdivision == 'Madhya Pradesh', project == 'hi.wikipedia', referer_class == "external") %>%
  group_by(date) %>%
  summarize(Madhya.Pradesh_hi.wikipedia = sum(pageviews)) %>%
  ungroup() %>%
  complete(date, fill = list(Madhya.Pradesh_hi.wikipedia = 0))
test_series_log <- mutate(test_series, Madhya.Pradesh_hi.wikipedia = log(Madhya.Pradesh_hi.wikipedia)) %>%
  mutate(Madhya.Pradesh_hi.wikipedia = ifelse(Madhya.Pradesh_hi.wikipedia == -Inf, 0, Madhya.Pradesh_hi.wikipedia))
control_series <- pageviews %>%
  filter(country_code == 'IN', subdivision != 'Madhya Pradesh', referer_class == "external") %>%
  group_by(date, subdivision, project) %>%
  summarize(pageviews = sum(pageviews)) %>%
  bind_rows(
    {pageviews %>%
       filter(country_code == 'IN', subdivision != 'Madhya Pradesh', referer_class == "external") %>%
       group_by(date, project) %>%
       summarize(pageviews = sum(pageviews)) %>%
       mutate(subdivision = 'other states')},
    {pageviews %>%
       filter(country_code == 'IN', subdivision != 'Madhya Pradesh', !(project %in% c('en.wikipedia', 'hi.wikipedia')), referer_class == "external") %>%
       group_by(date, subdivision) %>%
       summarize(pageviews = sum(pageviews)) %>%
       mutate(project = 'other wikis')},
    {pageviews %>%
       filter(country_code == 'IN', subdivision != 'Madhya Pradesh', !(project %in% c('en.wikipedia', 'hi.wikipedia')), referer_class == "external") %>%
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
save(model_compare, file = "data/hiwiki_external_pv_model_compare.RData")


# Examine the top models
load("data/hiwiki_external_pv_model_compare.RData")
top_model_ID <- model_compare$ID[model_compare$MAPE < 0.045 & model_compare$log == FALSE]

## Generate model objects

## Compare one step ahead prediction errors
CompareBstsModels(list("Model 803" = model_803$model,
                       "Model 818" = model_818$model,
                       "Model 819" = model_819$model,
                       "Model 823" = model_823$model,
                       "Model 839" = model_839$model,
                       "Model 883" = model_883$model,
                       "Model 886" = model_886$model,
                       "Model 899" = model_899$model)) # 886 is better than others

## Compare their imaginery causal impact
impact_803 <- CausalImpact(bsts.model = model_803$model,
                           post.period.response = as.numeric(model_803$post.period.response))
impact_818 <- CausalImpact(bsts.model = model_818$model,
                           post.period.response = as.numeric(model_818$post.period.response))
impact_819 <- CausalImpact(bsts.model = model_819$model,
                           post.period.response = as.numeric(model_819$post.period.response))
impact_823 <- CausalImpact(bsts.model = model_823$model,
                           post.period.response = as.numeric(model_823$post.period.response))
impact_839 <- CausalImpact(bsts.model = model_839$model,
                           post.period.response = as.numeric(model_839$post.period.response))
impact_883 <- CausalImpact(bsts.model = model_883$model,
                           post.period.response = as.numeric(model_883$post.period.response))
impact_886 <- CausalImpact(bsts.model = model_886$model,
                           post.period.response = as.numeric(model_886$post.period.response))
impact_899 <- CausalImpact(bsts.model = model_899$model,
                           post.period.response = as.numeric(model_899$post.period.response))
plot(impact_803); plot(impact_818); plot(impact_819); plot(impact_823);
plot(impact_839); plot(impact_883); plot(impact_886); plot(impact_899);
# 886 is better because it over predict in a later weekly cycle
sort(desc(c(
impact_803=impact_803$summary$p[1],
impact_818=impact_818$summary$p[1],
impact_819=impact_819$summary$p[1],
impact_823=impact_823$summary$p[1],
impact_839=impact_839$summary$p[1],
impact_883=impact_883$summary$p[1],
impact_886=impact_886$summary$p[1],
impact_899=impact_899$summary$p[1]
)))
impact_803$summary$AbsEffect.upper-impact_803$summary$AbsEffect.lower;
impact_818$summary$AbsEffect.upper-impact_818$summary$AbsEffect.lower;
impact_819$summary$AbsEffect.upper-impact_819$summary$AbsEffect.lower;
impact_823$summary$AbsEffect.upper-impact_823$summary$AbsEffect.lower;
impact_839$summary$AbsEffect.upper-impact_839$summary$AbsEffect.lower;
impact_883$summary$AbsEffect.upper-impact_883$summary$AbsEffect.lower;
impact_886$summary$AbsEffect.upper-impact_886$summary$AbsEffect.lower;
impact_899$summary$AbsEffect.upper-impact_899$summary$AbsEffect.lower;
# AbsEffect 886 839 823 803 883 819 818 899
# AbsEffect CI 823 839 818 899 883 819 803 886
# AbsEffect.sd 823 839 818 819 883 899 803 886
# p-value 886 839 823 803 818 883 819 899

# Try 886, 823 and 839


## Plot the selected control series to make sure they didn't affected by the intervention
x[, c(1, match(unlist(selected_controls["best_all"]), colnames(x)))] %>%
  filter(date >= as.Date("2018-01-01")) %>%
  gather(source, pageviews, -date) %>%
  ggplot(aes(x=date, y=pageviews, color=source)) +
  geom_line() +
  geom_vline(xintercept = online_start) +
  geom_vline(xintercept = tv_start, linetype = "dashed")


# Use model 886, 839&823 to compute causal impact (using hacked CausalImpact package)

train_start <- as.Date("2017-10-01")
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

model_839 <- run_bsts_model(x, y, train_start, train_end, test_start, test_end, selected_controls,
  control_condidates = "best_all", trend = "static", autoAR = FALSE, seasonality = TRUE,
  holiday = TRUE, niter = n_iters)
model_839_impact <- CausalImpact(bsts.model = model_839$model,
                                 post.period.response = model_839$UnStandardize(as.numeric(model_839$post.period.response)),
                                 UnStandardize = model_839$UnStandardize)
plot(model_839_impact) # negative not significant impact

model_823 <- run_bsts_model(x, y, train_start, train_end, test_start, test_end, selected_controls,
  control_condidates = "best_all", trend = "local_level_0.01", autoAR = FALSE, seasonality = TRUE,
  holiday = TRUE, niter = n_iters)
model_823_impact <- CausalImpact(bsts.model = model_823$model,
                                 post.period.response = model_823$UnStandardize(as.numeric(model_823$post.period.response)),
                                 UnStandardize = model_823$UnStandardize)
plot(model_823_impact) # negative not significant impact

model_886 <- run_bsts_model(x, y, train_start, train_end, test_start, test_end, selected_controls,
  control_condidates = "mixed", trend = "local_level_0.1", autoAR = TRUE, seasonality = TRUE,
  holiday = FALSE, niter = n_iters)
model_886_impact <- CausalImpact(bsts.model = model_886$model,
                                 post.period.response = model_886$UnStandardize(as.numeric(model_886$post.period.response)),
                                 UnStandardize = model_886$UnStandardize)
plot(model_886_impact) # negative not significant impact

p <- plot(model_839_impact)
p <- p + labs(title = "Impact of the online campaign in 60 days on Hindi Wikipedia external referred pageviews from Madhya Pradesh",
              subtitle = "Dashed line represents the start date of the online campaign on YouTube and Facebook, April 3rd")
ggsave("hiwiki_external_pv_online_impact.png", plot = p, path = 'figures', units = "in", dpi = 300, height = 9, width = 18)
save(model_839_impact, file = "data/hiwiki_external_pv_online_impact.RData")


# Not use custom model
train_start <- as.Date("2017-10-01")
train_data <- data.frame(
  y = filter(y, date >= train_start & date <= test_end) %>% select(-date) %>% unlist(),
  filter(x, date >= train_start & date <= test_end)[, match(unlist(selected_controls["best_all"]), colnames(x))]
) %>%
  xts::xts(order.by = seq.Date(train_start, test_end, "day"))
impact <- CausalImpact(train_data, c(train_start, train_end), c(test_start, test_end),
                       model.args = list(niter = n_iters, nseasons = 12, season.duration = 30))
plot(impact) # significantly negative impact
