library(tidyverse)
library(magrittr)

load("data/pageviews.RData")
load("data/hiwiki_main_pv.RData")
load("data/unique_devices.RData")
mp_gtrend <- read_csv(file = "data/Madhya_Pradesh_google_trends.csv", col_types = "Di")
india_gtrend <- read_csv(file = "data/india_google_trends.csv", col_types = "Di")
gsc_m_hiwiki_country <- read_csv(file = "data/gsc_m_hiwiki_country.csv", col_types = "Dciidd")
gsc_hiwiki_country <- read_csv(file = "data/gsc_hiwiki_country.csv", col_types = "Dciidd")

seed <- 2018
online_start <- as.Date("2018-04-03")
online_end <- as.Date("2018-04-23")
tv_start <- as.Date("2018-05-27")

pageviews %<>%
  mutate(
    date = lubridate::ymd(date),
    access_method = ifelse(grepl('^mobile', access_method), 'mobile', 'desktop'),
    referer_class = case_when(
      grepl('^external', referer_class) ~ 'external',
      referer_class == 'none' ~ 'direct',
      TRUE ~ referer_class
      )
    )

hiwiki_main_pv %<>%
   mutate(
    date = lubridate::ymd(date),
    access_method = ifelse(grepl('^mobile', access_method), 'mobile', 'desktop'),
    referer_class = case_when(
      grepl('^external', referer_class) ~ 'external',
      referer_class == 'none' ~ 'direct',
      TRUE ~ referer_class
      )
    )

unique_devices %<>%
  mutate(
    date = lubridate::ymd(date),
    access_method = case_when(
      grepl('^[a-z]+\\.m\\..+', domain) ~ "mobile",
      grepl('^m\\..+', domain) ~ "mobile",
      TRUE ~ "desktop"
    ),
    domain = gsub("(m\\.)|(\\.org$)", "", domain)
  ) %>%
  rename(total = uniques_estimate, first_visit = uniques_offset,
         return = uniques_underestimate, project = domain) %>%
  gather(key = type, value = uniques, total, first_visit, return)

gsc_hiwiki <- rbind(
  gsc_m_hiwiki_country %>% mutate(access_method = "mobile"),
  gsc_hiwiki_country %>% mutate(access_method = "desktop")
) %>%
  mutate(
    date = lubridate::ymd(date)
  )


internet_users <- data.frame(
  date = as.Date(c("2015-06-30", "2015-09-30", "2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2017-03-31", "2017-06-30", "2017-09-30", "2017-12-31", "2018-03-31", "2018-06-30", "2018-09-30")),
  internet_users = c(319.42, 324.95, 331.66, 342.65, 350.48, 367.48, 391.50, 422.19, 431.21, 429.23, 445.96, 493.96, 512.26, 560.01)
) %>%
  complete(date = seq.Date(min(date), max(date), by="day"))
internet_users$internet_users <- imputeTS::na.interpolation(internet_users$internet_users)
internet_users_mp <- data.frame(
  date = as.Date(c("2015-06-30", "2015-09-30", "2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2017-03-31", "2017-06-30", "2017-09-30", "2017-12-31", "2018-03-31", "2018-06-30", "2018-09-30")),
  internet_users = c(18.16, 18.91, 19.46, 20.42, 20.98, 22.24, 23.49, 24.77, 23.82, 21.83, 23.18, 25.89, 29.02, 35.41)
) %>%
  complete(date = seq.Date(min(date), max(date), by="day"))
internet_users_mp$internet_users <- imputeTS::na.interpolation(internet_users_mp$internet_users)
# In millions. Source: Telecom Regulatory Authority of India
# https://www.trai.gov.in/release-publication/reports/performance-indicators-reports


Diwali <- bsts::DateRangeHoliday(
  "Diwali",
  start = as.Date(c("2015-11-10", "2016-10-29", "2017-10-18")), # "2018-11-06"
  end = as.Date(c("2015-11-12", "2016-10-31", "2017-10-20")) # "2018-11-08"
  )
Raksha_Bandhan <- bsts::DateRangeHoliday(
  "RakshaBandhan",
  start = as.Date(c("2015-08-29", "2016-08-18", "2017-08-07")), # "2018-08-26"
  end = as.Date(c("2015-08-29", "2016-08-18", "2017-08-07")) # "2018-08-26"
  )
Holi <- bsts::DateRangeHoliday(
  "Holi",
  start = as.Date(c("2016-03-23", "2017-03-12", "2018-03-01")),
  end = as.Date(c("2016-03-24", "2017-03-13", "2018-03-02"))
  )
Dussehra <- bsts::DateRangeHoliday(
  "Dussehra",
  start = as.Date(c("2015-10-22", "2016-10-11", "2017-09-30")), # "2018-10-19"
  end = as.Date(c("2015-10-22", "2016-10-11", "2017-09-30")) # "2018-10-19"
  )
Newyear <- bsts::DateRangeHoliday(
  "NewYear",
  start = as.Date(c("2015-12-31", "2016-12-31", "2017-12-31")),
  end = as.Date(c("2016-01-01", "2017-01-01", "2018-01-01"))
  )
