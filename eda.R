source("refine.R")

# Online campaign

## main page pageviews

p1 <- hiwiki_main_pv %>%
  filter(
    referer_class != 'unknown',
    date >= as.Date('2018-03-01'),
    date < as.Date('2018-07-01')
  ) %>%
  mutate(subdivision = ifelse(subdivision == 'Madhya Pradesh', 'Madhya Pradesh', 'Other states')) %>%
  group_by(subdivision, access_method, referer_class, date) %>%
  summarize(pageviews = sum(pageviews)) %>%
  ggplot(aes(x = date, y = pageviews, color = referer_class)) +
  geom_line() +
  facet_grid(subdivision ~ access_method, switch = "y", scales = 'free') +
  scale_y_continuous("Pageviews", labels = polloi::compress) +
  scale_color_brewer("Referrer Class", palette = "Set1", guide = guide_legend(nrow = 1)) +
  scale_x_date(name = "Date") +
  geom_vline(xintercept = as.numeric(as.Date("2018-04-03")), linetype = "dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2018-04-23")), linetype = "dashed") +
  wmf::theme_facet(base_size = 12) +
  labs(title = "Hindi Wikipedia main page pageviews from India, March - June 2018",
       subtitle = "Dashed line represents the start and end of the online campaign on YouTube and Facebook, April 3rd - 23rd"
       )
p2 <- hiwiki_main_pv %>%
  filter(
    referer_class != 'unknown',
    date >= as.Date('2018-03-01'),
    date < as.Date('2018-07-01')
  ) %>%
  mutate(subdivision = ifelse(subdivision == 'Madhya Pradesh', 'Madhya Pradesh', 'Other states')) %>%
  group_by(subdivision, access_method, referer_class, date) %>%
  summarize(pageviews = sum(pageviews)) %>%
  group_by(subdivision, access_method, referer_class) %>%
  mutate(pageviews = scale(pageviews)) %>%
  ggplot(aes(x = date, y = pageviews, color = referer_class)) +
  geom_line() +
  facet_grid(subdivision ~ access_method, switch = "y", scales = 'free') +
  scale_y_continuous("Pageviews", labels = polloi::compress) +
  scale_color_brewer("Referrer Class", palette = "Set1", guide = guide_legend(nrow = 1)) +
  scale_x_date(name = "Date") +
  geom_vline(xintercept = as.numeric(as.Date("2018-04-03")), linetype = "dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2018-04-23")), linetype = "dashed") +
  wmf::theme_facet(base_size = 12) +
  labs(title = "Normalized Hindi Wikipedia main page pageviews from India, March - June 2018",
       subtitle = "Dashed line represents the start and end of the online campaign on YouTube and Facebook, April 3rd - 23rd"
       )
ggsave("hiwiki_main_pv_2018.png", plot = cowplot::plot_grid(p1, p2), path = 'figures', units = "in", dpi = 300, height = 9, width = 18)
# direct and external pv from MP increase a lot, but what we care about is whether users also look at other pages and the long term effect
# external pv from other state also increase too


## hiwiki pv from Madhya Pradesh by year
p <- pageviews %>%
  filter(
    country_code == 'IN',
    subdivision == 'Madhya Pradesh',
    project == 'hi.wikipedia'
  ) %>%
  group_by(year, date) %>%
  summarize(pageviews = sum(pageviews)) %>%
  ggplot(aes(x=as.Date(lubridate::yday(date), "1969-12-31"), y=pageviews, colour=factor(year))) +
  geom_line(size=1.2) +
  scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%b %d") +
  scale_y_continuous(labels=polloi::compress, name = "Pageviews") +
  scale_color_brewer("Year", palette = "Set1") +
  geom_vline(xintercept = as.numeric(as.Date("1970-04-03")),
             linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("1970-04-23")),
             linetype = "dashed", color = "black") +
  annotate("text", x = as.Date("1970-04-01"), y = 50000, label = "video campaign start", angle = 90) +
  annotate("text", x = as.Date("1970-04-25"), y = 50000, label = "video campaign end", angle = 90) +
  ggtitle("Hindi Wikipedia pageviews from Madhya Pradesh, by year") +
  wmf::theme_min(base_size = 15)
ggsave("hiwiki_mp_pv.png", p, path = 'figures', units = "in", dpi = 300, height = 6, width = 10)

## all pageviews
p1 <- pageviews %>%
  filter(
    country_code == 'IN',
    subdivision == 'Madhya Pradesh',
    referer_class != 'unknown',
    date >= as.Date('2018-03-01'),
    date < as.Date('2018-07-01')
  ) %>%
  mutate(project = case_when(
      project %in% c('en.wikipedia', 'hi.wikipedia') ~ project,
      TRUE ~ 'other wikis'
    )) %>%
  group_by(project, access_method, referer_class, date) %>%
  summarize(pageviews = sum(pageviews)) %>%
  ggplot(aes(x = date, y = pageviews, color = referer_class)) +
  geom_line() +
  facet_grid(project ~ access_method, switch = "y", scales = 'free') +
  scale_y_continuous("Pageviews", labels = polloi::compress) +
  scale_color_brewer("Referrer Class", palette = "Set1", guide = guide_legend(nrow = 1)) +
  scale_x_date(name = "Date") +
  geom_vline(xintercept = as.numeric(as.Date("2018-04-03")), linetype = "dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2018-04-23")), linetype = "dashed") +
  wmf::theme_facet(base_size = 12) +
  labs(title = "Pageviews from Madhya Pradesh, India, March - June 2018",
       subtitle = "Dashed line represents the start and end of the online campaign on YouTube and Facebook, April 3rd - 23rd",
       caption = "Other wikis include top 10 projects in India/Madhya Pradesh with the most pageviews, and other major Indian languages Wikipedias"
       )
p2 <- pageviews %>%
  filter(
    country_code == 'IN',
    subdivision == 'Madhya Pradesh',
    referer_class != 'unknown',
    date >= as.Date('2018-03-01'),
    date < as.Date('2018-07-01')
  ) %>%
  mutate(project = case_when(
      project %in% c('en.wikipedia', 'hi.wikipedia') ~ project,
      TRUE ~ 'other wikis'
    )) %>%
  group_by(project, access_method, referer_class, date) %>%
  summarize(pageviews = sum(pageviews)) %>%
  group_by(project, access_method, referer_class) %>%
  mutate(pageviews = scale(pageviews)) %>%
  ggplot(aes(x = date, y = pageviews, color = referer_class)) +
  geom_line() +
  facet_grid(project ~ access_method, switch = "y", scales = 'free') +
  scale_y_continuous("Pageviews", labels = polloi::compress) +
  scale_color_brewer("Referrer Class", palette = "Set1", guide = guide_legend(nrow = 1)) +
  scale_x_date(name = "Date") +
  geom_vline(xintercept = as.numeric(as.Date("2018-04-03")), linetype = "dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2018-04-23")), linetype = "dashed") +
  wmf::theme_facet(base_size = 12) +
  labs(title = "Normalized pageviews from Madhya Pradesh, India, March - June 2018",
       subtitle = "Dashed line represents the start and end of the online campaign on YouTube and Facebook, April 3rd - 23rd",
       caption = "Other wikis include top 10 projects in India/Madhya Pradesh with the most pageviews, and other major Indian languages Wikipedias"
       )
ggsave("pv_mp_bywiki_2018.png", plot = cowplot::plot_grid(p1, p2), path = 'figures', units = "in", dpi = 300, height = 9, width = 18)


# Google trends
p <- mp_gtrend %>%
  rename("Madhya Pradesh" = "Interest") %>%
  inner_join(india_gtrend %>% rename("India" = "Interest"), by = "Week") %>%
  gather(key = Region, value = Interest, -Week) %>%
  filter(Week >= as.Date("2016-1-1")) %>%
  ggplot(aes(x = Week, y = Interest, color = Region)) +
  geom_line(size = 1.2) +
  scale_color_brewer("Region", palette = "Set1", guide = guide_legend(nrow = 1)) +
  scale_x_date(name = "Date", date_breaks = "6 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.numeric(as.Date("2018-04-03")),
             linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2018-05-27")),
             linetype = "dashed", color = "black") +
  annotate("text", x = as.Date("2018-03-24"), y = 35, label = "online campaign start", angle = 90) +
  annotate("text", x = as.Date("2018-06-05"), y = 35, label = "TV campaign start", angle = 90) +
  labs(title = "Weekly Google trends of the search term 'wikipedia' in English and Hindi",
       subtitle = "Interest numbers are normalized by the past 5 years data") +
  wmf::theme_min(base_size = 15)
ggsave("google_trends.png", p, path = 'figures', units = "in", dpi = 300, height = 6, width = 10)


# TV campaign

## hiwiki pv from India by year
p <- pageviews %>%
  filter(
    country_code == 'IN',
    project == 'hi.wikipedia'
  ) %>%
  group_by(year, date) %>%
  summarize(pageviews = sum(pageviews)) %>%
  ggplot(aes(x=as.Date(lubridate::yday(date), "1969-12-31"), y=pageviews, colour=factor(year))) +
  geom_line(size=1.2) +
  scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%b %d") +
  scale_y_continuous(labels=polloi::compress, name = "Pageviews") +
  scale_color_brewer("Year", palette = "Set1") +
  geom_vline(xintercept = as.numeric(as.Date("1970-05-27")),
             linetype = "dashed", color = "black") +
  annotate("text", x = as.Date("1970-05-24"), y = 5e5, label = "TV campaign", angle = 90) +
  ggtitle("Hindi Wikipedia pageviews from India, by year") +
  wmf::theme_min(base_size = 15)
ggsave("hiwiki_india_pv.png", p, path = 'figures', units = "in", dpi = 300, height = 6, width = 10)

## hiwiki pv from India, compare to other wikis' traffic from India
p1 <- pageviews %>%
  filter(
    country_code == 'IN',
    referer_class != 'unknown',
    date >= as.Date('2018-04-01'),
    date < as.Date('2018-08-01')
  ) %>%
  mutate(project = case_when(
      project %in% c('en.wikipedia', 'hi.wikipedia') ~ project,
      TRUE ~ 'other wikis'
    )) %>%
  group_by(project, access_method, referer_class, date) %>%
  summarize(pageviews = sum(pageviews)) %>%
  ggplot(aes(x = date, y = pageviews, color = referer_class)) +
  geom_line() +
  facet_grid(project ~ access_method, switch = "y", scales = 'free') +
  scale_y_continuous("Pageviews", labels = polloi::compress) +
  scale_color_brewer("Referrer Class", palette = "Set1", guide = guide_legend(nrow = 1)) +
  scale_x_date(name = "Date") +
  geom_vline(xintercept = as.numeric(as.Date("2018-05-27")), linetype = "dashed") +
  wmf::theme_facet(base_size = 12) +
  labs(title = "Pageviews from India, April - July 2018",
       subtitle = "Dashed line represents the date of the TV campaign, May 27th",
       caption = "Other wikis include top 10 projects in India/Madhya Pradesh with the most pageviews, and other major Indian languages Wikipedias"
       )
p2 <- pageviews %>%
  filter(
    country_code == 'IN',
    referer_class != 'unknown',
    date >= as.Date('2018-04-01'),
    date < as.Date('2018-08-01')
  ) %>%
  mutate(project = case_when(
      project %in% c('en.wikipedia', 'hi.wikipedia') ~ project,
      TRUE ~ 'other wikis'
    )) %>%
  group_by(project, access_method, referer_class, date) %>%
  summarize(pageviews = sum(pageviews)) %>%
  group_by(project, access_method, referer_class) %>%
  mutate(pageviews = scale(pageviews)) %>%
  ggplot(aes(x = date, y = pageviews, color = referer_class)) +
  geom_line() +
  facet_grid(project ~ access_method, switch = "y", scales = 'free') +
  scale_y_continuous("Pageviews", labels = polloi::compress) +
  scale_color_brewer("Referrer Class", palette = "Set1", guide = guide_legend(nrow = 1)) +
  scale_x_date(name = "Date") +
  geom_vline(xintercept = as.numeric(as.Date("2018-05-27")), linetype = "dashed") +
  wmf::theme_facet(base_size = 12) +
  labs(title = "Normalized pageviews from India, April - July 2018",
       subtitle = "Dashed line represents the date of the TV campaign, May 27th",
       caption = "Other wikis include top 10 projects in India/Madhya Pradesh with the most pageviews, and other major Indian languages Wikipedias"
       )
ggsave("pv_india_bywiki_2018.png", plot = cowplot::plot_grid(p1, p2), path = 'figures', units = "in", dpi = 300, height = 9, width = 18)

## hiwiki pv from India compare to other countries
p1 <- pageviews %>%
  filter(
    project == 'hi.wikipedia',
    referer_class != 'unknown',
    date >= as.Date('2018-04-01'),
    date < as.Date('2018-08-01')
  ) %>%
  mutate(country = ifelse(country %in% c('India', 'United States'), country, 'Other countries')) %>%
  group_by(country, access_method, referer_class, date) %>%
  summarize(pageviews = sum(pageviews)) %>%
  ggplot(aes(x = date, y = pageviews, color = referer_class)) +
  geom_line() +
  facet_grid(country ~ access_method, switch = "y", scales = 'free') +
  scale_y_continuous("Pageviews", labels = polloi::compress) +
  scale_color_brewer("Referrer Class", palette = "Set1", guide = guide_legend(nrow = 1)) +
  scale_x_date(name = "Date") +
  geom_vline(xintercept = as.numeric(as.Date("2018-05-27")), linetype = "dashed") +
  wmf::theme_facet(base_size = 12) +
  labs(title = "Hindi Wikipedia pageviews by countries, April - July 2018",
       subtitle = "Dashed line represents the date of the TV campaign, May 27th"
       )
p2 <- pageviews %>%
  filter(
    project == 'hi.wikipedia',
    referer_class != 'unknown',
    date >= as.Date('2018-04-01'),
    date < as.Date('2018-08-01')
  ) %>%
  mutate(country = ifelse(country %in% c('India', 'United States'), country, 'Other countries')) %>%
  group_by(country, access_method, referer_class, date) %>%
  summarize(pageviews = sum(pageviews)) %>%
  group_by(country, access_method, referer_class) %>%
  mutate(pageviews = scale(pageviews)) %>%
  ggplot(aes(x = date, y = pageviews, color = referer_class)) +
  geom_line() +
  facet_grid(country ~ access_method, switch = "y", scales = 'free') +
  scale_y_continuous("Pageviews", labels = polloi::compress) +
  scale_color_brewer("Referrer Class", palette = "Set1", guide = guide_legend(nrow = 1)) +
  scale_x_date(name = "Date") +
  geom_vline(xintercept = as.numeric(as.Date("2018-05-27")), linetype = "dashed") +
  wmf::theme_facet(base_size = 12) +
  labs(title = "Normalized Hindi Wikipedia pageviews by countries, April - July 2018",
       subtitle = "Dashed line represents the date of the TV campaign, May 27th"
       )
ggsave("hiwiki_pv_bycountries_2018.png", plot = cowplot::plot_grid(p1, p2), path = 'figures', units = "in", dpi = 300, height = 9, width = 18)


## hiwiki ud from India by year (grid by uniques_underestimate and uniques_offset)
p <- unique_devices %>%
  filter(
    country_code == 'IN',
    project == 'hi.wikipedia',
    type != 'total'
  ) %>%
  group_by(year, date, type) %>%
  summarize(uniques = sum(uniques)) %>%
  ggplot(aes(x=as.Date(lubridate::yday(date), "1969-12-31"), y=uniques, colour=factor(year))) +
  geom_line(size=1.2) +
  scale_x_date(name = "Date", date_breaks = "3 months", date_labels = "%b %d") +
  scale_y_continuous(labels=polloi::compress, name = "Unique devices") +
  scale_color_brewer("Year", palette = "Set1") +
  geom_vline(xintercept = as.numeric(as.Date("1970-05-27")),
             linetype = "dashed", color = "black") +
  annotate("text", x = as.Date("1970-05-22"), y = 1e5, label = "TV campaign", angle = 90) +
  facet_wrap(~type, scales = "free") +
  labs(title = "Hindi Wikipedia unique devices from India, by year",
       caption = "First visit represents devices coming from 1-hit session without cookies; return represents devices that have looked at wikipedia before.") +
  wmf::theme_facet(base_size = 15)
ggsave("hiwiki_india_ud.png", p, path = 'figures', units = "in", dpi = 300, height = 6, width = 12)

## hiwiki ud from India, compare to other wikis' ud from India (grid by uniques_underestimate and uniques_offset)
p1 <- unique_devices %>%
  filter(
    country_code == 'IN',
    type != 'total',
    date >= as.Date('2018-04-01'),
    date < as.Date('2018-08-01')
  ) %>%
  mutate(project = case_when(
      project %in% c('en.wikipedia', 'hi.wikipedia') ~ project,
      TRUE ~ 'other wikis'
    )) %>%
  group_by(project, access_method, type, date) %>%
  summarize(uniques = sum(uniques)) %>%
  ggplot(aes(x = date, y = uniques, color = type)) +
  geom_line() +
  facet_grid(project ~ access_method, switch = "y", scales = 'free') +
  scale_y_continuous("Unique Devices", labels = polloi::compress) +
  scale_color_brewer("Type of visits", palette = "Set1", guide = guide_legend(nrow = 1)) +
  scale_x_date(name = "Date") +
  geom_vline(xintercept = as.numeric(as.Date("2018-05-27")), linetype = "dashed") +
  wmf::theme_facet(base_size = 12) +
  labs(title = "Unique devices from India, April - July 2018",
       subtitle = "Dashed line represents the date of the TV campaign, May 27th",
       caption = "Other wikis include top 10 projects in India/Madhya Pradesh with the most pageviews, and other major Indian languages Wikipedias"
       )
p2 <- unique_devices %>%
  filter(
    country_code == 'IN',
    type != 'total',
    date >= as.Date('2018-04-01'),
    date < as.Date('2018-08-01')
  ) %>%
  mutate(project = case_when(
      project %in% c('en.wikipedia', 'hi.wikipedia') ~ project,
      TRUE ~ 'other wikis'
    )) %>%
  group_by(project, access_method, type, date) %>%
  summarize(uniques = sum(uniques)) %>%
  group_by(project, access_method, type) %>%
  mutate(uniques = scale(uniques)) %>%
  ggplot(aes(x = date, y = uniques, color = type)) +
  geom_line() +
  facet_grid(project ~ access_method, switch = "y", scales = 'free') +
  scale_y_continuous("Unique Devices", labels = polloi::compress) +
  scale_color_brewer("Type of visits", palette = "Set1", guide = guide_legend(nrow = 1)) +
  scale_x_date(name = "Date") +
  geom_vline(xintercept = as.numeric(as.Date("2018-05-27")), linetype = "dashed") +
  wmf::theme_facet(base_size = 12) +
  labs(title = "Normalized unique devices from India, April - July 2018",
       subtitle = "Dashed line represents the date of the TV campaign, May 27th",
       caption = "Other wikis include top 10 projects in India/Madhya Pradesh with the most pageviews, and other major Indian languages Wikipedias"
       )
ggsave("ud_india_bywiki_2018.png", plot = cowplot::plot_grid(p1, p2), path = 'figures', units = "in", dpi = 300, height = 9, width = 18)

## hiwiki ud from India compare to other countries (grid by uniques_underestimate and uniques_offset)
p1 <- unique_devices %>%
  filter(
    project == 'hi.wikipedia',
    type != 'total',
    date >= as.Date('2018-04-01'),
    date < as.Date('2018-08-01')
  ) %>%
  mutate(country = ifelse(country %in% c('India', 'United States'), country, 'Other countries')) %>%
  group_by(country, access_method, type, date) %>%
  summarize(uniques = sum(uniques)) %>%
  ggplot(aes(x = date, y = uniques, color = type)) +
  geom_line() +
  facet_grid(country ~ access_method, switch = "y", scales = 'free') +
  scale_y_continuous("Unique Devices", labels = polloi::compress) +
  scale_color_brewer("Type of visits", palette = "Set1", guide = guide_legend(nrow = 1)) +
  scale_x_date(name = "Date") +
  geom_vline(xintercept = as.numeric(as.Date("2018-05-27")), linetype = "dashed") +
  wmf::theme_facet(base_size = 12) +
  labs(title = "Hindi Wikipedia unique devices by countries, April - July 2018",
       subtitle = "Dashed line represents the date of the TV campaign, May 27th"
       )
p2 <- unique_devices %>%
  filter(
    project == 'hi.wikipedia',
    type != 'total',
    date >= as.Date('2018-04-01'),
    date < as.Date('2018-08-01')
  ) %>%
  mutate(country = ifelse(country %in% c('India', 'United States'), country, 'Other countries')) %>%
  group_by(country, access_method, type, date) %>%
  summarize(uniques = sum(uniques)) %>%
  group_by(country, access_method, type) %>%
  mutate(uniques = scale(uniques)) %>%
  ggplot(aes(x = date, y = uniques, color = type)) +
  geom_line() +
  facet_grid(country ~ access_method, switch = "y", scales = 'free') +
  scale_y_continuous("Unique Devices", labels = polloi::compress) +
  scale_color_brewer("Type of visits", palette = "Set1", guide = guide_legend(nrow = 1)) +
  scale_x_date(name = "Date") +
  geom_vline(xintercept = as.numeric(as.Date("2018-05-27")), linetype = "dashed") +
  wmf::theme_facet(base_size = 12) +
  labs(title = "Normalized Hindi Wikipedia unique devices by countries, April - July 2018",
       subtitle = "Dashed line represents the date of the TV campaign, May 27th"
       )
ggsave("hiwiki_ud_bycountries_2018.png", plot = cowplot::plot_grid(p1, p2), path = 'figures', units = "in", dpi = 300, height = 9, width = 18)

## Google search console impression/clicks to hi.m.wikipedia from India
p <- gsc_hiwiki %>%
  select(-ctr, -position) %>%
  mutate(country = ifelse(country == "IND", "India", "Other countries")) %>%
  group_by(access_method, country, date) %>%
  summarize_all(sum) %>%
  gather(key = action, value = counts, clicks, impressions) %>%
  filter(date >= as.Date("2018-03-01"), date < as.Date("2018-08-01")) %>%
  ggplot(aes(x = date, y = counts, color = country)) +
  geom_line() +
  facet_grid(action ~ access_method, switch = "y", scales = 'free') +
  scale_y_continuous("Counts", labels = polloi::compress) +
  scale_color_brewer("Country", palette = "Set1", guide = guide_legend(nrow = 1)) +
  scale_x_date(name = "Date") +
  geom_vline(xintercept = as.numeric(as.Date("2018-04-03")), linetype = "dashed", color = "green") +
  geom_vline(xintercept = as.numeric(as.Date("2018-05-27")), linetype = "dashed", color = "black") +
  wmf::theme_facet(base_size = 12) +
  labs(title = "Google search impressions and clicks to Hindi Wikipedia, March - July 2018",
       subtitle = "Green dashed line represents the start of the online campaign, April 3rd; black dashed line represents the date of the TV campaign, May 27th"
       )
ggsave("gsc_hiwiki.png", plot = p, path = 'figures', units = "in", dpi = 300, height = 6, width = 12)
