# Remote on stat1005:

# Pageviews to major Indian languages wikis and most traffic wikis from multiple countries
# spark2R --master yarn --executor-memory 2G --executor-cores 1 --driver-memory 4G
start_date <- as.Date("2015-05-01")
end_date <- Sys.Date() - 1
pageviews <- do.call(rbind, lapply(seq(start_date, end_date, "day"), function(date) {
  cat("Fetching pageviews from ", as.character(date), "\n")
  clause_data <- wmf::date_clause(date)
  query <- paste("
      SELECT CONCAT(year, '-', LPAD(month, 2, '0'), '-', LPAD(day, 2, '0')) AS date,
      year, month, day,
      country_code, country,
      subdivision,
      project,
      access_method,
      referer_class,
      SUM(view_count) AS pageviews
      FROM wmf.pageview_hourly",
      clause_data$date_clause,
      "AND project IN ('en.wikipedia', 'hi.wikipedia', 'commons.wikimedia', 'ru.wikipedia', 'mr.wikipedia', 'ta.wikipedia', 'en.wiktionary', 'simple.wikipedia',
      'te.wikipedia', 'ml.wikipedia', 'hi.wikibooks', 'en.wikibooks', 'wikisource', 'hi.wiktionary', 'bn.wikipedia', 'gu.wikipedia', 'ur.wikipedia', 'kn.wikipedia')
      -- above include top 10 project in India/Madhya Pradesh with the most pv, and major languages (more than 4% in India/Madhya Pradesh) wikipedias
      AND country_code IN ('IN', 'US', 'PK', 'LK', 'MY', 'SG', 'BD', 'NP')
      -- above include top N countries with most pv to hiwiki, and countries whose official languages overlap with India
      -- see https://en.wikipedia.org/wiki/List_of_languages_by_the_number_of_countries_in_which_they_are_recognized_as_an_official_language
      AND agent_type = 'user'
      GROUP BY year, month, day, country_code, country, project, access_method, referer_class, subdivision")
  results <- tryCatch(
  suppressMessages(collect(sql(query))),
  error = function(e) {
    return(data.frame(
      date = date,
      year = integer(),
      month = integer(),
      day = integer(),
      country_code = character(),
      country = character(),
      subdivision= character(),
      project= character(),
      access_method= character(),
      referer_class= character(),
      pageviews = numeric()
    ))
  })
  return(results)
}))
save(pageviews, file = "data/india_campaign/pageviews.RData")

# hiwiki main page pageviews from Madhya Pradesh
# spark2R --master yarn --executor-memory 2G --executor-cores 1 --driver-memory 4G
start_date <- as.Date("2015-05-01")
end_date <- Sys.Date() - 1
hiwiki_main_pv <- do.call(rbind, lapply(seq(start_date, end_date, "day"), function(date) {
  cat("Fetching main page pageviews from ", as.character(date), "\n")
  clause_data <- wmf::date_clause(date)
  query <- paste("
      SELECT CONCAT(year, '-', LPAD(month, 2, '0'), '-', LPAD(day, 2, '0')) AS date,
      year, month, day,
      country_code, country,
      subdivision,
      project,
      access_method,
      referer_class,
      SUM(view_count) AS pageviews
      FROM wmf.pageview_hourly",
      clause_data$date_clause,
      "AND project = 'hi.wikipedia'
      AND country_code = 'IN'
      AND agent_type = 'user'
      AND namespace_id = 0
      AND page_id = 220108
      GROUP BY year, month, day, country_code, country, subdivision, project, access_method, referer_class")
  results <- tryCatch(
  suppressMessages(collect(sql(query))),
  error = function(e) {
    return(data.frame(
      date = date,
      year = integer(),
      month = integer(),
      day = integer(),
      country_code = character(),
      country = character(),
      subdivision= character(),
      project= character(),
      access_method= character(),
      referer_class= character(),
      pageviews = numeric()
    ))
  })
  return(results)
}))
save(hiwiki_main_pv, file = "data/india_campaign/hiwiki_main_pv.RData")

# Unique devices
query <- "
USE wmf;
SELECT CONCAT(year, '-', LPAD(month, 2, '0'), '-', LPAD(day, 2, '0')) AS date,
year, month, day,
country_code, country,
domain,
SUM(uniques_estimate) AS uniques_estimate,
SUM(uniques_underestimate) AS uniques_underestimate,
SUM(uniques_offset) AS uniques_offset
FROM unique_devices_per_domain_daily
WHERE domain IN ('en.wikipedia.org', 'hi.wikipedia.org', 'commons.wikimedia.org', 'ru.wikipedia.org', 'mr.wikipedia.org', 'ta.wikipedia.org', 'en.wiktionary.org', 'simple.wikipedia.org',
'te.wikipedia.org', 'ml.wikipedia.org', 'hi.wikibooks.org', 'en.wikibooks.org', 'wikisource.org', 'hi.wiktionary.org', 'bn.wikipedia.org', 'gu.wikipedia.org', 'ur.wikipedia.org', 'kn.wikipedia.org',
'en.m.wikipedia.org', 'hi.m.wikipedia.org', 'commons.m.wikimedia.org', 'ru.m.wikipedia.org', 'mr.m.wikipedia.org', 'ta.m.wikipedia.org', 'en.m.wiktionary.org', 'simple.m.wikipedia.org',
'te.m.wikipedia.org', 'ml.m.wikipedia.org', 'hi.m.wikibooks.org', 'en.m.wikibooks.org', 'm.wikisource.org', 'hi.m.wiktionary.org', 'bn.m.wikipedia.org', 'gu.m.wikipedia.org', 'ur.m.wikipedia.org', 'kn.m.wikipedia.org')
-- above include top 10 project in India/Madhya Pradesh with the most pv, and major languages (more than 4% in India/Madhya Pradesh) wikipedias
AND country_code IN ('IN', 'US', 'PK', 'LK', 'MY', 'SG', 'BD', 'NP')
-- above include top N countries with most pv to hiwiki, and countries whose official languages overlap with India
-- see https://en.wikipedia.org/wiki/List_of_languages_by_the_number_of_countries_in_which_they_are_recognized_as_an_official_language
AND year >= 2015
GROUP BY year, month, day, country_code, country, domain;
"
unique_devices <- wmf::query_hive(query)
save(unique_devices, file = "data/india_campaign/unique_devices.RData")


# Local:
system("scp chelsyx@stat5:~/data/india_campaign/pageviews.RData data/")
system("scp chelsyx@stat5:~/data/india_campaign/hiwiki_main_pv.RData data/")
system("scp chelsyx@stat5:~/data/india_campaign/unique_devices.RData data/")


# Google trends
# search term: hindi wikipedia + wikipedia + विकिपीडिया + vikipeediya

# Google search console data
# stat1006:/home/bearloga/gsc/output/https/wikipedia/hi.m.wikipedia.org/
# stat1006:/home/bearloga/gsc/output/https/wikipedia/hi.wikipedia.org/
