Standardize <- function(y, fit.range = NULL) {
  # This function is copied from CausalImpact package
  y.fit <- y[fit.range[1] : fit.range[2]]
  y.mu <- mean(y.fit, na.rm = TRUE)
  if (is.nan(y.mu)) {
    y.mu <- NA_real_
  }
  y.sd <- sd(y.fit, na.rm = TRUE)
  y <- y - y.mu
  if (!is.na(y.sd) && (y.sd > 0)) {
    y <- y / y.sd
  }
  UnStandardize <- function(y) {
    if (!is.na(y.sd) && (y.sd > 0)) {
      y <- y * y.sd
    }
    y <- y + y.mu
    return(y)
  }
  return(list(y = y, UnStandardize = UnStandardize))
}

StandardizeAllVariables <- function(data, fit.range = NULL) {
  # This function is copied from CausalImpact package
  if (!is.null(ncol(data))) {
    for (j in ncol(data) : 1) {
      tmp <- Standardize(data[, j], fit.range)
      data[, j] <- tmp$y
      UnStandardize <- tmp$UnStandardize
    }
  } else {
    tmp <- Standardize(data, fit.range)
    data <- tmp$y
    UnStandardize <- tmp$UnStandardize
  }
  return(list(data = data, UnStandardize = UnStandardize))
}

control_candidates <- function(test, control, match_period_start, match_period_end, n_candidates){
  distances <- data.frame(matrix(nrow=ncol(control)-1, ncol=4))
  names(distances) <- c('test', 'control', 'relative_distance', 'correlation')
  distances$test <- setdiff(colnames(test), "date")

  test <- filter(test, date >= match_period_start & date <= match_period_end) %>% select(-date) %>% unlist()
  control <- filter(control, date >= match_period_start & date <= match_period_end) %>% select(-date)
  for (i in 1:ncol(control)) {
    distances$control[i] <- colnames(control)[i]
    if (var(control[, i]) == 0)  next
    distances$relative_distance[i] <- dtw(test, control[, i], window.type=sakoeChibaWindow, window.size=1)$normalizedDistance
    distances$correlation[i] <- cor(test, control[, i])
  }

  # output selected control series
  dtw_all <- arrange(distances, relative_distance) %>%
    head(n_candidates) %>%
    .$control
  dtw_hiwiki <- filter(distances, grepl("_hi.wikipedia$", control)) %>%
    arrange(relative_distance) %>%
    head(n_candidates) %>%
    .$control
  corr_all <- arrange(distances, desc(abs(correlation))) %>%
    head(n_candidates) %>%
    .$control
  corr_hiwiki <- filter(distances, grepl("_hi.wikipedia$", control)) %>%
    arrange(desc(abs(correlation)))%>%
    head(n_candidates) %>%
    .$control

  return(list(dtw_all = dtw_all, dtw_hiwiki = dtw_hiwiki, corr_all = corr_all, corr_hiwiki = corr_hiwiki))
}
