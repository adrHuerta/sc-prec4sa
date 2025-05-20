
apply_break_detection <- function(time_series) {

  n_stations <- suppressWarnings(ncol(time_series[, -1]))

  if (n_stations < 4) {

    time_serie_samples <- time_series[, 1]
    # no preprocessing_clean(time_serie = time_serie_samples)
    time_serie_samples <- break_detection_tests(time_serie = time_serie_samples)

  } else {

    time_serie_samples <- time_series
    time_serie_samples <- lapply(
      time_serie_samples[, -1],
      function(x) (time_serie_samples[, 1] - x)
    )
    time_serie_samples <- do.call(cbind, time_serie_samples)
    # no preprocessing_clean(time_serie = x)
    # no  do.call(cbind, time_serie_samples)
    time_serie_samples <- lapply(
      time_serie_samples,
      function(x) break_detection_tests(time_serie = x)
    )
    time_serie_samples <- do.call(rbind, time_serie_samples)

  }

  res <- time_serie_samples

  return(res)

}


break_detection_tests <- function(time_serie,
                                  p_value = 0.05) {

  same_values <- var(as.numeric(time_serie))

  if (same_values == 0) {

    res <- data.frame(
      test = "No test",
      breaks = NA,
      year_break = NA,
      p.value = NA,
      sig = NA
    )

  } else {

    break_prosition_pettit <- suppressWarnings(
      BreakPoints::pettit(serie = time_serie)
    )
    break_prosition_man.whi <- suppressWarnings(
      BreakPoints::man.whi(serie = time_serie)
    )
    break_prosition_stu <- suppressWarnings(
      BreakPoints::stu(serie = time_serie)
    )
    break_prosition_snht <- suppressWarnings(
      BreakPoints::SNHT(serie = time_serie, simulations = 100)
    )
    break_prosition_buishand <- suppressWarnings(
      BreakPoints::Buishand_R(serie = time_serie, simulations = 100)
    )

    res <-
      rbind(
        data.frame(break_prosition_pettit, test = "Pettitt test"),
        data.frame(break_prosition_man.whi,
                   test = "Mann-Whitney-Wilcoxon test"),
        data.frame(break_prosition_stu, test = "Student t test"),
        data.frame(break_prosition_snht,
                   test = "Standard Normal Homogeneity test"),
        data.frame(break_prosition_buishand, test = "Buishand Range test")
      )

    res$year_break <- as.numeric(format(time(time_serie)[res$breaks], "%Y"))
    res$sig <- sapply(res$p.value, function(x) ifelse(x < p_value, 0, 1))

    res <- res[, c("test", "breaks", "year_break", "p.value", "sig")]

  }

  return(res)

}


get_break_year <- function(detection_results_output,
                           percentage = 7) {

  res <- detection_results_output[detection_results_output$sig == 0, ]
  limit_n_test <- nrow(detection_results_output) * percentage / 100

  if (nrow(res) < 1) {

    res <- NA

  } else {

    res$freq <- sapply(res$breaks, function(x){

      length(res$breaks[res$breaks <= (x + 1) & res$breaks >= (x - 1)])

    })

    res <- res[res$freq > limit_n_test, ]
    # it is possible that after this point the data.frame does not have any row

    if (nrow(res) < 1) {

      res <- NA

    } else {

      res <- getmode(res[res$freq == max(res$freq, na.rm = TRUE),]$year_break)

    }
  }

  return(res)

}


getmode <- function(v) {

  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]

}