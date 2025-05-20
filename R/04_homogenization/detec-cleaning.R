detreding_clean <- function(time_serie,
                            p_value = 0.05) {

  # testing trend significance
  sample_value <- as.numeric(time_serie)
  mk_res <- Kendall::MannKendall(time_serie)

  # removing trend if mk_res$sl < p_value
  if (as.numeric(mk_res$sl) < p_value) {

    res <- pracma::detrend(sample_value, tt = "linear")
    zoo::coredata(time_serie) <- res

    return(time_serie)

  } else {

    return(time_serie)

  }

}

autocorrelation_clean <- function(time_serie,
                                  p_value = 0.05) {

  # testing AR1 significance
  sample_value <- as.numeric(time_serie)
  dwt_res <- car::durbinWatsonTest(lm(sample_value ~ 1), max.lag = 1)

  # removing AR1 if dwt_res$p < p_value
  if (dwt_res$p < p_value) {

    ar_model <- arima(sample_value, order = c(1, 0, 0))
    res <- as.numeric(ar_model$residual)
    zoo::coredata(time_serie) <- res

    return(time_serie)

  } else {

    return(time_serie)

  }

}

preprocessing_clean <- function(time_serie,
                                p_value = 0.05) {

  #first detrending
  trend_test <- detreding_clean(time_serie = time_serie,
                                p_value = p_value)

  #second prewhitening
  ar1_test <- autocorrelation_clean(time_serie = trend_test,
                                    p_value = p_value)

  return(ar1_test)

}