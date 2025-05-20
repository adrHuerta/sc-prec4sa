
apply_break_correction <- function(time_series,
                                   year_of_break) {

  n_stations <- suppressWarnings(ncol(time_series[, -1]))

  if (is.na(year_of_break)) {

    hmg_res <- time_series[, 1]

  } else {

    if (n_stations < 4) {

      target_res <- time_series[, 1]
      hmg_res <- apply_absolute_correction(target_res,
                                           year_of_break = year_of_break)

    } else {

      target_res <- time_series[, 1]
      nearby_res <- time_series[, -1]
      hmg_res <- apply_relative_correction(target = target_res,
                                           nearby = nearby_res,
                                           year_of_break = year_of_break)

    }

  }

  res <- hmg_res
  return(res)

}


apply_absolute_correction <- function(target,
                                      year_of_break,
                                      window_c = 15,
                                      wet_day = 0) {

  dailyVar <- sort(unique(format(time(target), format = "%m-%d")))
  tail0 <- dailyVar[(length(dailyVar) - window_c + 1):length(dailyVar)]
  tail1 <- dailyVar[1:window_c]
  dailyVar <- c(tail0, dailyVar, tail1)

  dailyVar <- mapply(function(x, y) {
    unlist(dailyVar[x:y])
  }, x = 1:366, y = (window_c * 2 + 1):length(dailyVar), SIMPLIFY = FALSE)

  hmg_target_before_break <-
    lapply(
      seq_along(dailyVar),
      function(jx) {

        target_date <- dailyVar[[jx]]
        target_time_serie <- target[
          format(time(target), "%m-%d") %in% target_date
        ]
        model <- target_time_serie[
          paste("/", as.character(year_of_break), sep = "")
        ]
        obs <- target_time_serie[
          paste(as.character(year_of_break + 1), "/", sep = "")
        ]
        model2transf <- model[
          format(time(model), "%m-%d") %in% target_date[window_c + 1]
        ]

        new_model2transf <- tryCatch({

          quantile_matching_absolute(model = model,
                                     obs = obs,
                                     model2transf = model2transf,
                                     wet_day = wet_day)

        }, error = function(e) {

          return(model2transf)

        })

        new_model2transf

      }
    )

  hmg_target_before_break <- do.call(rbind, hmg_target_before_break)
  target[time(hmg_target_before_break)] <- hmg_target_before_break

  return(target)

}

quantile_matching_absolute <- function(model,
                                       obs,
                                       model2transf,
                                       wet_day = 0.1) {

  model2 <- model[model > wet_day]
  obs2 <- obs[obs > wet_day]
  model2transf2 <- model2transf[model2transf > wet_day]

  model3 <- pp2transf(model2)
  obs3 <- pp2transf(obs2)
  model2transf3 <- pp2transf(model2transf2)

  model4 <- model3[!is.na(model3)]
  obs4 <- obs3[!is.na(obs3)]
  model2transf4 <- model2transf3[!is.na(model2transf3)]

  quantiles_obs <- compute_quantiles(obs4)
  quantiles_model <- compute_quantiles(model4)

  matching_model <- approxfun(x = quantiles_model,
                              y = quantiles_obs,
                              method = "linear",
                              ties = "ordered")

  model2transf4_transf <- matching_model(model2transf4)
  model2transf4_transf_pp <- transf2pp(model2transf4_transf)
  zoo::coredata(model2transf4) <- model2transf4_transf_pp

  new_model2transf <- model2transf
  new_model2transf[time(model2transf4)] <- model2transf4

  return(new_model2transf)
}


apply_relative_correction <- function(target,
                                      nearby,
                                      year_of_break,
                                      window_c = 15,
                                      wet_day = 0) {

  dailyVar <- sort(unique(format(time(target), format = "%m-%d")))
  tail0 <- dailyVar[(length(dailyVar) - window_c + 1):length(dailyVar)]
  tail1 <- dailyVar[1:window_c]
  dailyVar <- c(tail0, dailyVar, tail1)

  dailyVar <- mapply(function(x, y){
    unlist(dailyVar[x:y])
  }, x = 1:366, y = (window_c * 2 + 1):length(dailyVar), SIMPLIFY = FALSE)

  hmg_target_before_break <-
    lapply(
      seq_along(dailyVar),
      function(jx) {

        target_date <- dailyVar[[jx]]
        target_time_serie <- target[
          format(time(target), "%m-%d") %in% target_date
        ]
        nearby_time_serie <- nearby[
          format(time(target), "%m-%d") %in% target_date
        ]

        model_target <- target_time_serie[
          paste("/", as.character(year_of_break), sep = "")
        ]
        obs_target <- target_time_serie[
          paste(as.character(year_of_break + 1), "/", sep = "")
        ]
        model_target2transf <- model_target[
          format(time(model_target), "%m-%d") %in% target_date[window_c + 1]
        ]

        model_nearby <- nearby_time_serie[
          paste("/", as.character(year_of_break), sep = "")
        ]
        obs_nearby <- nearby_time_serie[
          paste(as.character(year_of_break + 1), "/", sep = "")
        ]

        new_model2transf <- tryCatch({

          quantile_matching_relative(model_target = model_target,
                                     obs_target = obs_target,
                                     model_nearby = model_nearby,
                                     obs_nearby = obs_nearby,
                                     model_target2transf = model_target2transf,
                                     wet_day = wet_day)

        }, error = function(e) {

          return(model_target2transf)

        })

        new_model2transf

      }
    )

  hmg_target_before_break <- do.call(rbind, hmg_target_before_break)
  target[time(hmg_target_before_break)] <- hmg_target_before_break

  return(target)

}

# in quantiles_range 20 / 30 not too much change, 10 can be dangerous
quantile_matching_relative <- function(model_target,
                                       obs_target,
                                       model_nearby,
                                       obs_nearby,
                                       model_target2transf,
                                       wet_day = 0.1,
                                       quantiles_range = c(10, 100)) {

  model_dryday_target <- model_target[model_target > wet_day]
  obs_dryday_target <- obs_target[obs_target > wet_day]
  modeltransf2_dryday <- model_target2transf[model_target2transf > wet_day]
  obs_dryday_nearby <- lapply(obs_nearby, function(z) z[z > wet_day])
  model_dryday_nearby <- lapply(model_nearby, function(z) z[z > wet_day])

  obs2_target <- pp2transf(obs_dryday_target)
  model2_target <- pp2transf(model_dryday_target)
  model2transf_target <- pp2transf(modeltransf2_dryday)
  obs2_nearby <- lapply(obs_dryday_nearby, function(z) pp2transf(z))
  model2_nearby <- lapply(model_dryday_nearby, function(z) pp2transf(z))

  obs_nona_target <- obs2_target[!is.na(obs2_target)]
  model_nona_target <- model2_target[!is.na(model2_target)]
  model2transf_nona_target <- model2transf_target[!is.na(model2transf_target)]
  obs_nona_nearby <- lapply(obs2_nearby, function(z) z[!is.na(z)]) 
  model_nona_nearby <- lapply(model2_nearby, function(z) z[!is.na(z)])

  quantiles_model_target <- compute_quantiles(
    obs_nona_target, length.out = quantiles_range[1]
  )

  quantiles_obs_target <- compute_quantiles(
    model_nona_target, length.out = quantiles_range[1]
  )

  quantiles_model_nearby <- lapply(
    model_nona_nearby,
    function(z) compute_quantiles(z, length.out = quantiles_range[1])
  )
  quantiles_model_nearby <- do.call(cbind, quantiles_model_nearby)

  quantiles_obs_nearby <- lapply(
    obs_nona_nearby,
    function(z) compute_quantiles(z, length.out = quantiles_range[1])
  )
  quantiles_obs_nearby <- do.call(cbind, quantiles_obs_nearby)

  a_vect <- ((quantiles_obs_nearby - quantiles_model_nearby) -
      (quantiles_obs_target - quantiles_model_target)
  )
  moving_a_vect <- apply(
    a_vect, 2,
    function(x) {

      zoo::rollapply(
        x, width = 5, partial = TRUE,
        align = "center",
        FUN = function(z) mean(z)
      )

    }
  )
  moving_median_vect <- apply(moving_a_vect, 1, median)

  # at this part I could check the a_vect

  quantiles_model_target_100 <- compute_quantiles(
    model_nona_target, length.out = quantiles_range[2]
  )
  quantiles_obs_target_100 <- compute_quantiles(
    obs_nona_target, length.out = quantiles_range[2]
  )
  quantiles_model_target_100_c <- quantiles_model_target_100

  # at this part I could make better the quantile correction from 10 to 100

  a_vect <- data.frame(
    y = moving_median_vect,
    x = as.numeric(gsub("[\\%,]", "", names(quantiles_obs_target)))
  )
  a_vect_interpolated <- approx(
    a_vect$x, a_vect$y, xout = 1:100, method = "linear", ties = "ordered"
  )$y
  a_vect_interpolated_smoothed <- zoo::rollapply(
    a_vect_interpolated,
    width = 10,
    partial = TRUE,
    align = "center",
    FUN = function(z) mean(z)
  )

  quantiles_model_target_100_c <- quantiles_model_target_100  +
    a_vect_interpolated_smoothed

  # at this part I could ensure no negative slopes
  # now it not needeed, but it could be useful in the future

  matching_model <- approxfun(x = quantiles_model_target_100,
                              y = quantiles_model_target_100_c,
                              method = "linear",
                              ties = "ordered")

  model2transf4_transf <- matching_model(model2transf_nona_target)
  model2transf4_transf_pp <- transf2pp(model2transf4_transf)
  zoo::coredata(model2transf_nona_target) <- model2transf4_transf_pp

  new_model2transf <- model_target2transf
  new_model2transf[time(model2transf_nona_target)] <- model2transf_nona_target

  return(new_model2transf)

}