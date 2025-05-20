# issue install metrica with renv
# check https://github.com/rstudio/renv/issues/1926

apply_metrics <- function(target_data,
                          annual_cycle = FALSE) {

  target_data <- data.frame(
    time_step = target_data[, match("time_step", colnames(target_data))],
    actual = as.numeric(target_data[, match("obs", colnames(target_data))]),
    predicted = as.numeric(
      target_data[, -match(c("obs", "time_step"), colnames(target_data))]
    )
  )
  target_data <- target_data[complete.cases(target_data), ]
  target_data$month <- as.numeric(format(target_data$time_step, "%m"))
  target_data$month <- factor(target_data$month, levels = 1:12)

  if (isTRUE(annual_cycle)) {

    res_metrics <- by(target_data, target_data$month,
                      function(ij) {

                        get_metrics(actual = ij$actual,
                                    predicted = ij$predicted)

                      })

    res_metrics <- do.call(rbind, res_metrics)
    res_metrics <- colMeans(res_metrics)


  } else {

    res_metrics <- get_metrics(actual = target_data$actual,
                               predicted = target_data$predicted)

  }

  return(res_metrics)

}

get_metrics <- function(actual,
                        predicted) {

  # categorical
  actual_p_class <- actual
  actual_p_class[actual_p_class < 0.1] <- 0
  actual_p_class[actual_p_class >= 0.1] <- 1

  model_p_class <- predicted
  model_p_class[model_p_class < 0.1] <- 0
  model_p_class[model_p_class >= 0.1] <- 1

  m_0per <- length(actual_p_class[actual_p_class == 1]) / length(actual_p_class)

  m_precision <- tryCatch(as.numeric(metrica::precision(obs = actual_p_class,
                                                        pred = model_p_class)),
                          error = function(e) {

                            NA

                          })

  m_recall <- tryCatch(as.numeric(metrica::recall(obs = actual_p_class,
                                                  pred = model_p_class)),
                       error = function(e) {

                         NA

                       })

  m_f1 <- tryCatch(as.numeric(metrica::fscore(obs = actual_p_class,
                                              pred = model_p_class)),
                   error = function(e) {

                     NA

                   })

  m_ba <- tryCatch(as.numeric(metrica::balacc(obs = actual_p_class,
                                              pred = model_p_class)),
                   error = function(e) {

                     NA

                   })

  m_gm <- tryCatch(as.numeric(metrica::gmean(obs = actual_p_class,
                                             pred = model_p_class)),
                   error = function(e) {

                     NA

                   })

  m_accuracy <- tryCatch(as.numeric(metrica::accuracy(obs = actual_p_class,
                                                      pred = model_p_class)),
                         error = function(e) {

                           NA

                         })

  # no cantegorical
  m_mae <- as.numeric(Metrics::mae(actual = actual,
                                   predicted = predicted))

  m_nmae <- as.numeric(ifelse(length(unique(actual)) < 2, NA,
                              compute.nmae(Y = predicted,
                                           X = actual)))
  m_rmse <- as.numeric(Metrics::rmse(actual = actual,
                                     predicted = predicted))
  m_nrmse <- nrmse_func(obs = actual,
                        pred = predicted)
  m_nrmse <- as.numeric(ifelse(is.infinite(m_nrmse), NA, m_nrmse))

  m_dr <- IOA(mod = "actual",
              obs = "predicted",
              x = data.frame(actual = as.numeric(actual),
                             predicted = as.numeric(predicted)))
  m_dr <- as.numeric(m_dr)

  res <- data.frame(mae = m_mae,
                    nmae = m_nmae,
                    rmse = m_rmse,
                    nrmse = m_nrmse,
                    dr = m_dr,
                    wet_day = m_0per,
                    accuracy = m_accuracy,
                    precision = m_precision,
                    recall = m_recall,
                    f1 = m_f1,
                    bcc = m_ba,
                    g_mean = m_gm,
                    n_data = length(actual))

  res <- round(res, 2)

  return(res)

}


compute.nmae <- function(Y, X) {

  # https://github.com/cran/DTWBI/blob/master/R/1_2NMAE.R

  if (length(Y)!=length(X)) {
    stop("Input vectors are of different length !!!")
  }

  lengthNAX <- sum(is.na(X)) # Number of NA values

  if (lengthNAX > 0 ) {
    warning(paste("Vector of true values contains ", lengthNAX, " NA !!! NA excluded", sep = ""))
  }

  lengthNAY <- sum(is.na(Y)) # Number of NA values

  if (lengthNAY > 0) {
    warning(paste("Vector of imputed values contains ", lengthNAY, " NA !!! NA excluded", sep = ""))
  }
  n <- length(X)-max(lengthNAX, lengthNAY)

  if ((max(X, na.rm = TRUE) - min(X, na.rm = TRUE)) == 0) {
    if ((max(Y, na.rm = TRUE) -min(Y, na.rm = TRUE)) == (max(X, na.rm = TRUE) - min(X, na.rm = TRUE)) ){

      warning("Vectors of true and imputed values are constant and equal !!! By definition NMAE=0")

      out <- 0
      return(out)

    } else {

        warning("Vector of true values is constant !!! MAE was computed instead of NMAE !!!")
        numerator <- sum(abs(Y - X), na.rm = TRUE)
        out <- numerator / n
        return(out)
    }
  } else {

    numerator <- sum(abs(Y-X) / (max(X, na.rm = TRUE) - min(X, na.rm = TRUE)), na.rm = TRUE)
    out <- numerator / n
    return(out)
  }
}


nrmse_func <-  function(obs,
                        pred,
                        type = "sd") {

  # https://www.marinedatascience.co/blog/2019/01/07/normalizing-the-rmse/
  squared_sums <- sum((obs - pred)^2)
  mse <- squared_sums/length(obs)
  rmse <- sqrt(mse)
  if (type == "sd") nrmse <- rmse / sd(obs)
  if (type == "mean") nrmse <- rmse / mean(obs)
  if (type == "maxmin") nrmse <- rmse / (max(obs) - min(obs))
  if (type == "iq") nrmse <- rmse / (quantile(obs, 0.75) - quantile(obs, 0.25))
  if (!type %in% c("mean", "sd", "maxmin", "iq")) message("Wrong type!")
  nrmse <- round(nrmse, 3)
  return(nrmse)

}

IOA <- function(x,
                mod = "mod",
                obs = "obs")
{
  x <- na.omit(x[, c(mod, obs)])

  LHS <- sum(abs(x[[mod]] - x[[obs]]))
  RHS <- 2 * sum(abs(x[[obs]] - mean(x[[obs]])))

  if (LHS <= RHS) res <- 1 - LHS / RHS else res <- RHS / LHS - 1

  data.frame(IOA = res)
}



eval_ts_plots <- function(target_data,
                          station_id,
                          model_id,
                          make_plot = FALSE) {

  station_id_df <- target_data[
    target_data$station == station_id &  target_data$model == model_id,
  ]
  station_id_df_ts <- xts::xts(
    station_id_df[, c("obs",
                      "mod_pred",
                      "obs_mod_pred",
                      "bc_pred",
                      "obs_bc_pred")],
    station_id_df[, "time_step"]
  )

  if (make_plot == TRUE) {

    print(
      lattice::xyplot(
        station_id_df_ts, type = "p", cex = .1,
        ylim = c(min(station_id_df[, c("obs",
                                       "mod_pred",
                                       "obs_mod_pred",
                                       "bc_pred",
                                       "obs_bc_pred")],
                     na.rm = TRUE),
                 max(station_id_df[, c("obs",
                                       "mod_pred",
                                       "obs_mod_pred",
                                       "bc_pred",
                                       "obs_bc_pred")],
                     na.rm = TRUE))
      )
    )

  } else {

    station_id_df_ts
  }

}