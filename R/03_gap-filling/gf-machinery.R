
build_matrix <- function(stations,
                         covars = c("LAT", "LON", "dcoast"),
                         xts_database,
                         xy_database) {

  out_xts <- xts_database[, stations]
  out_xy <- xy_database[match(stations, xy_database$ID),  c("ID", covars)]
  row.names(out_xy) <- NULL

  out_list <- list(values_data = out_xts,
                   predictors_data = out_xy,
                   covars = covars)

  return(out_list)

}


gap_filling <- function(target_data,
                        FUN,
                        lmt_min = 7,
                        lmt_max = 10) {

  matrix_data <- target_data$values_data

  out <-
    lapply(seq_len(nrow(matrix_data)), function(zz) {

      zz_matrix_data <- matrix_data[zz, ]

      max_id <- which(cumsum(!is.na(zz_matrix_data[, -1])) == lmt_max)
      lmt_max_id <- ifelse(
        length(max_id) < 1,
        ncol(zz_matrix_data[, -1]),
        max(max_id) + 1
      )
      new_zz_matrix_data <- zz_matrix_data[, 1:lmt_max_id]
      new_zz_predictor_data <- target_data$predictors_data[1:lmt_max_id, ]

      length_step <- sum(!is.na(new_zz_matrix_data[, -1]))
      max_step <- max((new_zz_matrix_data[, -1]), na.rm = TRUE)
      all_equal <- var(as.numeric(new_zz_matrix_data[, -1]), na.rm = TRUE)

      if (length_step < lmt_min | max_step == -Inf) {

        FUN(values_data = new_zz_matrix_data,
            predictors_data = new_zz_predictor_data,
            covars = target_data$covars,
            set2naOR0 = TRUE)

      } else if (max_step == "0" | all_equal == 0) {

        FUN(values_data = new_zz_matrix_data,
            predictors_data = new_zz_predictor_data,
            covars = target_data$covars,
            set2naOR0 = "0")

      } else {

        FUN(values_data = new_zz_matrix_data,
            predictors_data = new_zz_predictor_data,
            covars = target_data$covars,
            set2naOR0 = FALSE)

      }
    })

  out <- do.call(rbind, out)
  out <- data.frame(time_step = time(matrix_data), out)

  return(out)

}


blending <- function(target_data) {

  new_target_data <- transform(
    target_data,
    obs_pred = ifelse(!is.na(obs), obs, round(mod_pred, 1))
  )

  new_target_data <- new_target_data[,
    c("time_step",
      "obs",
      "wd_pred",
      "raw_pred",
      "mod_pred",
      "err",
      "obs_pred",
      "n_stations")
  ]

  return(new_target_data)

}