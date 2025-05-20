deciding_matching_10m <- function(list_xy,
                                  xy_database,
                                  val_database) {

  val_list_xy <- val_database[, list_xy]

  # size
  # lengh_values_xy <- colSums(!is.na(val_list_xy))

  # correlation/mae with 5 closest stations
  nearby_stations_xy <- get_nearby_points(xy_target = list_xy[1],
                                          xy_database = xy_database,
                                          lmt_xy = NA,
                                          lmt_n = 5 + length(list_xy) - 1)

  nearby_stations_xy <- nearby_stations_xy[
    -match(list_xy, nearby_stations_xy)
  ]

  val_list_nearby_xy <- val_database[, nearby_stations_xy]

  r_nearby <-
    lapply(val_list_xy,
           function(idd) {

             sapply(val_list_nearby_xy,
                    function(izz) {

                      to_con <- data.frame(actual = idd,
                                           pred = izz)
                      to_con <- to_con[complete.cases(to_con), ]
                      ifelse(nrow(to_con) < 30,
                             NA,
                             cor(to_con$actual, to_con$pred))

                    })

           })
  r_nearby <- sapply(r_nearby, median, na.rm = TRUE)

  mae_nearby <-
    lapply(val_list_xy,
           function(idd) {

             sapply(val_list_nearby_xy,
                    function(izz) {

                      to_con <- data.frame(actual = idd,
                                           pred = izz)
                      to_con <- to_con[complete.cases(to_con), ]
                      ifelse(nrow(to_con) < 30,
                             NA,
                             Metrics::mae(to_con$actual, to_con$pred))

                    })

           })
  mae_nearby <- sapply(mae_nearby, median, na.rm = TRUE)

  choosen_station <- ifelse(
    which.max(r_nearby) == which.min(mae_nearby),
    names(which.max(r_nearby)),
    names(which.max(r_nearby))
  )
  choosen_station <- as.character(choosen_station)
  removed_station <- list_xy[-match(choosen_station, list_xy)]

  out <- list(choosen_station = choosen_station,
              removed_station = removed_station)

  return(out)

}


lagpad <- function(x, k) {
  if (k > 0) {

    return(c(rep(NA, k), x)[1:length(x)]);

  } else {

    return(c(x[(-k + 1) : length(x)], rep(NA, -k)))

  }
}


deciding_matching_25km <- function(list_xy,
                                   xy_database = NULL,
                                   val_database) {

  val_list_xy <- val_database[, list_xy]

  # check based on r and mae
  r_nearby <-
    lapply(c(-1, 0, 1),
           function(idd) {

             lagged_df <- val_list_xy
             lagged_df[, 1] <- lagpad(lagged_df[, 1], k = idd)

             sapply(seq_len(ncol(lagged_df)),
                    function(izz) {

                      to_con <- data.frame(actual = lagged_df[, 1],
                                           pred = lagged_df[, izz])
                      to_con <- to_con[complete.cases(to_con), ]
                      ifelse(nrow(to_con) < 30,
                             NA,
                             cor(to_con$actual, to_con$pred))


                    })

           })

  mae_nearby <-
    lapply(c(-1, 0, 1),
           function(idd) {

             lagged_df <- val_list_xy
             lagged_df[, 1] <- lagpad(lagged_df[, 1], k = idd)

             sapply(seq_len(ncol(lagged_df)),
                    function(izz) {

                      to_con <- data.frame(actual = lagged_df[, 1],
                                           pred = lagged_df[, izz])
                      to_con <- to_con[complete.cases(to_con), ]
                      ifelse(nrow(to_con) < 30,
                             NA,
                             Metrics::mae(to_con$actual, to_con$pred))


                    })

           })

  rmae_df <-
    lapply(
      seq_len(length(r_nearby)),
      function(x) {

        data.frame(rval = r_nearby[[x]],
                   maeval = mae_nearby[[x]],
                   ID = list_xy)

      }

    )

  rmae_df <- do.call(rbind, rmae_df)
  rmae_df <- rmae_df[complete.cases(rmae_df), ]

  rmae_id <- rmae_df[rmae_df$rval > 0.9999 & rmae_df$maeval < 0.1, ]$ID
  rmae_id <- unique(rmae_id[!is.na(rmae_id)])

  # check based on same values
  # same values above 0.5 mm
  val_list_xy_comp <- val_list_xy
  val_list_xy_comp[val_list_xy_comp < 0.5] <- 0

  check_same_values <- lapply(
    seq_along(list_xy)[2:length(list_xy)],
    function(i) {

      n_same_values_df <- data.frame(target = val_list_xy_comp[, 1,],
                                     nearby = val_list_xy_comp[, i])
      n_same_values_df <- n_same_values_df[complete.cases(n_same_values_df), ]

      n_same_values_df <- transform(n_same_values_df,
                                    dif = target - nearby,
                                    is0 = (target + nearby) == 0)

      n_same_values_df <- n_same_values_df[
        n_same_values_df$is0 == FALSE,
      ]

      n_same_values_df <- n_same_values_df[
        n_same_values_df$dif == 0,
      ]

      all_values_target <- val_list_xy_comp[, 1]
      all_values_target <- sum(!is.na(all_values_target))

      all_values_nearby <- val_list_xy_comp[, i]
      all_values_nearby <- sum(!is.na(all_values_nearby))

      n_same_values <- length(n_same_values_df$target)
      target_percent <- n_same_values  * 100 / all_values_target
      nearby_percent <- n_same_values  * 100 / all_values_nearby

      # 50% of the values are the same in both stations
      if (target_percent > 50 & nearby_percent > 50) {

        out1 <- colnames(val_list_xy_comp[, 1, drop = FALSE])
        out2 <- colnames(val_list_xy_comp[, i, drop = FALSE])
        out <- c(out1, out2)

      } else {

        out <- NULL

      }

      return(out)

    }
  )
  check_same_values <- unlist(check_same_values)

  # combine all coinditions
  all_id <- c(rmae_id, check_same_values)
  all_id <- unique(all_id)

  # deciding by size
  # if two stations have the same size, both are removed
  # in the final procedure
  lengh_values_xy <- colSums(!is.na(val_list_xy[, all_id, drop = FALSE]))

  choosen_station <- names(which.max(lengh_values_xy))
  choosen_station <- as.character(choosen_station)
  removed_station <- names(lengh_values_xy[-which.max(lengh_values_xy)])

  out <- list(choosen_station = choosen_station,
              removed_station = removed_station)

  return(out)

}


deciding_elevation <- function(xy_target,
                               xy_database,
                               dem_data,
                               params = list(buffer_ = 1000, percent_ = 0.5),
                               make_plot = FALSE) {

  target <- xy_database[match(xy_target, xy_database$ID), ]
  target_bf <- terra::buffer(target, params$buffer_)

  dem_value <- terra::crop(dem_data, target_bf)
  dem_value <- terra::mask(dem_value, target_bf)

  if (make_plot == TRUE) {

    terra::plot(dem_value)
    terra::plot(target_bf, add = TRUE)
    terra::plot(target, add = TRUE)

  }

  dem_target_value <- as.numeric(terra::extract(dem_value, target, ID = FALSE))
  dem_value <- terra::values(dem_value)
  dem_value <- as.numeric(dem_value[complete.cases(dem_value)])

  size_ores <- length(dem_value)
  size_n_ores <- sum(dem_value < 0)
  more_negative_elv_values <- size_n_ores >= (size_ores * params$percent_)

  choosen_station <- target$ID
  removed_station <-
    ifelse(
      dem_target_value < 0 | isTRUE(more_negative_elv_values),
      choosen_station,
      NA
    )

  out <- list(choosen_station = choosen_station,
              removed_station = removed_station)

  return(out)

}
