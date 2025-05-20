spatemp_value_check <- function(xy_target,
                                xy_database,
                                xts_database,
                                params = list(n_nearby_ = 5)) {

  # spatial check

  xy_nearby <- get_nearby_points(xy_target = xy_target,
                                 xy_database = xy_database,
                                 lmt_xy = 400000,
                                 lmt_n = 10)
  xy_nearby <- xy_nearby[-1]

  target_xts <- xts_database[, xy_target]
  nearby_xts <- xts_database[, xy_nearby]

  dif_target_nearby <- lapply(
    seq_len(ncol(nearby_xts)),
    function(idd) {

      dif_ts <- target_xts - nearby_xts[, idd]
      dif_099 <- quantile(dif_ts, 0.9999, na.rm = TRUE)
      flag_099 <- time(dif_ts[dif_ts >= dif_099])
      out_ts <- dif_ts
      out_ts[!is.na(out_ts)] <- 0
      out_ts[flag_099] <- 1

      out_ts
    }
  )

  dif_target_nearby <- do.call(cbind, dif_target_nearby)
  dif_target_nearby <- rowSums(dif_target_nearby, na.rm = TRUE)
  # time step in which at least 5 nearby points are flagged
  dif_target_nearby <- which(dif_target_nearby >= params$n_nearby_)
  # getting the dates
  dif_target_nearby <- time(target_xts)[dif_target_nearby]
  dif_target_nearby <- as.character(dif_target_nearby)

  # temporal check
  # previous date
  dif_target_target_pre <-  target_xts - stats::lag(target_xts, k = -1)
  quant_099_pre <- quantile(
    dif_target_target_pre,
    0.9999,
    na.rm = TRUE
  )
  dif_target_target_pre <- dif_target_target_pre[
    dif_target_target_pre > quant_099_pre |
      dif_target_target_pre < -quant_099_pre
  ]
  dif_target_target_pre <- as.character(time(dif_target_target_pre))

  # next date
  dif_target_target_nex <-  stats::lag(target_xts, k = 1) - target_xts
  quant_099_next <- quantile(
    dif_target_target_nex[dif_target_target_nex > 0],
    0.9999,
    na.rm = TRUE
  )
  dif_target_target_nex <- dif_target_target_nex[
    dif_target_target_nex > quant_099_next |
      dif_target_target_nex < -quant_099_next
  ]
  dif_target_target_nex <- as.character(time(dif_target_target_nex))
  dif_target_target <- intersect(dif_target_target_pre, dif_target_target_nex)

  # intersecting both checks
  rm_dates <- intersect(dif_target_target, dif_target_nearby)

  qc_data <- qc_data_flagged <- target_xts
  qc_data[rm_dates] <- NA
  qc_data_flagged[!is.na(qc_data_flagged)] <- 0
  qc_data_flagged[rm_dates] <- 1

  out <- list(
    qc_data = qc_data,
    qc_data_flagged = qc_data_flagged
  )

  return(out)
}