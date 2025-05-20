few_dry_values_check <- function(
    target_xts,
    params = list(n_unique_ = 15, size_dry_ = 99.5)) {


  nona_ts <- target_xts[!is.na(target_xts)]
  wetday_per <- length(nona_ts[nona_ts >= 0.1]) / length(nona_ts)

  if (wetday_per <= 0.05) {

    rm_dates <- NULL

  } else {

    size_unique <- length(unique(nona_ts))
    size_dry <- (length(nona_ts[nona_ts < 0.5]) / length(nona_ts)) * 100

    if (size_unique < params$n_unique_ || size_dry > params$size_dry) {

      rm_dates <- as.character(time(nona_ts))
      rm_dates <- as.Date(rm_dates)

    } else {

      rm_dates <- NULL

    }
  }

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
