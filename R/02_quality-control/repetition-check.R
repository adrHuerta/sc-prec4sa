repetition_nonzero_check <- function(target_xts) {

  n_consec_rep <- unlist(
    lapply(
      rle(as.numeric(target_xts))$lengths,
      seq_len
    )
  )

  df_consec <- data.frame(
    date = time(target_xts),
    ym = format(time(target_xts), "%Y-%m"),
    values = as.numeric(target_xts),
    rep = n_consec_rep
  )

  rm_dates <- df_consec[df_consec$values > 10 & df_consec$rep >= 4, ]
  rm_dates <- rm_dates$date

  # Also check for consecutive repetitions by month
  # (flaged all values if values are the same in a month)
  rm_dates_by_month <-
    by(df_consec,
       df_consec$ym,
       function(idd) {

        t_idd <- idd[idd$values > 10 & idd$rep >= length(idd$ym), ]$date
        if (length(t_idd) >= 1) {
          as.character(idd$date)
        } else {
          NULL
        }

      }
    )

  rm_dates_by_month <- as.Date(unlist(rm_dates_by_month))
  rm_dates <- c(rm_dates, rm_dates_by_month)
  rm_dates <- sort(unique(rm_dates))

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

repetition_zero_check <- function(target_xts,
                                  params = list(weight_ = 6)) {

  nona_ts <- target_xts[!is.na(target_xts)]
  wetday_per <- length(nona_ts[nona_ts >= 0.1]) / length(nona_ts)

  # If the percentage of wet days is less than 5%
  # then the function will not be applied
  if (wetday_per <= 0.05) {

    rm_dates <- NULL


  } else {

    yearly_zero_freq <-
      xts::apply.yearly(
        target_xts,
        function(iij) {

          iij <- iij[!is.na(iij)]
          n_jzeros <- length(iij[iij == 0])
          n_jall <- length(iij)
          (n_jzeros / n_jall) * 100

        }
      )

    yearly_size <-
      xts::apply.yearly(
        target_xts,
        function(iij) {

          iij <- iij[!is.na(iij)]
          n_jall <- length(iij)
          (n_jall / 365) * 100

        }
      )

    df_consec <- data.frame(
      year = format(time(yearly_zero_freq), "%Y"),
      zero_freq = as.numeric(yearly_zero_freq),
      size = as.numeric(yearly_size)
    )
    df_consec <- df_consec[complete.cases(df_consec), ]

    tresh_u <- median(
      df_consec[df_consec$size > 85, ]$zero_freq
    ) +
      params$weight_ * IQR(df_consec[df_consec$size > 85, ]$zero_freq)

    tresh_l <- median(
      df_consec[df_consec$size > 85, ]$zero_freq
    ) -
      params$weight_ * IQR(df_consec[df_consec$size > 85, ]$zero_freq)

    # If the percentage of zeros is greater than the upper or lower threshold
    # but also the size of the series is greater than 85%
    # then the dates will be flagged
    rm_dates <-
      transform(
        df_consec,
        y_0_freq = ifelse(
          zero_freq > tresh_u | zero_freq < tresh_l,
          TRUE,
          FALSE
        ),
        y_l = ifelse(
          size > 85,
          TRUE,
          FALSE)
      )
    rm_dates <- rm_dates[rm_dates$y_0_freq == TRUE & rm_dates$y_l == TRUE,  ]
    rm_dates <- rm_dates$year
    rm_dates <- time(target_xts)[format(time(target_xts), "%Y") %in% rm_dates]

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