zscore_based_outlier_check <- function(target_xts,
                                       out_weight = 10) {

  nona_ts <- target_xts[!is.na(target_xts)]
  wetday_per <- length(nona_ts[nona_ts >= 0.1]) / length(nona_ts)

  mean_sd_mean <- zscore_based_mean(target_xts = target_xts)
  out_weight <- ifelse(wetday_per <= 0.05, out_weight * 3, out_weight)
  mean_sd_mean$sd_md <- mean_sd_mean$sd_md * out_weight
  mean_sd_mean$mean_plus_sd <- mean_sd_mean$mean_md + mean_sd_mean$sd_md
  # mean_sd_mean$mean_plus_sd <- mean_sd_mean$mean_md - mean_sd_mean$sd_md


  rm_dates <- lapply(
    mean_sd_mean$md,
    function(izz) {

      izz_val <- target_xts[format(time(target_xts), "%m-%d") %in% izz]
      izz_above <- izz_val > mean_sd_mean[mean_sd_mean$md == izz, ]$mean_plus_sd
      # izz_below <- izz_val < mean_sd_mean[mean_sd_mean$md == izz, ]$mean_plus_sd

      out <- c(
        time(izz_above[izz_above == TRUE])
      )
      out <- unique(as.character(out))
      out

    }
  )
  rm_dates <- unlist(rm_dates)

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


zscore_based_mean <- function(target_xts,
                              window_c = 15,
                              n_years = 10) {

  md_unique <- format(time(target_xts), format = "%m-%d")
  md_unique <- sort(unique(md_unique))

  tail_0 <- md_unique[
    (length(md_unique) - window_c + 1):length(md_unique)
  ]
  tail_1 <- md_unique[1:window_c]

  md_unique <- c(tail_0, md_unique, tail_1)
  md_unique <- mapply(
    function(xdd, ydd) {

      md_unique[xdd:ydd]

    },
    x = 1:366,
    y = ((window_c * 2) + 1):length(md_unique),
    SIMPLIFY = FALSE
  )
  md_unique <- lapply(
    md_unique,
    function(idd) {

      val_idd <- target_xts[format(time(target_xts), "%m-%d") %in% idd]
      length_idd <- xts::apply.yearly(val_idd, function(xi) sum(!is.na(xi)))
      # a month is full if it has 15 days of data (-15 / 15)
      length_idd <- nrow(length_idd[length_idd >= 15])
      mean_md <- mean(as.numeric(val_idd), na.rm = TRUE)
      sd_md <- sd(as.numeric(val_idd), na.rm = TRUE)
      data.frame(
        ny = length_idd,
        md = idd[window_c + 1],
        mean_md = mean_md,
        sd_md = sd_md
      )

    }
  )
  md_unique <- do.call(rbind, md_unique)
  # to be used in the zscore_based_outlier_check function
  # the mean and sd should be calculated using at least 10 years of data
  md_unique <- md_unique[md_unique$ny >= n_years, ]
  md_unique

}