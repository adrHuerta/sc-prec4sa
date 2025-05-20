wd_value <- function(xts_obj) {

  nona_ts <- xts_obj[!is.na(xts_obj)]
  wetday_per <- length(nona_ts[nona_ts >= 0.1]) / length(nona_ts)
  wetday_per <- wetday_per * 100

  return(wetday_per)

}

get_wc <- function(xts_obj) {

  nona_ts <- xts_obj[!is.na(xts_obj)]
  wetday_per <- round(
    length(nona_ts[nona_ts >= 0.1]) /
      length(nona_ts) * 100,
    1
  )

  if (dim(xts_obj)[1] < 1 | wetday_per < 10) {

    data.frame(year = NA,
               dec = NA)
  } else {

    wd_fraction(xts_obj)

  }

}

get_wd_fraction <- function(xts_obj) {

  original_locale <- Sys.getlocale("LC_TIME")  # Save the current locale
  Sys.setlocale("LC_TIME", "C")                # Set locale to English

  out_df <- data.frame(
    value = as.numeric(xts_obj),
    week = weekdays(time(xts_obj))
  )
  out_df_wd <- out_df[out_df$value >= 0.1, ]

  length_values <- aggregate(
    value ~ week,
    data = out_df,
    function(x) length(x[!is.na(x)]),
    na.action = NULL
  )

  if (all(is.na(out_df_wd$value))) {

    lenght_wd <- length_values

  } else {

    lenght_wd <- aggregate(
        value ~ week,
        data = out_df_wd,
        function(x) length(x[!is.na(x)]),
        na.action = NULL
    )

  }

  out_df <- merge(length_values, lenght_wd, by = "week")
  colnames(out_df) <- c("week", "count", "count_wd")
  out_df <- transform(
    out_df,
    week = factor(
        week,
        levels = c("Monday",
                   "Tuesday",
                   "Wednesday",
                   "Thursday",
                   "Friday",
                   "Saturday",
                   "Sunday"),
        labels = c("Mon",
                   "Tue",
                   "Wed",
                   "Thu",
                   "Fri",
                   "Sat",
                   "Sun"),
        ordered = TRUE
    )

  )

  out_df <- transform(out_df, frac_wd = count_wd / count)


  if (is.nan(sum(out_df$frac_wd)) | is.na(sum(out_df$frac_wd))) {

    out_df$bin_test <- NA
    out_df$bin_test <- factor(
      out_df$bin_test,
      levels = c("Rejected Ho", "No Rejected Ho")
    )

  } else {

    for (i in 1:nrow(out_df)) {

      test_bt <- binom.test(out_df$count_wd[i], out_df$count[i],
                            p = sum(out_df$count_wd)/sum(out_df$count),
                            alternative = "two.sided",
                            conf.level = 0.95)

      out_df$bin_test[i] <- ifelse(
        !((test_bt$conf.int[1] < sum(out_df$count_wd)/sum(out_df$count)) &
            (sum(out_df$count_wd)/sum(out_df$count) < test_bt$conf.int[2])),
        "Rejected Ho",
        "No Rejected Ho"
      )

    }

    out_df$bin_test <- factor(
      out_df$bin_test,
      levels = c("Rejected Ho", "No Rejected Ho")
    )

  }

  out_df <- out_df[order(out_df$week), ]
  rownames(out_df) <- NULL
  out_df$frac_wd <- round(out_df$frac_wd * 100, 1)

  Sys.setlocale("LC_TIME", original_locale)    # Reset to the original locale
  return(out_df)

}

get_levels_wc <- function(xts_obj) {

  wc_res <- get_wd_fraction(xts_obj = xts_obj)
  wc_res_rej <- wc_res[wc_res$bin_test == "Rejected Ho", ]
  wc_res_norej <- wc_res[wc_res$bin_test == "No Rejected Ho", ]

  # special case with station X9446 in ES (aragon)
  if (length(wc_res_norej$frac_wd) == 0) {

    diff_rejnorej <- Inf

  } else {

    diff_rejnorej <- sapply(
      wc_res_rej$frac_wd,
      function(idd) abs(idd - wc_res_norej$frac_wd)
    )
    diff_rejnorej <- ifelse(length(diff_rejnorej) < 1, 0, max(diff_rejnorej))

  }

  if (nrow(wc_res_rej) == 0) {

    response <- 0

  } else {

    if (nrow(wc_res_rej) <= 2 & diff_rejnorej < 10) {

      response <- 1

    } else {

      response <- 2

    }

  }

  return(response)

}
