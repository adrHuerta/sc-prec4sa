nextyear_dup_monthly_check <- function(target_xts,
                                       Nyears = 11) {

  df_ym <- data.frame(
    values = as.numeric(target_xts),
    date = time(target_xts),
    ym = format(time(target_xts), "%Y-%m")
  )

  # ensuring tha low precipitation values are not considered
  df_ym <-
    transform(
      df_ym,
      values = ifelse(values > 0 & values < 0.5, round(values, 0), values)
    )
  # df_ym <- df_ym[complete.cases(df_ym), ]

  ym_unique <- unique(df_ym$ym)

  if (length(ym_unique) < (12 * 5)) {

    rm_dates <- NULL

  } else {

    ym_unique_cut <- ym_unique

    subseq_m <-
      lapply(
        seq_along(ym_unique_cut),
        function(x) {

          # Nyears later
          seg_to_check <- ym_unique[seq(x, length(ym_unique), 12)][1:Nyears]
          seg_target <- df_ym[df_ym$ym %in% seg_to_check[1], ]$values
          seq_eval <- df_ym[
            df_ym$ym %in% seg_to_check[2:length(seg_to_check)],
          ]

          size_na_seg_target <- length(seg_target[!is.na(seg_target)])
          size_na_seq_eval <- seq_eval$values
          size_na_seq_eval <- length(size_na_seq_eval[!is.na(size_na_seq_eval)])

          if (size_na_seg_target < 10 | size_na_seq_eval < 10) {

            rep_values_dates <- NA

          } else {

            # here we compute the moving correlation with specific parameters
            rep_values_dates <- df_win_mov_cor(xva = seg_target,
                                               ydf = seq_eval)


          }

          rep_values_dates <- rep_values_dates[!is.na(rep_values_dates)]

          list(
            date_eva = as.character(
              df_ym[df_ym$ym %in% seg_to_check[1], ]$date
            ),
            date_rep = as.character(rep_values_dates)
          )
        }
      )

    subseq_m <- subseq_m[lapply(subseq_m, function(x) length(x$date_rep)) != 0]
    subseq_m_a <- lapply(subseq_m, function(x) as.character(x$date_rep))
    subseq_m_a <- unlist(subseq_m_a)
    subseq_m_b <- lapply(subseq_m, function(x) as.character(x$date_eva))
    subseq_m_b <- unlist(subseq_m_b)

    rm_dates <- unique(c(subseq_m_a, subseq_m_b))
    rm_dates <- as.Date(rm_dates)
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


subyear_dup_monthly_check <- function(target_xts,
                                      Nmonths = 11) {

  df_ym <- data.frame(
    values = as.numeric(target_xts),
    date = time(target_xts),
    ym = format(time(target_xts), "%Y-%m")
  )
  df_ym <-
    transform(
      df_ym,
      values = ifelse(values > 0 & values < 0.5, round(values, 0), values)
    )

  ym_unique <- unique(df_ym$ym)

  if (length(ym_unique) < (12 * 5)) {

    rm_dates <- NULL

  } else {

    ym_unique_cut <- ym_unique

    subseq_m <-
      lapply(
        seq_along(ym_unique_cut),
        function(x) {

          seg_to_check <- ym_unique[x:(x + Nmonths)] # Nmonths later
          seg_target <- df_ym[df_ym$ym %in% seg_to_check[1], ]$values
          seq_eval <- df_ym[
            df_ym$ym %in% seg_to_check[2:length(seg_to_check)],
          ]

          size_na_seg_target <- length(seg_target[!is.na(seg_target)])
          size_na_seq_eval <- seq_eval$values
          size_na_seq_eval <- length(size_na_seq_eval[!is.na(size_na_seq_eval)])

          if (size_na_seg_target < 10 | size_na_seq_eval < 10) {

            rep_values_dates <- NA

          } else {

            # here we compute the moving correlation with specific parameters
            rep_values_dates <- df_win_mov_cor(xva = seg_target,
                                               ydf = seq_eval)


          }

          rep_values_dates <- rep_values_dates[!is.na(rep_values_dates)]

          list(
            date_eva = as.character(
              df_ym[df_ym$ym %in% seg_to_check[1], ]$date
            ),
            date_rep = as.character(rep_values_dates)
          )
        }
      )

    subseq_m <- subseq_m[lapply(subseq_m, function(x) length(x$date_rep)) != 0]
    subseq_m_a <- lapply(subseq_m, function(x) as.character(x$date_rep))
    subseq_m_a <- unlist(subseq_m_a)
    subseq_m_b <- lapply(subseq_m, function(x) as.character(x$date_eva))
    subseq_m_b <- unlist(subseq_m_b)

    rm_dates <- unique(c(subseq_m_a, subseq_m_b))
    rm_dates <- as.Date(rm_dates)
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

df_win_mov_cor <- function(xva,
                           ydf,
                           params = list(n_splits_ = 10,
                                         r_val_ = 0.3,
                                         n_same_ = 10)) {

  nsplits <- seq_len(nrow(ydf))
  nsplits <- split(
    nsplits,
    ceiling(seq_along(nsplits) / params$n_splits_)
  )
  nsplits <- sapply(nsplits, function(izz) izz[1])

  out <-
    lapply(
      nsplits,
      function(idx) {

        ydf_j <- ydf[idx:(length(xva) + idx - 1), ]
        r_j <- suppressWarnings(cor(ydf_j$values, xva))

        if (is.na(r_j) | r_j < params$r_val_) {

          n_same_values <- 0

        } else {

          n_same_values_df <- data.frame(ydf_j,
                                         target =  xva)
          # is0 is to ensure that both values are not zero
          # (we considered zero, values that are less than 0.5)
          # (we defined this in the function that calls this one)
          # if both values are zero, they are not considered as the same
          n_same_values_df <- transform(n_same_values_df,
                                        dif = values - target,
                                        is0 = (values + target) == 0)
          n_same_values_df <- n_same_values_df[
            n_same_values_df$is0 == FALSE,
          ]
          n_same_values_df <- n_same_values_df[
            n_same_values_df$dif == 0,
          ]
          n_same_values_df <- n_same_values_df[
            complete.cases(n_same_values_df),
          ]

          n_same_values <- length(n_same_values_df$values)

        }

        # if the number of same values is greater than the threshold
        # we consider that the values are the same, within the whole month
        if (n_same_values >= params$n_same_) {

          df_ym_res <- as.character(ydf_j$date)

        } else {

          df_ym_res <- NA

        }

        df_ym_res

      }
    )
  out <- unlist(out)

  return(out)
}
