Mode <- function(x){

  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]

}

get_dec_from_xts <- function(xts_obj,
                             removeNAs = TRUE) {

  if (dim(xts_obj)[1] < 1) {

    data.frame(year = NA, dec = NA)

  } else {

    w <- abs(xts_obj)
    w <- (round(w, 1) - as.integer(round(w, 1)))
    w <- as.integer(w * 10)

    out_df <- data.frame(
      year =  as.numeric(format(time(xts_obj), "%Y")),
      dec = factor(
        w,
        levels = seq(9, 0, -1),
        labels = paste("x.",
                       seq(9, 0, -1),
                       sep = "")
      )
    )

    if (isTRUE(removeNAs)) {

      return(out_df[complete.cases(out_df), ])

    } else {

      return(out_df)

    }

  }

}

get_ndec <- function(xts_obj,
                     lmn_yday = 365 * 80 / 100) {

  w <- abs(xts_obj)
  w <- (round(w, 1) - as.integer(round(w, 1)))
  w <- as.integer(w * 10)

  out_df <- data.frame(
    year = as.numeric(format(time(xts_obj), "%Y")),
    dec = factor(
      w,
      levels = seq(9, 0, -1),
      labels = paste("x.", seq(9, 0, -1), sep = "")
    )
  )

  out_df <- out_df[complete.cases(out_df), ]

  out  <- reshape2::dcast(out_df, year ~ dec,
                          value.var = "dec",
                          fun.aggregate = length)

  if (ncol(out) > 2) {

    out$size <- rowSums(out[,-1])

  } else {

    out$size <- out[,2]

  }

  out <- out[out$size > lmn_yday, ]
  return(out)

}


get_dec_patterns <- function(xts_obj) {

  out <- get_ndec(xts_obj = xts_obj)
  out_dec <- out[, -match(c("year", "size"), colnames(out)), drop = FALSE]
  out_dec[out_dec > 1] <- 1
  out_dec[out_dec == 0] <- NA

  out$pattern_shape <- sapply(1:nrow(out_dec), function(idd) {

    idd_d <- out_dec[idd,] * as.numeric(gsub("x.", "", colnames(out_dec)))
    idd_d <- unlist(idd_d)
    idd_d <- idd_d[!is.na(idd_d)]
    paste0(idd_d, collapse = ".")

  })

  out$pattern_lenght <- sapply(1:nrow(out_dec), function(idd) {

    idd_d <- out_dec[idd,] * as.numeric(gsub("x.", "", colnames(out_dec)))
    idd_d <- unlist(idd_d)
    idd_d <- idd_d[!is.na(idd_d)]
    length(idd_d)

  })

  out$pattern_shape <- factor(out$pattern_shape)
  # out$pattern_lenght <- factor(out$pattern_lenght)

  return(out)

}

get_dec_patterns_mode <- function(xts_obj) {

  out <- get_dec_patterns(xts_obj)
  out <- length(out$pattern_lenght[out$pattern_lenght <= 5]) /
    length(out$pattern_lenght)
  out <- out * 100
  # out <- median(out$pattern_lenght)

  return(out)

}

get_levels_rpp <- function(xts_obj) {

  out <- get_dec_patterns(xts_obj = xts_obj)
  pattern_shape_types <- summary(out$pattern_shape)
  pattern_shape_mode <- max(pattern_shape_types)

  if (pattern_shape_mode >= round(nrow(out) * 70 / 100)) {

    response <- 0

  } else if (pattern_shape_mode >= round(nrow(out) * 50 / 100)) {

    response <- 1

  } else {

    response <- 2

  }

  return(response)

}


apply_e_rpp <- function(xts_obj) {

  levels_rpp <- get_levels_rpp(xts_obj = xts_obj)

  qc_data <- qc_data_flagged <- xts_obj
  qc_data_flagged[!is.na(qc_data_flagged)] <- levels_rpp

  out <- list(
    qc_data = qc_data,
    qc_data_flagged = qc_data_flagged
  )

  return(out)

}