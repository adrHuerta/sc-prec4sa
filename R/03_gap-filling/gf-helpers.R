
standardization <- function(obs, sim, dates, window){

  days <- substr(dates, 6, 10)

  pred_st <- rep(NA, length = length(sim))

  for (i in 1:length(obs)) {

    # select days within the window
    ini <- (i - (window - 1) / 2)
    if (ini < 1) ini <- 1
    end <- (i + (window - 1) / 2)
    if (end > length(obs)) end <- length(obs)
    dd <- dates[ini:end]
    # get data from all days
    m <- which(days %in% substr(dd, 6, 10))
    wd <- which(dates[i] == dates[m])
    o <- obs[m]
    s <- sim[m]
    rr <- stand_qq(o, s)
    pred_st[i] <- rr[wd]

  }

  return(pred_st)

}

stand_qq <- function(o, s){
  suppressMessages(library(qmap))

  w0 <- which(s == 0)
  ww <- which((o + s) != 0)
  sww <- var(s[ww])
  oww <- var(s[ww])

  if (length(ww) < 5 | sww == 0 | oww == 0) return(s) else {
    qm.fit <- fitQmap(o[ww],
                      s[ww],
                      method = "QUANT",
                      wet.day = FALSE,
                      qstep = 0.1)
    # wet.day was not set before (creating fake automatic wet.day)
    # wet.day = 0 = FALSE same
    w <- which(!is.na(s))

    if (length(w) < length(s)) {

      xx <- doQmap(s[w], qm.fit)
      s[w] <- xx

    } else {

      s <- doQmap(s, qm.fit)

    }

    s[w0] <- 0

    return(round(s, 2))

  }
}


bias_correction_reddPrec <- function(target_data, window = 15) {

  obs_s <- as.numeric(target_data$obs)
  sim_s <- as.numeric(target_data$mod_pred)
  window_s <- window

  rr <- standardization(obs = obs_s,
                        sim = sim_s,
                        dates = target_data$time_step,
                        window = window_s)
  target_data$bc_pred <- rr
  target_data <- target_data[,
    c("time_step",
      "obs",
      "wd_pred",
      "raw_pred",
      "mod_pred",
      "bc_pred",
      "err",
      "n_stations")
  ]

  return(target_data)

}
