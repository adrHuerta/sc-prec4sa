
fillData_glm <- function(values_data,
                         predictors_data,
                         covars,
                         set2naOR0 = FALSE) {

  full_data <- predictors_data
  full_data$val <- as.numeric(values_data)

  can <- full_data[1, ]
  ref <- full_data[-1, ]
  ref <- ref[complete.cases(ref), ]

  rr <- ref
  rr$val[rr$val > 0] <- 1

  if (isTRUE(set2naOR0)) {

    out <- data.frame(obs = can$val,
                      wd_pred = NA,
                      raw_pred = NA,
                      mod_pred = NA,
                      err = NA,
                      n_stations = length(rr$val))

  } else if (set2naOR0 == "0") {

    out <- data.frame(obs = can$val,
                      wd_pred = unique(rr$val),
                      raw_pred = unique(ref$val),
                      mod_pred = unique(ref$val),
                      err = 0,
                      n_stations = length(rr$val))

  } else {

    f <- as.formula(
      paste("val ~ ", paste(covars, collapse = " + "), collapse = " ")
    )
    fmtb <- suppressWarnings(
      glm(f, family = binomial(), data = rr)
    )

    pb <- round(predict(fmtb,newdata = can,
                        type = "response"), 2)

    #rescaling
    rr <- ref
    MINc <- min(rr$val) -
      (as.numeric(quantile(rr$val, 0.50)) - as.numeric(quantile(rr$val, 0.25)))
    MINc <- ifelse(MINc < 0, 0, MINc)
    MAXc <- max(rr$val) +
      (as.numeric(quantile(rr$val, 0.75))- as.numeric(quantile(rr$val, 0.50)))
    RANGE <- as.numeric(MAXc - MINc)
    rr$val <- (rr$val - MINc) / RANGE

    fmt <- suppressWarnings(
      glm(f, family = quasibinomial(), data = rr)
    )

    p <- predict(fmt, newdata = can, type = "response")
    p <- round((p * RANGE) + MINc, 2)

    # error calculation
    e <- sqrt(
      sum((rr$val - predict(fmt, type = "response")) ^ 2) /
        (length(rr$val) - length(covars))
    )
    e <- round((e * RANGE) + MINc, 2)

    out <- data.frame(obs = can$val,
                      wd_pred = pb,
                      raw_pred = p,
                      mod_pred = ifelse(pb <= 0.5, 0, p),
                      err = e,
                      n_stations = length(rr$val))

  }

  return(out)

}


fillData_svm <- function(values_data,
                         predictors_data,
                         covars,
                         set2naOR0 = FALSE) {

  full_data <- predictors_data
  full_data$val <- as.numeric(values_data)

  can <- full_data[1, ]
  ref <- full_data[-1, ]
  ref <- ref[complete.cases(ref), ]

  rr <- ref
  rr$val[rr$val > 0] <- 1
  rr$val <- factor(rr$val, levels = c(0, 1))

  if (isTRUE(set2naOR0)) {

    out <- data.frame(obs = can$val,
                      wd_pred = NA,
                      raw_pred = NA,
                      mod_pred = NA,
                      err = NA,
                      n_stations = length(rr$val))

  } else if (set2naOR0 == "0") {

    out <- data.frame(obs = can$val,
                      wd_pred = unique(rr$val),
                      raw_pred = unique(ref$val),
                      mod_pred = unique(ref$val),
                      err = 0,
                      n_stations = length(rr$val))

  } else {

    f <- as.formula(
      paste("val ~ ", paste(covars, collapse = " + "), collapse = " ")
    )

    if (length(unique(rr$val)) != 1) {

      set.seed(123)
      fmtb <- e1071::svm(f, data = rr[c("val", covars)], kernel = "radial",
                         scale = FALSE,
                         type = "C-classification",
                         probability = TRUE)
      pb <- predict(fmtb, can[, covars], probability = TRUE)
      pb <- as.data.frame(attr(pb, "probabilities"))
      pb <- pb[, "1"]

    } else {

      pb <- as.numeric(levels(unique(rr$val)))[unique(rr$val)]

    }

    #rescaling
    rr <- ref
    MINc <- min(rr$val) -
      (as.numeric(quantile(rr$val, 0.50)) - as.numeric(quantile(rr$val, 0.25)))
    MINc <- ifelse(MINc < 0, 0, MINc)
    MAXc <- max(rr$val) +
      (as.numeric(quantile(rr$val, 0.75))-as.numeric(quantile(rr$val, 0.50)))
    RANGE <- as.numeric(MAXc - MINc)
    rr$val <- (rr$val - MINc) / RANGE

    f <- as.formula(
      paste("val ~ ", paste(covars, collapse = " + "), collapse = " ")
    )
    set.seed(123)
    fmt <- e1071::svm(f, data = rr[c("val", covars)], kernel = "radial",
                      scale = FALSE,
                      type = "nu-regression",
                      nu = 0.1) # nu > 0.5 negative values in PAD
    p <- predict(fmt, newdata = can[, covars], type = "response")
    p <- round((p * RANGE) + MINc, 2)

    # error calculation
    e <- sqrt(
      sum(
          (rr$val - predict(fmt,
                            newdata = rr[c("val", covars)],
                            type = "response")) ^ 2) /
        (length(rr$val) - length(covars))
    )
    e <- round((e * RANGE) + MINc, 2)

    out <- data.frame(obs = can$val,
                      wd_pred = pb,
                      raw_pred = p,
                      mod_pred = ifelse(pb <= 0.5, 0, p),
                      err = e,
                      n_stations = length(rr$val))

  }

  return(out)

}


fillData_rf <- function(values_data,
                        predictors_data,
                        covars,
                        set2naOR0 = FALSE) {

  full_data <- predictors_data
  full_data$val <- as.numeric(values_data)

  can <- full_data[1, ]
  ref <- full_data[-1, ]
  ref <- ref[complete.cases(ref), ]

  rr <- ref
  rr$val[rr$val > 0] <- 1
  rr$val <- factor(rr$val, levels = c(0, 1))

  if (isTRUE(set2naOR0)) {

    out <- data.frame(obs = can$val,
                      wd_pred = NA,
                      raw_pred = NA,
                      mod_pred = NA,
                      err = NA,
                      n_stations = length(rr$val))

  } else if (set2naOR0 == "0") {

    out <- data.frame(obs = can$val,
                      wd_pred = unique(rr$val),
                      raw_pred = unique(ref$val),
                      mod_pred = unique(ref$val),
                      err = 0,
                      n_stations = length(rr$val))

  } else {

    f <- as.formula(
      paste("val ~ ", paste(covars, collapse = " + "), collapse = " ")
    )

    if (length(unique(rr$val)) != 1) {

      set.seed(123)
      fmtb <- randomForest::randomForest(f,
                                         data = rr[, c("val", covars)],
                                         type = "classification")
      pb <- predict(fmtb, can[, covars], type = "prob")
      pb <- data.frame(pb)
      pb <- pb[, "X1"]

    } else {

      pb <- as.numeric(levels(unique(rr$val)))[unique(rr$val)]

    }

    #rescaling
    rr <- ref
    MINc <- min(rr$val) -
      (as.numeric(quantile(rr$val, 0.50)) - as.numeric(quantile(rr$val, 0.25)))
    MINc <- ifelse(MINc < 0, 0, MINc)
    MAXc <- max(rr$val) +
      (as.numeric(quantile(rr$val, 0.75)) - as.numeric(quantile(rr$val, 0.50)))
    RANGE <- as.numeric(MAXc - MINc)
    rr$val <- (rr$val - MINc) / RANGE

    set.seed(123)
    fmt <- randomForest::randomForest(f,
                                      data = rr[, c("val", covars)],
                                      type = "regression")
    p <- predict(fmt, newdata = can, type = "response")
    p <- round((p * RANGE) + MINc, 2)

    # error calculation
    e <- sqrt(
      sum(
        (rr$val - predict(fmt,
                          newdata = rr[, c("val", covars)],
                          type = "response")) ^ 2
      ) /
        (length(rr$val) - length(covars))
    )
    e <- round((e * RANGE) + MINc, 2)

    out <- data.frame(obs = can$val,
                      wd_pred = pb,
                      raw_pred = p,
                      mod_pred = ifelse(pb <= 0.5, 0, p),
                      err = e,
                      n_stations = length(rr$val))

  }

  return(out)

}


fillData_xgboost <- function(values_data,
                             predictors_data,
                             covars,
                             set2naOR0 = FALSE) {

  full_data <- predictors_data
  full_data$val <- as.numeric(values_data)

  can <- full_data[1, ]
  ref <- full_data[-1, ]
  ref <- ref[complete.cases(ref), ]

  rr <- ref
  rr$val[rr$val > 0] <- 1
  rr$val <- factor(rr$val, levels = c(0, 1), labels = c("dd", "wd"))

  if (isTRUE(set2naOR0)) {

    out <- data.frame(obs = can$val,
                      wd_pred = NA,
                      raw_pred = NA,
                      mod_pred = NA,
                      err = NA,
                      n_stations = length(rr$val))

  } else if (set2naOR0 == "0") {

    out <- data.frame(obs = can$val,
                      wd_pred = unique(rr$val),
                      raw_pred = unique(ref$val),
                      mod_pred = unique(ref$val),
                      err = 0,
                      n_stations = length(rr$val))

  } else {

    f <- as.formula(
      paste(paste(covars, collapse = " + "), "~ val", collapse = " ")
    )
    rr <- reshape2::dcast(
      data = rr[, c(covars, "val")],
      f,
      length,
      value.var = "val"
    )

    set.seed(123)
    fmb <- xgboost::xgboost(data = as.matrix(rr[, covars]),
                            label = rr$wd,
                            verbose = 0, nthread = 1,
                            objective = "binary:logistic",
                            nrounds = 5)

    pb <- predict(fmb, newdata = as.matrix(can[, covars]), type = "prob")
    pb <- as.numeric(pb)

    #rescaling
    rr <- ref
    MINc <- min(rr$val) -
      (as.numeric(quantile(rr$val, 0.50)) - as.numeric(quantile(rr$val, 0.25)))
    MINc <- ifelse(MINc < 0, 0, MINc)
    MAXc <- max(rr$val) +
      (as.numeric(quantile(rr$val, 0.75)) - as.numeric(quantile(rr$val, 0.50)))
    RANGE <- as.numeric(MAXc - MINc)
    rr$val <- (rr$val - MINc) / RANGE

    set.seed(123)
    fmt <- xgboost::xgboost(data = as.matrix(rr[, covars]),
                            label = rr$val,
                            objective = "reg:squarederror",
                            verbose = 0, nthread = 1,
                            nrounds = 5)

    p <- predict(fmt, newdata = as.matrix(can[, covars]), type = "response")
    p <- round((p * RANGE) + MINc, 2)

    # error calculation
    e <- sqrt(
      sum(
        (rr$val - predict(fmt,
                          newdata = as.matrix(rr[, covars]),
                          type = "response",
                          verbose = 0)) ^ 2
      ) /
        (length(rr$val) - length(covars))
    )
    e <- round((e * RANGE) + MINc, 2)

    out <- data.frame(obs = can$val,
                      wd_pred = pb,
                      raw_pred = p,
                      mod_pred = ifelse(pb <= 0.5, 0, p),
                      err = e,
                      n_stations = length(rr$val))

  }

  return(out)

}


fillData_nn <- function(values_data,
                        predictors_data,
                        covars,
                        set2naOR0 = FALSE) {

  full_data <- predictors_data
  full_data$val <- as.numeric(values_data)

  can <- full_data[1, ]
  ref <- full_data[-1, ]
  ref <- ref[complete.cases(ref), ]

  rr <- ref
  rr$val[rr$val > 0] <- 1
  rr$val <- factor(rr$val, levels = c(0, 1))

  if (isTRUE(set2naOR0)) {

    out <- data.frame(obs = can$val,
                      wd_pred = NA,
                      raw_pred = NA,
                      mod_pred = NA,
                      err = NA,
                      n_stations = length(rr$val))

  } else if (set2naOR0 == "0") {

    out <- data.frame(obs = can$val,
                      wd_pred = unique(rr$val),
                      raw_pred = unique(ref$val),
                      mod_pred = unique(ref$val),
                      err = 0,
                      n_stations = length(rr$val))

  } else {

    f <- as.formula(
      paste("val ~", paste(covars, collapse = " + "), collapse = " ")
    )

    if (length(unique(rr$val)) != 1) {

      set.seed(123)
      fmtb <- neuralnet::neuralnet(
        f,
        data = rr[, c("val", covars)],
        linear.output = FALSE,
        hidden = c(4, 2),
        stepmax = 1e6,
        threshold = 0.75
      ) # threshold worked well with 0.5 in CH, changed to 0.75 by MP-SA
      pb <- predict(fmtb, can[, covars])
      pb <- pb[, 2]

    } else {

      pb <- as.numeric(levels(unique(rr$val)))[unique(rr$val)]

    }

    #rescaling
    rr <- ref
    MINc <- min(rr$val) -
      (as.numeric(quantile(rr$val, 0.50)) - as.numeric(quantile(rr$val, 0.25)))
    MINc <- ifelse(MINc < 0, 0, MINc)
    MAXc <- max(rr$val) +
      (as.numeric(quantile(rr$val, 0.75))- as.numeric(quantile(rr$val, 0.50)))
    RANGE <- as.numeric(MAXc - MINc)
    rr$val <- (rr$val - MINc) / RANGE

    set.seed(123)
    fmt <- neuralnet::neuralnet(
      f,
      data = rr[, c("val", covars)],
      hidden = c(4, 2),
      stepmax = 1e6,
      # act.fct = custom,
      threshold = 0.75
    ) # stepmax problems with 0.01 (negative values with 0.1)
    # threshold worked well with 0.5 in CH, changed to 0.75 by MP-SA
    p <- predict(fmt, newdata = can, type = "response")
    p <- round((p * RANGE) + MINc, 2)

    # error calculation 
    e <- sqrt(
      sum(
        (rr$val - predict(fmt,
                          newdata = rr[, c("val", covars)],
                          type = "response")) ^ 2
      ) /
        (length(rr$val) - length(covars))
    )
    e <- round((e * RANGE) + MINc, 2)

    out <- data.frame(obs = can$val,
                      wd_pred = pb,
                      raw_pred = p,
                      mod_pred = ifelse(pb <= 0.5, 0, p),
                      err = e,
                      n_stations = length(rr$val))

  }

  return(out)

}