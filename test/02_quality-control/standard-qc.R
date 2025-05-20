rm(list = ls())

source("R/utils/pipe.R")
source("R/utils/get_nearby_points.R")
source("R/02_quality-control/repetition-check.R")
source("R/02_quality-control/duplicated-values-check.R")
source("R/02_quality-control/zscore-outlier-check.R")
source("R/02_quality-control/spatio-temporal-check.R")
source("R/02_quality-control/few-dry-values-check.R")


raw_data <- readRDS(
  file.path(
    "data",
    "processed",
    "point",
    "datos_SA_1960-2015_v1.2_01.RDS"
  )
)

raw_data$data <- xts::xts(
  raw_data$data,
  seq(as.Date("1960-01-01"), as.Date("2015-12-31"), by = "day")
)


# repetition_nonzero_check

qc_data_01 <- repetition_nonzero_check(
  target_xts = raw_data$data[, "id_21201130"]
)

qc_data_01_time <- qc_data_01 %>%
  .$qc_data_flagged %>%
  .[. == 1] %>%
  time() %>%
  format("%Y-%m") %>%
  unique()

lattice::xyplot(
  cbind(raw_data$data[, "id_21201130"],
        qc_data_01$qc_data) %>%
    .[qc_data_01_time]
)


# repetition_zero_check

qc_data_02 <- repetition_zero_check(
  target_xts = qc_data_01$qc_data
)

qc_data_02_time <- qc_data_02 %>%
  .$qc_data_flagged %>%
  .[. == 1] %>%
  time() %>%
  format("%Y-%m") %>%
  unique()

lattice::xyplot(
  cbind(qc_data_02$qc_data[, "id_21201130"],
        qc_data_02$qc_data) %>%
    .[qc_data_02_time]
)


# duplicated_monthly_submonthly_record_check

qc_data_03_raw <- raw_data$data[, "id_21205920"]
qc_data_03_raw["2001-08"] <- qc_data_03_raw["2001-07"]
qc_data_03_raw["2001-10"] <- qc_data_03_raw["2001-07"]

qc_data_03 <- subyear_dup_monthly_check(
  target_xts = qc_data_03_raw
)

qc_data_03_time <- qc_data_03 %>%
  .$qc_data_flagged %>%
  .[. == 1] %>%
  time() %>%
  format("%Y-%m") %>%
  unique()

lattice::xyplot(
  cbind(qc_data_03_raw,
        qc_data_03$qc_data) %>%
    .[qc_data_03_time]
)


# nextyear_duplicated_monthly_submonthly_record_check

qc_data_04 <- nextyear_dup_monthly_check(
  target_xts = raw_data$data[, "id_21205920"]
)

qc_data_04_time <- qc_data_04 %>%
  .$qc_data_flagged %>%
  .[. == 1] %>%
  time() %>%
  format("%Y-%m") %>%
  unique()

lattice::xyplot(
  cbind(raw_data$data[, "id_21205920"],
        qc_data_04$qc_data) %>%
    .[qc_data_04_time]
)


# zscore_based_outlier_check

qc_data_05 <- zscore_based_outlier_check(
  target_xts = raw_data$data[, "id_21206260"]
)

qc_data_05_time <- qc_data_05 %>%
  .$qc_data_flagged %>%
  .[. == 1] %>%
  time() %>%
  format("%Y-%m") %>%
  unique()

lattice::xyplot(
  cbind(raw_data$data[, "id_21206260"],
        qc_data_05$qc_data) %>%
    .[qc_data_05_time]
)


# spatio_temporal_outlier_check

fake_raw_data <- raw_data$data
fake_raw_data[, "id_21206260"]["2001-08-01"] <- 1000

## it requires at least 5 nearby points to compare (otherwise it will not work)

get_nearby_points(xy_target = "id_21206260",
                  xy_database = raw_data$xyz,
                  lmt_xy = 400000,
                  lmt_n = 10) %>%
  fake_raw_data[, .] %>%
  .["2001-08-01"]

## it requires also the previous/next values to compare (at the target point)

fake_raw_data[, "id_21206260"][
  as.Date("2001-08-01") + c(-1, 0, 1)
]

##
qc_data_06 <- spatemp_value_check(
  xy_target = "id_21206260",
  xy_database = raw_data$xyz,
  xts_database = fake_raw_data
)

qc_data_06_time <- qc_data_06 %>%
  .$qc_data_flagged %>%
  .[. == 1] %>%
  time() %>%
  format("%Y-%m") %>%
  unique()

lattice::xyplot(
  cbind(fake_raw_data[, "id_21206260"],
        qc_data_06$qc_data) %>%
    .[qc_data_06_time]
)


# few_dry_values_check

qc_data_07_raw <- raw_data$data[, "id_21205920"]
zoo::coredata(qc_data_07_raw) <- sample(1:10,
                                        length(qc_data_07_raw),
                                        replace = TRUE)

qc_data_07 <- few_dry_values_check(
  target_xts = qc_data_07_raw
)

# this test flag all the dates is the tresold is reached
# i.e. it remove the station
qc_data_07_time <- qc_data_07 %>%
  .$qc_data_flagged %>%
  .[. == 1] %>%
  time() %>%
  format("%Y-%m") %>%
  unique()

lattice::xyplot(
  cbind(qc_data_07_raw,
        qc_data_07$qc_data) %>%
    .[qc_data_07_time]
)
