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

qc_data <-
  raw_data$data %>%
  parallel::mclapply(function(z) {

    repetition_nonzero_check(target_xts = z)

  }, mc.cores = 100)


saveRDS(
  lapply(qc_data, `[[`, "qc_data_flagged") %>% do.call(cbind, .),
  file.path(
    "output",
    "02_quality-control",
    "sqc-01-repetition-nonzero.RDS"
  )
)

qc_data <- lapply(qc_data, `[[`, "qc_data")


# repetition_zero_check

qc_data <-
  qc_data %>%
  parallel::mclapply(function(z) {

    repetition_zero_check(target_xts = z)

  }, mc.cores = 100)


saveRDS(
  lapply(qc_data, `[[`, "qc_data_flagged") %>% do.call(cbind, .),
  file.path(
    "output",
    "02_quality-control",
    "sqc-02-repetition-zero.RDS"
  )
)

qc_data <- lapply(qc_data, `[[`, "qc_data")


# duplicated_monthly_submonthly_record_check

qc_data <-
  qc_data %>%
  parallel::mclapply(function(z) {

    subyear_dup_monthly_check(target_xts = z)

  }, mc.cores = 100)


saveRDS(
  lapply(qc_data, `[[`, "qc_data_flagged") %>% do.call(cbind, .),
  file.path(
    "output",
    "02_quality-control",
    "sqc-03-dup-sub-monthly.RDS"
  )
)

qc_data <- lapply(qc_data, `[[`, "qc_data")


# nextyear_duplicated_monthly_submonthly_record_check

qc_data <-
  qc_data %>%
  parallel::mclapply(function(z) {

    nextyear_dup_monthly_check(target_xts = z)

  }, mc.cores = 100)


saveRDS(
  lapply(qc_data, `[[`, "qc_data_flagged") %>% do.call(cbind, .),
  file.path(
    "output",
    "02_quality-control",
    "sqc-04-dup-next-year.RDS"
  )
)

qc_data <- lapply(qc_data, `[[`, "qc_data")


# zscore_based_outlier_check
### three times

qc_data <-
  qc_data %>%
  parallel::mclapply(function(z) {

    zscore_based_outlier_check(target_xts = z, out_weight = 9)

  }, mc.cores = 100)

qc_zcore_out_01 <- lapply(qc_data, `[[`, "qc_data_flagged") %>%
  do.call(cbind, .)

qc_data <- lapply(qc_data, `[[`, "qc_data")
qc_data <- do.call(cbind, qc_data)

qc_data <-
  qc_data %>%
  parallel::mclapply(function(z) {

    zscore_based_outlier_check(target_xts = z, out_weight = 9)

  }, mc.cores = 100)

qc_zcore_out_02 <- lapply(qc_data, `[[`, "qc_data_flagged") %>%
  do.call(cbind, .)

qc_data <- lapply(qc_data, `[[`, "qc_data")
qc_data <- do.call(cbind, qc_data)

qc_data <-
  qc_data %>%
  parallel::mclapply(function(z) {

    zscore_based_outlier_check(target_xts = z, out_weight = 9)

  }, mc.cores = 100)

qc_zcore_out_03 <- lapply(qc_data, `[[`, "qc_data_flagged") %>%
  do.call(cbind, .)

qc_zcore_out <- qc_zcore_out_01 + qc_zcore_out_02 + qc_zcore_out_03

saveRDS(
  qc_zcore_out,
  file.path(
    "output",
    "02_quality-control",
    "sqc-05-zscore-outlier.RDS"
  )
)
rm(list = ls(pattern = "qc_zcore_out"))


qc_data <- lapply(qc_data, `[[`, "qc_data")
qc_data <- do.call(cbind, qc_data)


# spatio_temporal_outlier_check

qc_data <-
  names(qc_data) %>% as.character() %>%
  parallel::mclapply(function(z) {

    spatemp_value_check(xy_target = z,
                        xy_database = raw_data$xyz,
                        xts_database = qc_data)

  }, mc.cores = 100)


saveRDS(
  lapply(qc_data, `[[`, "qc_data_flagged") %>% do.call(cbind, .),
  file.path(
    "output",
    "02_quality-control",
    "sqc-06-spatemp_outliers.RDS"
  )
)

qc_data <- lapply(qc_data, `[[`, "qc_data")


# few_dry_values_check

qc_data <-
  qc_data %>%
  parallel::mclapply(function(z) {

    few_dry_values_check(target_xts = z)

  }, mc.cores = 100)


saveRDS(
  lapply(qc_data, `[[`, "qc_data_flagged") %>% do.call(cbind, .),
  file.path(
    "output",
    "02_quality-control",
    "sqc-07-few_dry_values.RDS"
  )
)

qc_data <- lapply(qc_data, `[[`, "qc_data")
qc_data <- do.call(cbind, qc_data)


# defining the longest stations for enhanced quality control

## 1st criteria
## stations with 10 years of data for each day of the year
## (from 1 to 365) # useful for the bias-correction
stations_longest_01 <- data.table::as.data.table(qc_data)
stations_longest_01$index <- format(stations_longest_01$index, "%m-%d")
stations_longest_01 <- stations_longest_01[,
  lapply(.SD, function(x) sum(!is.na(x))), by = index
]
stations_longest_01 <- stations_longest_01[!(index == "02-29"), ]
stations_longest_01 <- stations_longest_01[,
  lapply(.SD, function(x) sum(x >= 10))
]
stations_longest_01 <- unlist(stations_longest_01)[
  unlist(stations_longest_01) >= 365
]

## 2nd criteria
## stations with at least NYEARS consecutive years of data
## a years is considered if it has at least NYEARS days of data
## (we wanted NYEARS = 10, but some ecoregiones (MPN, GCH, PPS)
## will not have enough data)
NDAYS <- 255 # ~ 70% of the days in a year (255 * 100 / 365)
NYEARS <- 5
stations_longest_02 <- data.table::as.data.table(qc_data)
stations_longest_02$index <- format(stations_longest_02$index, "%Y")
stations_longest_02 <- stations_longest_02[,
  lapply(.SD, function(x) ifelse(sum(!is.na(x)) >= NDAYS, 1, 0)), by = index
]
stations_longest_02 <- stations_longest_02[, lapply(.SD, function(x) {

  res <- rle(x)$lengths[which(rle(x)$values %in% 1)]
  any(res[res >= NYEARS])

})]
stations_longest_02 <- unlist(stations_longest_02)[
  unlist(stations_longest_02) == TRUE
]

## Selecting the stations that are in both criteria
stations_longest <- intersect(
  names(stations_longest_01), names(stations_longest_02)
)

raw_data$xyz$SIZE <- 0
raw_data$xyz[match(stations_longest, raw_data$xyz$ID), ]$SIZE <- 1

all(colnames(qc_data) == raw_data$xyz$ID)
raw_data <- list(data = qc_data,
                 xyz = raw_data$xyz)

saveRDS(
  raw_data,
  file.path(
    "data",
    "processed",
    "point",
    "datos_SA_1960-2015_v1.2_02.RDS"
  )
)