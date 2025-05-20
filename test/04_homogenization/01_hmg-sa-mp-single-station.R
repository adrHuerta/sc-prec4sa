rm(list = ls())

library(xts)
source("R/utils/pipe.R")
source("R/utils/get_nearby_points.R")
source("R/04_homogenization/detec-correc-helpers.R")
source("R/04_homogenization/detec-cleaning.R")
source("R/04_homogenization/detec-tests.R")
source("R/04_homogenization/correc-tests.R")
source("R/04_homogenization/detec-correc-workflow.R")


# using obs-bc
raw_data <- readRDS(
  file.path(
    "data",
    "processed",
    "point",
    "datos_SA_1960-2015_v1.2_04_obs-bc.RDS"
  )
)

raw_data_data <- raw_data$data
raw_data_xyz <- raw_data$xyz


# absolute single

## 1° nearby points
## 2° build matrix
ts_target <- get_nearby_points(xy_target = "id_AR_058",
                               xy_database = raw_data_xyz,
                               lmt_xy = 1e+6,
                               lmt_n = 0) %>%
  build_matrix_hmg(stations = .,
                   xts_database = raw_data_data)

## 3° compute indices
ts_target_indices <- compute_indices(target_data = ts_target)
lattice::xyplot(ts_target_indices$indices$prcptot_annual)
lattice::xyplot(ts_target_indices$indices$r1mm_annual)

# we decided not using detrending and prewhitening
# because some obvious inhomogeneities were not detected
# id_AR_058 is a good example (try to apply the break detection
# using the indices and you will see that the break is not detected)
preprocessing_clean(time_serie = ts_target_indices$indices$prcptot_annual) %>%
  lattice::xyplot()

preprocessing_clean(time_serie = ts_target_indices$indices$r1mm_annual) %>%
  lattice::xyplot()

## 4° apply break detection
apply_break_detection(time_series = ts_target_indices$indices$prcptot_annual)
apply_break_detection(time_series = ts_target_indices$indices$r1mm_annual)

ts_target_detection <- detection_test(target_data = ts_target_indices)

## 5° apply correction
ts_target_corrected <- apply_absolute_correction(
  target = ts_target_detection$original,
  year_of_break = ts_target_detection$break_year
)

lattice::xyplot(ts_target_detection$original, type = "p", cex = .1)
lattice::xyplot(ts_target_corrected, type = "p", cex = .1)


# relative

## 1° nearby points
## 2° build matrix
ts_target <- get_nearby_points(xy_target = "id_AR_058",
                               xy_database = raw_data_xyz,
                               lmt_xy = 1e+6,
                               lmt_n = 8) %>%
  build_matrix_hmg(stations = .,
                   xts_database = raw_data_data)

## 3° compute indices
ts_target_indices <- compute_indices(target_data = ts_target)
lattice::xyplot(ts_target_indices$indices$prcptot_annual)
lattice::xyplot(ts_target_indices$indices$r1mm_annual)

## same as before, we did not apply the detrending and prewhitening
lattice::xyplot(lapply(1:8, function(x) {

  ts_target_indices$indices$r1mm_annual[, 1] -
    ts_target_indices$indices$r1mm_annual[, 1 + x]

}) %>% do.call(cbind, .))

lattice::xyplot(lapply(1:8, function(x) {

  (ts_target_indices$indices$r1mm_annual[, 1] -
     ts_target_indices$indices$r1mm_annual[, 1 + x]) %>%
    preprocessing_clean(time_serie = .)

}) %>% do.call(cbind, .))

lapply(1:8, function(x) {

  (ts_target_indices$indices$r1mm_annual[, 1] -
     ts_target_indices$indices$r1mm_annual[, 1 + x]) %>%
    preprocessing_clean(time_serie = .) %>%
    apply_break_detection(time_serie = .)

})

## 4° apply break detection
ts_target_detection <- detection_test(target_data = ts_target_indices)

## 5° apply correction
ts_target_corrected <- apply_relative_correction(
  target = ts_target_detection$original[, 1],
  nearby =  ts_target_detection$original[, -1],
  year_of_break = ts_target_detection$break_year
)

lattice::xyplot(ts_target_detection$original[, 1], type = "p", cex = .1)
lattice::xyplot(ts_target_corrected, type = "p", cex = .1)
