rm(list = ls())

suppressMessages(library(xts))
source("R/utils/pipe.R")
source("R/utils/get_nearby_points.R")
source("R/04_homogenization/detec-correc-helpers.R")
source("R/04_homogenization/detec-cleaning.R")
source("R/04_homogenization/detec-tests.R")
source("R/04_homogenization/correc-tests.R")
source("R/04_homogenization/detec-correc-workflow.R")


raw_data <- readRDS(
  file.path(
    "data",
    "processed",
    "point",
    "datos_SA_1960-2015_v1.2_04_obs-bc.RDS"
  )
)

## stages for homogenization
param_spt <- list(list(lmt_xy = 1e+6,
                       lmt_n = 8),
                  list(lmt_xy = 1e+6,
                       lmt_n = 8),
                  list(lmt_xy = 1e+6,
                       lmt_n = 8))
## ecoregions
ecr <- rev(c("NAS", "PAD", "CAS", "SAS", "AOL", "EHL", "GCH", "PPS", "MPN"))


### obs_mod
hmg_results_path <- file.path(
  "output", "04_homogenization", "obs_bc"
)

#### ecoregion
for (ecr_i in ecr) {

  idd_s <- raw_data$xyz[
    !is.na(raw_data$xyz$EQC) &
      raw_data$xyz$EQC == 1 &
      raw_data$xyz$ECOREGIONS == ecr_i,
  ]$ID

  raw_data_data <- raw_data$data[, idd_s]
  raw_data_xyz <- raw_data$xyz[match(idd_s, raw_data$xyz$ID), ]

  print(ecr_i)
  print(length(idd_s))
  ecr_i_path <- file.path(hmg_results_path, ecr_i)
  dir.create(ecr_i_path, showWarnings = FALSE, recursive = TRUE)

  data_tobe_homogenized <- raw_data_data

  #### stages
  for (stage in seq_along(param_spt)) {

    print(stage)
    ecr_i_stage_path <- file.path(ecr_i_path, stage)
    dir.create(ecr_i_stage_path, showWarnings = FALSE, recursive = TRUE)

    #### stations
    parallel::mclapply(
      seq_along(idd_s),
      function(id_id) {

        get_nearby_points(xy_target = idd_s[id_id],
                          xy_database = raw_data_xyz,
                          lmt_xy = param_spt[[stage]]$lmt_xy,
                          lmt_n = param_spt[[stage]]$lmt_n) %>%

          build_matrix_hmg(stations = .,
                           xts_database = data_tobe_homogenized) %>%

          compute_indices(target_data = .) %>%

          detection_test(target_data = .) %>%

          homogenization_correction(target_data = .) %>%

          qc_after_hmg(target_data = .) %>%

          saveRDS(
            object = .,
            file.path(ecr_i_stage_path,
                      paste(formatC(id_id, width = 4, format = "d", flag = "0"),
                            "_",
                            idd_s[id_id],
                            ".RDS",
                            sep = ""))
          )

      }, mc.cores = 150
    )

    temp_stage_raw <- dir(ecr_i_stage_path, full.names = TRUE)

    if (stage > 2) {

      next

    } else {

      temp_stage_raw <- parallel::mclapply(
        seq_along(temp_stage_raw),
        function(idx) {

          obs_pred <- readRDS(temp_stage_raw[idx])
          obs_pred <- obs_pred$hmg_time_serie

        }, mc.cores = 150
      )

      temp_stage_raw <- do.call(cbind, temp_stage_raw)
      colnames(temp_stage_raw) <- idd_s
      data_tobe_homogenized[, idd_s] <- temp_stage_raw

      rm(temp_stage_raw)
      gc()

    }

  }

  rm(data_tobe_homogenized)
  gc()

}
