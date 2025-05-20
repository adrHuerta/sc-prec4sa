rm(list = ls())

suppressMessages(library(xts))
source("R/utils/pipe.R")
source("R/utils/get_nearby_points.R")
source("R/03_gap-filling/gf-machinery.R")
source("R/03_gap-filling/gf-models.R")
source("R/03_gap-filling/gf-helpers.R")
source("R/03_gap-filling/gf-metrics.R")


raw_data <- readRDS(
  file.path(
    "data",
    "processed",
    "point",
    "datos_SA_1960-2015_v1.2_03_tc.RDS"
  )
)

era5land_data <- readRDS(
  file.path(
    "data",
    "processed",
    "point",
    "datos_era5land_ecoregion_sa_qmap.RDS"
  )
)

raw_data <- list(
  data = cbind(raw_data$data, era5land_data$data),
  xyz = rbind(raw_data$xyz, era5land_data$xyz)
)

# gap-filling

## PCA covariables
pca_cov <- raw_data$xyz[,
  -match(c("ID", "NAME", "LON", "LAT", "ALT",
           "COUNTRY", "SOURCE", "ECOREGIONS",
           "SIZE", "EQC", "elevation", "latitude", "longitude"),
         colnames(raw_data$xyz))
]
pca_cov <- pca_cov[,
  -match(c("vrm", "tri", "tpi", "slope"),
         colnames(pca_cov))
]
pca_cov <- prcomp(scale(pca_cov))
pca_cov <- pca_cov$x[, 1:4]

new_sts <- raw_data$xyz[, c("ID", "LON", "LAT", "ALT")]
new_sts <- cbind(new_sts, pca_cov)


## stages for gap-filling
param_spt <- list(list(lmt_xy = 175 * 1000,
                       lmt_n = 16),
                  list(lmt_xy = 275 * 1000,
                       lmt_n = 16),
                  list(lmt_xy = 650 * 1000,
                       lmt_n = 16))

## ecoregions
ecr <- rev(c("NAS", "PAD", "CAS", "SAS", "AOL", "EHL", "GCH", "PPS", "MPN"))

### mod_pred
gf_results_path <- file.path(
  "output", "03_gap-filling", "mod_pred"
)

#### ecoregion
for (ecr_i in ecr) {

  idd_ecr_i <- raw_data$xyz[
    !is.na(raw_data$xyz$EQC) &
      raw_data$xyz$EQC == 1 &
      raw_data$xyz$ECOREGIONS == ecr_i,
  ]$ID

  print(ecr_i)
  print(length(idd_ecr_i))
  ecr_i_path <- file.path(gf_results_path, ecr_i)
  dir.create(ecr_i_path, showWarnings = FALSE, recursive = TRUE)

  data_tobe_filled <- raw_data$data

  #### stages
  for (stage in seq_along(param_spt)) {

    print(stage)
    ecr_i_stage_path <- file.path(ecr_i_path, stage)
    dir.create(ecr_i_stage_path, showWarnings = FALSE, recursive = TRUE)

    #### stations
    parallel::mclapply(
      seq_along(idd_ecr_i),
      function(id_id) {

        get_nearby_points(xy_target = idd_ecr_i[id_id],
                          xy_database = new_sts,
                          lmt_xy = param_spt[[stage]]$lmt_xy,
                          lmt_n = NA) %>%

          build_matrix(stations = .,
                       xts_database = data_tobe_filled,
                       xy_database = new_sts,
                       covars = colnames(new_sts)[-1]) %>%

          gap_filling(target_data = .,
                      lmt_min = 8,
                      lmt_max = param_spt[[stage]]$lmt_n,
                      FUN = fillData_xgboost) %>%

          blending(target_data = .) %>%

          saveRDS(
            object = .,
            file.path(ecr_i_stage_path,
                      paste(formatC(id_id, width = 4, format = "d", flag = "0"),
                            "_",
                            idd_ecr_i[id_id],
                            ".RDS",
                            sep = ""))
          )

      }, mc.cores = 150, mc.preschedule	= FALSE
    )

    temp_stage_raw <- dir(ecr_i_stage_path, full.names = TRUE)

    if (stage > 2) {

      next

    } else {

      temp_stage_raw <- parallel::mclapply(
        seq_along(temp_stage_raw),
        function(idx) {

          obs_pred <- readRDS(temp_stage_raw[idx])
          obs_pred <- obs_pred$obs_pred
          xts::xts(obs_pred, time(data_tobe_filled))

        }, mc.cores = 150
      )

      temp_stage_raw <- do.call(cbind, temp_stage_raw)
      colnames(temp_stage_raw) <- idd_ecr_i
      data_tobe_filled[, idd_ecr_i] <- temp_stage_raw

      rm(temp_stage_raw)
      gc()

    }
  }

  rm(data_tobe_filled)
  gc()

}
