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

## ecoregions
ecr <- unique(raw_data$xyz$ECOREGIONS)

### mod_pred final stage results
mod_pred_path <- file.path(
  "output", "03_gap-filling", "mod_pred"
)

### bc_pred results
bc_pred_path <- file.path(
  "output", "03_gap-filling", "bc_pred"
)

ecr <- c("NAS", "PAD", "CAS", "SAS", "AOL", "EHL", "GCH", "PPS", "MPN")


for (ecr_i in ecr) {

  idd_ecr_i <- raw_data$xyz[
    !is.na(raw_data$xyz$EQC) &
      raw_data$xyz$EQC == 1 &
      raw_data$xyz$ECOREGIONS == ecr_i,
  ]$ID

  print(ecr_i)
  print(length(idd_ecr_i))
  print(4)


  ecr_i_path_bc_pred <- file.path(bc_pred_path, ecr_i)
  dir.create(ecr_i_path_bc_pred, showWarnings = FALSE, recursive = TRUE)

  ecr_i_path_mod_pred <- file.path(mod_pred_path, ecr_i, 3)
  ecr_i_path_mod_pred_id <- dir(ecr_i_path_mod_pred, full.names = TRUE)
  cbind(ecr_i_path_mod_pred_id, idd_ecr_i)

  parallel::mclapply(
    seq_along(idd_ecr_i),
    function(idx) {

      #### reading mod_pred
      idx_df <- readRDS(ecr_i_path_mod_pred_id[idx])

      #### adding original obs
      idx_df$obs <- as.numeric(raw_data$data[, idd_ecr_i[idx]])

      #### applying bias correction
      idx_df <- bias_correction_reddPrec(target_data = idx_df)

      #### getting obs + mod_pred and obs + bc_pred
      idx_df <- transform(idx_df,
                          obs_mod_pred = ifelse(is.na(obs), mod_pred, obs),
                          obs_bc_pred = ifelse(is.na(obs), bc_pred, obs))

      idx_df <- data.frame(idx_df,
                           station = idd_ecr_i[idx])

      saveRDS(
        idx_df,
        file.path(ecr_i_path_bc_pred,
                  paste(formatC(idx, width = 4, format = "d", flag = "0"),
                        "_",
                        idd_ecr_i[idx],
                        ".RDS",
                        sep = ""))
      )

    }, mc.cores = 150
  )

}
