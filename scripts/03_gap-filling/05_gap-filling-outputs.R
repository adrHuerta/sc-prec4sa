rm(list = ls())

suppressMessages(library(xts))
source("R/utils/pipe.R")

raw_data <- readRDS(
  file.path(
    "data",
    "processed",
    "point",
    "datos_SA_1960-2015_v1.2_03.RDS"
  )
)

raw_data_xyz <- raw_data$xyz[raw_data$xyz$EQC == 1, ]
row.names(raw_data_xyz) <- NULL


# obs_mod

gf_results_path <- file.path("output", "03_gap-filling", "bc_pred")
gf_all_files <- dir(gf_results_path, recursive = TRUE, full.names = TRUE)

obs_mod <- parallel::mclapply(
  gf_all_files,
  function(idx) {

    idx <- readRDS(idx)
    idd_xts <- xts::xts(idx$obs_mod_pred, idx$time_step)
    colnames(idd_xts) <- unique(idx$station)
    idd_xts

  }, mc.cores = 100
) %>%
  do.call(cbind, .)

obs_mod <- obs_mod[, raw_data_xyz$ID]
out_obs_mod <- list(data = obs_mod,
                    xyz = raw_data_xyz)

all(colnames(out_obs_mod$data) == out_obs_mod$xyz$ID)

saveRDS(
  out_obs_mod,
  file.path(
    "data",
    "processed",
    "point",
    "datos_SA_1960-2015_v1.2_04_obs-mod.RDS"
  )
)

rm(out_obs_mod)
rm(obs_mod)

# obs_bc

gf_results_path <- file.path("output", "03_gap-filling", "bc_pred")
gf_all_files <- dir(gf_results_path, recursive = TRUE, full.names = TRUE)

obs_bc <- parallel::mclapply(
  gf_all_files,
  function(idx) {

    idx <- readRDS(idx)
    idd_xts <- xts::xts(idx$obs_bc_pred, idx$time_step)
    colnames(idd_xts) <- unique(idx$station)
    idd_xts

  }, mc.cores = 100
) %>%
  do.call(cbind, .)

obs_bc <- obs_bc[, raw_data_xyz$ID]
out_obs_bc <- list(data = obs_bc,
                   xyz = raw_data_xyz)

all(colnames(out_obs_bc$data) == out_obs_bc$xyz$ID)

saveRDS(
  out_obs_bc,
  file.path(
    "data",
    "processed",
    "point",
    "datos_SA_1960-2015_v1.2_04_obs-bc.RDS"
  )
)
