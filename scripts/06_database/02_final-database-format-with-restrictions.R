rm(list = ls())

suppressMessages(library(xts))
source("R/utils/pipe.R")


final_database_path <- file.path("output", "06_database")

# xyz dataset
# same of the hmg or gf

load(
  file.path(dirname(dirname(getwd())),
            "datasets",
            "observed_precipitation",
            "sn_raw",
            "datos_SA_1960-2015.RData")
)

raw_data <- readRDS(
  file.path(
    "data",
    "processed",
    "point",
    "datos_SA_1960-2015_v1.2_04_obs-mod.RDS"
  )
)


raw_xyz <- sts
raw_xyz$ID <- gsub("-", "_", raw_xyz$ID)
raw_xyz$ID <- gsub("/.", "_", raw_xyz$ID)
raw_xyz$ID <- paste("id_", raw_xyz$ID, sep = "")

shared_xyz <- raw_data$xyz
shared_xyz$ALTs <- raw_xyz[match(shared_xyz$ID, raw_xyz$ID), "ALT"]
shared_xyz <- shared_xyz[, -c(9, 10)]
shared_xyz <- shared_xyz[, c("ID", "NAME", "LON", "LAT", "ALTs", "ALT", "COUNTRY", "SOURCE", "ECOREGIONS")]

# xyz_path <- file.path("output", "06_database", "xyz")
# dir.create(xyz_path, showWarnings = FALSE, recursive = TRUE)

# write.csv(
#   shared_xyz,
#   file = file.path(
#     xyz_path,
#     "xyz.csv"
#   ),
#   row.names = FALSE
# )


# data dataset files by ecoregion

raw_data <- readRDS(
  file.path(
    "data",
    "processed",
    "point",
    "datos_SA_1960-2015_v1.2_01.RDS"
  )
)

gf_results_path <- file.path(
  "output", "03_gap-filling", "bc_pred"
)

hmg_results_path_mod <- file.path(
  "output", "04_homogenization", "obs_mod"
)

hmg_results_path_bc <- file.path(
  "output", "04_homogenization", "obs_bc"
)

ecr <- c("NAS", "PAD", "CAS", "SAS", "AOL", "EHL", "GCH", "PPS", "MPN")

for (ecr_i in ecr) {
  
  ecr_i_files <- dir(
    file.path(
      hmg_results_path_bc, ecr_i, 3
    )
  )
  
  final_database_path_ecr <- file.path(
    final_database_path,
    "data_full_r",
    ecr_i
  )
  
  dir.create(final_database_path_ecr, showWarnings = FALSE, recursive = TRUE)
  
  ecr_i_files <- substr(ecr_i_files, 6, nchar(ecr_i_files))
  ecr_i_files <- gsub(".RDS", "", ecr_i_files)
  
  parallel::mclapply(
    ecr_i_files,
    function(idx) {
      
      
      hmg_bc_data <- dir(file.path(hmg_results_path_bc, ecr_i, 3),
                         recursive = TRUE,
                         full.names = TRUE,
                         pattern = idx)
      hmg_bc_data <- readRDS(hmg_bc_data)$hmg_time_serie
      colnames(hmg_bc_data) <- "hmg_obs_bc"
      
      
      hmg_mod_data <- dir(file.path(hmg_results_path_mod, ecr_i, 3),
                          recursive = TRUE,
                          full.names = TRUE,
                          pattern = idx)
      hmg_mod_data <- readRDS(hmg_mod_data)$hmg_time_serie
      colnames(hmg_mod_data) <- "hmg_obs_mod"
      
      
      gf_data <- dir(file.path(gf_results_path, ecr_i),
                     recursive = TRUE,
                     full.names = TRUE,
                     pattern = idx)
      obs_mod <- round(readRDS(gf_data)$obs_mod_pred, 1)
      mod_pred <- round(readRDS(gf_data)$mod_pred, 1)
      obs_bc <- round(readRDS(gf_data)$obs_bc_pred, 1)
      bc_pred <- round(readRDS(gf_data)$bc_pred, 1)
      err <- round(readRDS(gf_data)$err, 1)
      qc_obs <- readRDS(gf_data)$obs
      
      
      raw_data_data <- raw_data$data[, idx]
      
      final_dataframe <- data.frame(
        time_step = time(hmg_bc_data),
        raw_obs  = raw_data_data
      )
      
      final_dataframe <- cbind(final_dataframe,
                               qc_obs,
                               mod_pred,
                               bc_pred,
                               err,
                               obs_mod,
                               obs_bc,
                               hmg_mod_data,
                               hmg_bc_data)
      
      get_idx_country <- shared_xyz[match(idx, shared_xyz$ID),]$COUNTRY
      
      if(get_idx_country %in% c("BOLIVIA")) {
        
        final_dataframe$raw_obs <- NA
        final_dataframe$qc_obs <- NA
        final_dataframe$obs_mod <- NA
        final_dataframe$obs_bc <- NA
        
      }
      
      
      write.csv(
        final_dataframe,
        file = file.path(
          final_database_path_ecr,
          paste0(idx, ".csv")
        ),
        row.names = FALSE
      )
      
      rm(final_dataframe)
      gc()
      
      
    }, mc.cores = 10
  )
}


## gf metrics
# 
# gf_metric_path <- file.path(
#   "output", "03_gap-filling", "metrics"
# )
# 
# gf_metric_file <- dir(
#   gf_metric_path, full.names = TRUE, recursive = TRUE
# )
# 
# 
# gf_metric_file <- parallel::mclapply(
#   gf_metric_file,
#   function(idx) {
#     readRDS(idx)
#   }, mc.cores = 10
# ) %>%
#   do.call(rbind, .)
# 
# colnames(gf_metric_file)[14:16] <- c("ID", "MOD", "ECOREGIONS")
# gf_metric_file <- gf_metric_file[, c("ID", "ECOREGIONS", "MOD",
#                                      "n_data", "dr", "mae", "rmse", "nmae", "nrmse",
#                                      "accuracy", "precision", "recall", "f1", "bcc", "g_mean",
#                                      "wet_day")]
# 
# gf_res_path <- file.path("output", "06_database", "gf_metrics")
# dir.create(gf_res_path, showWarnings = FALSE, recursive = TRUE)
# 
# write.csv(
#   gf_metric_file,
#   file = file.path(
#     gf_res_path,
#     "gf-metrics.csv"
#   ),
#   row.names = FALSE
# )
