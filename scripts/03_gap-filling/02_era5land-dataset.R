rm(list = ls())

suppressMessages(library(xts))
source("R/utils/pipe.R")
source("R/03_gap-filling/era5land-grid.R")

raw_data <- readRDS(
  file.path(
    "data",
    "processed",
    "point",
    "datos_SA_1960-2015_v1.2_03.RDS"
  )
)

# era5land

path_era5land <- "/home/ahuerta/repos/era5land_gd/era5land_tp_sa_gf"
grid_era5land <- terra::rast(
  dir(file.path(path_era5land, "1961"), full.names = TRUE)[1]
)

coords_xy <- raw_data$xyz[
  raw_data$xyz$EQC == 1,
  c("ID", "LON", "LAT")
]

xyz_era5land <- get_modobs_xyz(
  df_coord_obs = coords_xy,
  grid_mod = grid_era5land
)

era5_ts <- parallel::mclapply(1960:2015, function(yid) {

  grids_yid <- dir(
    file.path(path_era5land, as.character(yid)),
    full.names = TRUE
  )

  ts_pergrid <- lapply(seq_along(grids_yid), function(jid) {

    grd_jid <- terra::rast(grids_yid[jid])
    grd_ts <- grd_jid[xyz_era5land$cell]
    grd_ts

  })

  ts_pergrid <- do.call(cbind, ts_pergrid)
  ts_pergrid <- t(ts_pergrid)
  ts_pergrid

}, mc.cores = 50)

era5_ts <- do.call(rbind, era5_ts)
era5_ts <- xts::xts(era5_ts, time(raw_data$data))
colnames(era5_ts) <- xyz_era5land$ID

era5_ts_bc <- parallel::mclapply(
  xyz_era5land$ID_obs,
  function(yid) {

    obs_obs <- xts::xts(raw_data$data[, yid], time(era5_ts))
    sim_sim <- xts::xts(era5_ts[, paste(yid, "mod", sep = "_")], time(era5_ts))
    sim_sim[sim_sim < 0] <- 0

    sim_bc <- qmap_era5land(ts_obs = round(obs_obs, 1),
                            ts_model = round(sim_sim, 1),
                            window_c = 15)

    as.numeric(sim_bc)

  }, mc.cores = 100
)

era5_ts_bc <- do.call(cbind, era5_ts_bc)
era5_ts_bc <- xts::xts(era5_ts_bc, time(era5_ts))
colnames(era5_ts_bc) <- xyz_era5land$ID
era5_ts_bc <- round(era5_ts_bc, 1)

# topographic covariables for era5land

covs_path <- file.path(
  file.path(
    dirname(dirname(getwd())),
    "datasets",
    "grid",
    "topographic_variables",
    "topo_vars_sa_gf.nc"
  )
)

covs <- terra::rast(covs_path)
names(covs) <- c(
  "aspectcosine", "aspectsine", "dist2coast", "dx", "dxx",
  "dy", "dyy", "eastness", "elevation", "latitude", "longitude",
  "northness", "pcurv", "roughness", "slope", "tcurv", "tpi", "tri", "vrm"
)

xy_cells <- terra::extract(
  covs[[1]],
  xyz_era5land[, c("LON", "LAT")],
  cells = TRUE
)

xy_cells <- xy_cells$cell
xy_topo_cov <- covs[xy_cells]

xzy_era5_f <- data.frame(
  ID = xyz_era5land$ID,
  NAME = xyz_era5land$ID,
  LON = xy_topo_cov$longitude,
  LAT = xy_topo_cov$latitude,
  ALT = xy_topo_cov$elevation,
  COUNTRY = NA,
  SOURCE = NA,
  ECOREGIONS = NA,
  SIZE = NA,
  EQC = NA
)

xzy_era5_f <- cbind(xzy_era5_f, xy_topo_cov)

raw_data <- list(data = era5_ts_bc,
                 xyz = xzy_era5_f)

all(colnames(raw_data$data) == raw_data$xyz$ID)

saveRDS(
  raw_data,
  file.path(
    "data",
    "processed",
    "point",
    "datos_era5land_ecoregion_sa_qmap.RDS"
  )
)
