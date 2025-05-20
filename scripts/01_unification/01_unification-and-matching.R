rm(list = ls())

source("R/utils/pipe.R")
source("R/utils/get_nearby_points.R")
source("R/01_unification/unification-and-matching.R")


raw_data <- readRDS(
  file.path(
    "data",
    "processed",
    "point",
    "datos_SA_1960-2015_v1.2.RDS"
  )
)

crs_shp <- terra::vect(
  file.path(
    dirname(dirname(getwd())),
    "datasets",
    "vector",
    "sa_shapefile2",
    "SouthAmerica2.shp"
  )
) %>% terra::crs(.)

gmted2010_250m <- terra::rast(
  file.path(
    dirname(dirname(getwd())),
    "datasets",
    "grid",
    "dem",
    "GMTED2010_250m.tif"
  )
)

# check really close stations (10 meters)

filter_10m_stations <-
  parallel::mclapply(raw_data$xyz$ID %>% as.character,
                     function(x) {

                       get_nearby_points(xy_target = x,
                                         xy_database = raw_data$xyz,
                                         lmt_xy = 10,
                                         lmt_n = NA)

                     }, mc.cores = 100) %>%
  .[sapply(., length) > 1]


filter_10m_stations_res <-
  parallel::mclapply(filter_10m_stations,
                     function(x) {

                       deciding_matching_10m(list_xy = x,
                                             xy_database = raw_data$xyz,
                                             val_database = raw_data$data)

                     }, mc.cores = 100)

saveRDS(
  filter_10m_stations_res,
  file = file.path("output", "01_unification", "01-10m-close-stations.RDS")
)


filter_10m_removed_stations <-
  lapply(
         filter_10m_stations_res,
         `[[`, "removed_station") %>%
  unlist() %>%
  unique()

raw_data_data2 <- raw_data$data[, -match(filter_10m_removed_stations,
                                         colnames(raw_data$data))
]
rownames(raw_data_data2) <- NULL

raw_data_xyz2 <- raw_data$xyz[-match(filter_10m_removed_stations,
                                     raw_data$xyz$ID),
]
rownames(raw_data_xyz2) <- NULL



# check close stations (up to 25 kilometers)

filter_25km_stations <-
  parallel::mclapply(raw_data_xyz2$ID %>% as.character,
                     function(x) {

                       get_nearby_points(xy_target = x,
                                         xy_database = raw_data_xyz2,
                                         lmt_xy = 25 * 1000,
                                         lmt_n = NA)

                     }, mc.cores = 100) %>%
  .[sapply(., length) > 1]

filter_25km_stations_res <-
  parallel::mclapply(filter_25km_stations,
                     function(x) {

                       deciding_matching_25km(list_xy = x,
                                              val_database = raw_data_data2)

                     }, mc.cores = 100)

saveRDS(
  filter_25km_stations_res,
  file = file.path("output", "01_unification", "02-25km-close-stations.RDS")
)

filter_25km_removed_stations <-
  lapply(
         filter_25km_stations_res,
         `[[`, "removed_station") %>%
  unlist() %>%
  unique()

raw_data_data3 <- raw_data_data2[, -match(filter_25km_removed_stations,
                                          colnames(raw_data_data2))
]
rownames(raw_data_data3) <- NULL

raw_data_xyz3 <- raw_data_xyz2[-match(filter_25km_removed_stations,
                                      raw_data_xyz2$ID),
]
rownames(raw_data_xyz3) <- NULL



# check of station elevation versus dem

xyz_vect <- terra::vect(
  raw_data_xyz3, geom = c("LON", "LAT"), crs = crs_shp, keepgeom = TRUE
)

check_elev <-
  parallel::mclapply(raw_data_xyz3$ID %>% as.character,
                     function(x) {

                       deciding_elevation(xy_target = x,
                                          xy_database = xyz_vect,
                                          dem_data = gmted2010_250m,
                                          params = list(buffer_ = 500,
                                                        percent_ = 0.5))


                     }, mc.cores = 100)

saveRDS(
  check_elev,
  file = file.path("output",
                   "01_unification",
                   "03-elevation-vs-dem-elevation.RDS")
)

check_elev_removed_stations <-
  lapply(
         check_elev,
         `[[`, "removed_station") %>%
  unlist() %>%
  unique() %>%
  .[complete.cases(.)]

length(check_elev_removed_stations)

# raw_data_data4 <- raw_data_data3[, -match(check_elev_removed_stations,
#                                           colnames(raw_data_data3))
# ]
# rownames(raw_data_data4) <- NULL

# raw_data_xyz4 <- raw_data_xyz3[-match(check_elev_removed_stations,
#                                       raw_data_xyz3$ID),
# ]
# rownames(raw_data_xyz4) <- NULL 

all(colnames(raw_data_data3) == raw_data_xyz3$ID)
raw_data <- list(data = raw_data_data3,
                 xyz = raw_data_xyz3)

saveRDS(
  raw_data,
  file.path(
    "data",
    "processed",
    "point",
    "datos_SA_1960-2015_v1.2_01.RDS"
  )
)