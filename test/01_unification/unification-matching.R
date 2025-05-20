rm(list = ls())

source("R/utils/pipe.R")
source("R/utils/get_nearby_points.R")
source("R/01_unification/unification-and-matching.R")


raw_data <- readRDS(
  file.path(
    dirname(dirname(getwd())),
    "datasets",
    "observed_precipitation",
    "raw",
    "datos_SA_1960-2015_v1.RDS"
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

#
get_nearby_points(xy_target = "id_AR_122",
                  xy_database = raw_data$xyz,
                  lmt_xy = 10,
                  lmt_n = NA) %>%
  deciding_matching_10m(list_xy = .,
                        xy_database = raw_data$xyz,
                        val_database = raw_data$data)

plot(raw_data$data[, c("id_AR_122")],
     raw_data$data[, c("id_AR_040")])

# special case when not all the data is the same (r > 0.999 & mae < 0.1)
# the original algorithm could not find the match
# but the new algorithm can
get_nearby_points(xy_target = "id_SU_300",
                  xy_database = raw_data$xyz,
                  lmt_xy = 25 * 1000,
                  lmt_n = NA) %>%
  deciding_matching_25km(list_xy = .,
                         val_database = raw_data$data)

plot(raw_data$data[, c("id_SU_300")],
     raw_data$data[, c("id_SU_317")])

#
xyz_vect <- terra::vect(
  raw_data$xyz, geom = c("LON", "LAT"), crs = crs_shp, keepgeom = TRUE
)

deciding_elevation(
  xy_target = "id_BO3332",
  xy_database = xyz_vect,
  dem_data = gmted2010_250m,
  params = list(buffer_ = 500,
                percent_ = 0.5),
  make_plot = TRUE
)
