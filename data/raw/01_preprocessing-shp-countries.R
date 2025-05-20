rm(list = ls())

source("R/utils/pipe.R")
library(terra)

sa_file <- file.path(
  dirname(dirname(getwd())),
  "datasets",
  "vector",
  "sa_shapefile",
  "SouthAmerica.shp"
)

sa_shp <- terra::vect(sa_file)
sa_shp$Areakm <- terra::expanse(sa_shp, unit = "m") / 1000
terra::plot(sa_shp)

# removing islands that really far from the continent
sa_shp2 <- sa_shp[sa_shp$Name != "CHILE" &
                    sa_shp$Name != "BRAZIL" &
                    sa_shp$Name != "VENEZUELA" &
                    sa_shp$Name != "ECUADOR" &
                    sa_shp$Name != "ARGENTINA", ]
sa_shp2 <- terra::crop(sa_shp2, terra::ext(-81.5, -50, -40, 20))
sa_shp2 <-
  rbind(sa_shp2,
    terra::crop(sa_shp[sa_shp$Name == "CHILE", ],
                terra::ext(-78, -65, -67, -15)),
    terra::crop(sa_shp[sa_shp$Name == "BRAZIL", ],
                terra::ext(-75, -35, -40, 10)),
    terra::crop(sa_shp[sa_shp$Name == "ECUADOR", ],
                terra::ext(-85, -70, -10, 5)),
    sa_shp[sa_shp$Name == "ARGENTINA", ],
    sa_shp[sa_shp$Name == "VENEZUELA", ]
  )
sa_shp2 <- sa_shp2[, c("Name", "Areakm")]
terra::plot(sa_shp2)

terra::writeVector(
  sa_shp2,
  file.path("data",
            "processed",
            "vector",
            "SouthAmerica2.shp"),
  overwrite = TRUE
)
