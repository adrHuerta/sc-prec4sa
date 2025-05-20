rm(list = ls())

source("R/utils/pipe.R")
library(terra)
library(data.table)

load(
  file.path(dirname(dirname(getwd())),
            "datasets",
            "observed_precipitation",
            "sn_raw",
            "datos_SA_1960-2015.RData")
)
data_pp <- data.table::setDT(dat)
data_xyz <- data.table::setDT(sts)


# removing station that have bad metada based on visual inspection

data_pp[, c("X000346", "X000400", "X000536", "26255020") := NA]

# remove stations with full NA
NNNas <- data_pp[, lapply(.SD, function(x) sum(!is.na(x)))]
names_NNNas <- names(unlist(NNNas)[unlist(NNNas) > 0])
data_pp_2 <- data_pp[, ..names_NNNas]
data_xyz_2 <- data_xyz[data_xyz$ID %in% names_NNNas, ]
rownames(data_pp_2) <- NULL
rownames(data_xyz_2) <- NULL


# remove station outside '(buffer distance)' continental SA
sa_shp <- terra::vect(
  file.path("data",
            "processed",
            "vector",
            "SouthAmerica2.shp")
)
sa_shp$Areakm <- terra::expanse(sa_shp, unit = "m") / 1000
sa_shp2 <- sa_shp[sa_shp$Name != "CHILE" &
                  sa_shp$Name != "BRAZIL" &
                  sa_shp$Name != "VENEZUELA" &
                  sa_shp$Name != "ECUADOR" &
                  sa_shp$Name != "ARGENTINA", ]
sa_shp2 <- terra::crop(sa_shp2, terra::ext(-81.5, -50, -40, 20))
sa_shp2 <- rbind(sa_shp2,
                 terra::crop(sa_shp[sa_shp$Name == "CHILE", ],
                             terra::ext(-78, -65, -67, -15)),
                 terra::crop(sa_shp[sa_shp$Name == "BRAZIL",],
                             terra::ext(-75, -35, -40, 10)),
                 terra::crop(sa_shp[sa_shp$Name == "ECUADOR",],
                             terra::ext(-85, -70, -10, 5)),
                 sa_shp[sa_shp$Name == "ARGENTINA",],
                 sa_shp[sa_shp$Name == "VENEZUELA",])
sa_shp3 <- terra::aggregate(sa_shp2)
sa_shp3 <- terra::buffer(sa_shp3, 150000)

pts_raw <- terra::vect(data_xyz_2[, c("ID", "LON", "LAT")],
                       geom = c("LON", "LAT"),
                       crs = terra::crs(sa_shp3))
pts_raw2 <- terra::intersect(pts_raw, sa_shp3)$ID %>% as.character()
# dim(data_xyz_2)[1] - length(pts_raw2) # 49 outside SA
data_pp_3 <- data_pp_2[, ..pts_raw2]
data_xyz_3 <- data_xyz_2[ID %in% pts_raw2, ]
rownames(data_pp_3) <- NULL
rownames(data_xyz_3) <- NULL

# adding ECOREGIONS
pts_raw3 <- terra::vect(
  data_xyz_3[, c("ID", "COUNTRY", "LON", "LAT")],
  geom = c("LON", "LAT"),
  crs = terra::crs(sa_shp3),
  keepgeom = TRUE
)
ec_shp <- terra::vect(
  file.path(
            dirname(dirname(getwd())),
            "datasets",
            "vector",
            "sa_eco2",
            "sa_eco_l3_2_paper.shp")
)
stations_ec <- terra::intersect(ec_shp, pts_raw3)
ec_shp2 <- terra::buffer(ec_shp, 3000)
stations_ec_remain <- pts_raw3[-match(stations_ec$ID, pts_raw3$ID)]
stations_ec_2 <- terra::intersect(ec_shp2, stations_ec_remain)

stations_ec_remain2 <- stations_ec_remain[
  -match(stations_ec_2$ID, stations_ec_remain$ID),
]
plot(ec_shp)
points(stations_ec_remain2, col = "red", cex = 2)
# data.frame(stations_ec_remain2)
stations_ec_remain2$nr <- c("Eastern Hihglands",
                            "Northern Andes",
                            "Eastern Hihglands",
                            "Eastern Hihglands",
                            "Northern Andes",
                            "Northern Andes")
stations_ec_remain2$nr_id <- c("EHL",
                               "NAS",
                               "EHL",
                               "EHL",
                               "NAS",
                               "NAS")
stations_ec_remain2 <- stations_ec_remain2[, c("nr",
                                               "nr_id",
                                               "ID",
                                               "COUNTRY",
                                               "LON",
                                               "LAT")]
stations_ec_all <- rbind(stations_ec,
                         stations_ec_2,
                         stations_ec_remain2)
stations_ec_all <- merge(pts_raw3,
                         stations_ec_all,
                         by = "ID")

dim(stations_ec_all)[1] == dim(data_xyz_3)[1]
all(stations_ec_all$ID == data_xyz_3$ID)
#data_xyz_3$ECOREGIONS <- stations_ec_all$nr
data_xyz_3$ECOREGIONS <- stations_ec_all$nr_id

all(data_xyz_3$ID == colnames(data_pp_3))
data_xyz_3$ID <- gsub("-", "_", data_xyz_3$ID)
data_xyz_3$ID <- gsub("/.", "_", data_xyz_3$ID)
data_xyz_3$ID <- paste("id_", data_xyz_3$ID, sep = "")
colnames(data_pp_3) <- data_xyz_3$ID


# replacing original elevation by DEM
gmted2010_250m <- terra::rast(
  file.path(
    dirname(dirname(getwd())),
    "datasets",
    "grid",
    "dem",
    "GMTED2010_250m.tif"
  )
)

latlonelv_xyz3 <- data.frame(data_xyz_3[, c("LON", "LAT", "ALT")])
elv_dem <- terra::extract(
  gmted2010_250m,
  latlonelv_xyz3[, c("LON", "LAT")]
)
elv_dem <- elv_dem[, 2]
data_xyz_3$ALT <- elv_dem

# final dataset
raw_data <- list(
  data = as.data.frame(data_pp_3),
  xyz = as.data.frame(data_xyz_3)
)

# making everything one single decimal 
raw_data$data <- round(raw_data$data, 1)

all(colnames(raw_data$data) == raw_data$xyz$ID)

vect(raw_data$xyz, "points", geom = c("LON", "LAT")) %>%
  terra::plot(y = "COUNTRY", type = "classes")

vect(raw_data$xyz, "points", geom = c("LON", "LAT")) %>%
  terra::plot(y = "ECOREGIONS", type = "classes")

dim(raw_data$xyz)


# removing some bad data based on users's inspection

raw_data$data[, "id_CI000085406"] <- round(raw_data$data[, "id_CI000085406"] / 100, 1)
raw_data$data[, "id_CIM00085418"] <- round(raw_data$data[, "id_CIM00085418"] / 100, 1)
raw_data$data[, "id_CI000085442"] <- round(raw_data$data[, "id_CI000085442"] / 100, 1)
raw_data$data[, "id_PE000084721"] <- round(raw_data$data[, "id_PE000084721"] / 100, 1)
raw_data$data[, "id_PEM00084628"] <- round(raw_data$data[, "id_PEM00084628"] / 100, 1)
raw_data$data[, "id_PEM00084691"] <- round(raw_data$data[, "id_PEM00084691"] / 100, 1)
raw_data$data[, "id_PEM00084390"][raw_data$data[, "id_PEM00084390"] > 200] <- NA
raw_data$data[, "id_PEM00084452"][raw_data$data[, "id_PEM00084452"] > 110] <- NA
raw_data$data[, "id_X150700"][raw_data$data[, "id_X150700"] > 90] <- NA
raw_data$data[, "id_X156205"][1:6000] <- NA
raw_data$data[, "id_X152403"][10000:length(raw_data$data[, "id_X152403"])] <- NA
raw_data$data[, "id_X000390"][9000:length(raw_data$data[, "id_X000390"])] <- NA
raw_data$data[, "id_BR002750007"][raw_data$data[, "id_BR002750007"] > 119] <- NA
raw_data$data[, "id_M0758"][raw_data$data[, "id_M0758"] > 150] <- NA
raw_data$data[, "id_M0735"][raw_data$data[, "id_M0735"] > 100] <- NA
raw_data$data[, "id_M0389"][raw_data$data[, "id_M0389"] > 100] <- NA
raw_data$data[, "id_23050170"][6500:8500] <- NA
raw_data$data[, "id_83038"][1:7500] <- NA

saveRDS(raw_data,
  file = file.path("data",
                   "processed",
                   "point",
                   "datos_SA_1960-2015_v1.2.RDS")
)
