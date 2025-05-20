rm(list = ls())

source("R/utils/pipe.R")
library(terra)

sa_shp <- terra::vect(
  file.path("data",
            "processed",
            "vector",
            "SouthAmerica2.shp")
)
ecoregions_sa <- terra::vect(
  file.path(dirname(dirname(getwd())),
            "datasets",
            "vector",
            "sa_eco",
            "sa_eco_l3.shp")
)
ecoregions_sa <- terra::project(ecoregions_sa, sa_shp)
terra::plot(ecoregions_sa, y = "LEVEL1", type = "classes")

# making the area from EHL that is inside AOL to AOL
ecoregions_sa[ecoregions_sa$LEVEL2 == 21.1, ]$LEVEL1 <- 20
terra::plot(ecoregions_sa, y = "LEVEL1", type = "classes")

# making the NAS
ecoregions_sa_no.17.1.7 <- ecoregions_sa[ecoregions_sa$LEVEL1 == 17, ]
ecoregions_sa_no.17.1.7 <- ecoregions_sa_no.17.1.7[
  ecoregions_sa_no.17.1.7$LEVEL3 != "17.1.7",
]
ecoregions_sa_17.1.7 <- ecoregions_sa[
  ecoregions_sa$LEVEL3 == "17.1.7" & ecoregions_sa$LEVEL1 == 17,
]
ecoregions_sa_17.1.7 <- ecoregions_sa[ecoregions_sa$LEVEL3 == "17.1.7", ]
terra::plot(ecoregions_sa_17.1.7)

sa_shp_colecu <- sa_shp[sa_shp$Name == "COLOMBIA" | sa_shp$Name == "ECUADOR", ]
sa_shp_colecu <- terra::aggregate(sa_shp_colecu)
ecoregions_sa_17.1.7 <- terra::intersect(ecoregions_sa_17.1.7, sa_shp_colecu)
terra::plot(ecoregions_sa_17.1.7)

ecoregions_sa_na <- terra::union(ecoregions_sa_no.17.1.7, ecoregions_sa_17.1.7)
ecoregions_sa_na <- terra::aggregate(ecoregions_sa_na, "LEVEL1")
ecoregions_sa_na$nr <- "Northern Andes"
terra::plot(ecoregions_sa_na, y = "nr", type = "classes")

# making the CAS and PAD
ecoregions_sa_ca <- ecoregions_sa[ecoregions_sa$LEVEL1 == 18, ]
terra::plot(ecoregions_sa_ca)
ecoregions_sa_ca <- terra::aggregate(ecoregions_sa_ca, "LEVEL2")
ecoregions_sa_ca$nr <- transform(ecoregions_sa_ca,
                                 nr = ifelse(
                                             LEVEL2 != 18.4,
                                             "Central Andes",
                                             "Peruvian-Atacama Desert"))$nr
ecoregions_sa_ca <- terra::aggregate(ecoregions_sa_ca, "nr")
terra::plot(ecoregions_sa_ca, y = "nr", type = "classes")

# making the SAS
ecoregions_sa_sa <- ecoregions_sa[ecoregions_sa$LEVEL1 == 19, ]
ecoregions_sa_sa <- terra::crop(ecoregions_sa_sa,
                                terra::ext(-180, -63, -60, 10))
ecoregions_sa_sa <- terra::aggregate(ecoregions_sa_sa, "LEVEL1")
ecoregions_sa_sa$nr <- "Southern Andes"
terra::plot(ecoregions_sa_sa, y = "nr", type = "classes")

# making the rest of ecoregions
ecoregions_sa_noca <- ecoregions_sa[ecoregions_sa$LEVEL1 > 19, ]
ecoregions_sa_noca <- terra::aggregate(ecoregions_sa_noca, "LEVEL1")
ecoregions_sa_noca$nr <- c("Amazonian-Orinocan Lowland",
                           "Eastern Highlands",
                           "Gran Chaco",
                           "Pampas",
                           "Monte-Patagonian")
terra::plot(ecoregions_sa_noca, y = "nr", type = "classes")

ecoregions_sa_f <- terra::union(ecoregions_sa_noca[, "nr"],
                                ecoregions_sa_na[, "nr"]) %>%
  terra::union(., ecoregions_sa_ca[, "nr"]) %>%
  terra::union(., ecoregions_sa_sa[, "nr"])

ecoregions_sa_f$nr_id <- c("AOL", "EHL", "GCH",
                           "PPS", "MPN", "NAS",
                           "CAS", "PAD", "SAS")
ecoregions_sa_f$nr_id <- factor(ecoregions_sa_f$nr_id,
                                levels = c("NAS", "PAD", "CAS",
                                           "SAS", "AOL", "EHL",
                                           "GCH", "PPS", "MPN"))
# ecoregions_sa_f$nr <- factor(ecoregions_sa_f$nr, 
#                              levels = c("Northern Andes", "Peruvian/Atacaman Deserts", "Central Andes",
#                                         "Southern Andes", "Amazonian-Orinocan Lowland", "Gran Chaco",
#                                         "Monte-Patagonian", "Eastern Hihglands", "Pampas"))

terra::plot(x = ecoregions_sa_f,
            y = "nr",
            type = "classes")

terra::plot(x = ecoregions_sa_f,
            y = "nr_id",
            type = "classes",
            sort = levels(ecoregions_sa_f$nr_id))

terra::writeVector(
  ecoregions_sa_f,
  file.path("data",
            "processed",
            "vector",
            "sa_eco_l3_2_paper.shp"),
  overwrite = TRUE
)
