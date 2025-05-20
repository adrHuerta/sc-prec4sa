rm(list = ls())

source("R/utils/pipe.R")
suppressMessages(library(ggplot2))
suppressMessages(library(tidyterra))
suppressMessages(library(terra))
suppressMessages(library(data.table))

raw_data <- readRDS(
  file.path("data",
            "processed",
            "point",
            "datos_SA_1960-2015_v1.2.RDS")
)

tper_ecr <- aggregate(ID ~ ECOREGIONS + SOURCE, raw_data$xyz, FUN = length)
tper_ecr$ECOREGIONS <- factor(tper_ecr$ECOREGIONS,
  levels = c("NAS", "PAD",
             "CAS", "SAS",
             "AOL", "EHL",
             "GCH", "PPS", "MPN")
)
tper_ecr <- with(tper_ecr, tper_ecr[order(ECOREGIONS, SOURCE, ID), ]) %>%
  setNames(c("Ecoregion", "Source", "Number of stations"))

tper_cou <- aggregate(ID ~ COUNTRY + SOURCE, raw_data$xyz, FUN = length)
tper_cou <- with(tper_cou, tper_cou[order(COUNTRY, SOURCE, ID), ])  %>%
  setNames(c("Country", "Source", "Number of stations"))

write.csv(
  tper_ecr,
  file = file.path("output",
                   "05_visualization",
                   "tab_01-summary-nstations-ecor.csv"),
  row.names = FALSE
)

write.csv(
  tper_cou,
  file = file.path("output",
                   "05_visualization",
                   "tab_02-summary-nstations-countries.csv"),
  row.names = FALSE
)
