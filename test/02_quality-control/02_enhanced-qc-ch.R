rm(list = ls())

library(xts)
source("R/utils/pipe.R")
source("R/02_quality-control/rounding-and-precision.R")
source("R/02_quality-control/weekly-cycles.R")
source("R/02_quality-control/small-gaps.R")
source("R/02_quality-control/truncation.R")


shp_esa_data <- terra::vect(
  file.path(
    dirname(dirname(getwd())), "datasets", "vector", "ch_shapefile", "ch.shp"
  )
)

raw_data <- readRDS(
  file.path(
    "data",
    "processed",
    "point",
    "datos_CH_1863-2020_v2.RDS"
  )
)

raw_data$data <- xts::xts(
  raw_data$data,
  seq(as.Date("1960-01-01"), as.Date("2015-12-31"), by = "day")
)

raw_data_sa_xyz <- raw_data$xyz
raw_data_sa_data <- raw_data$data


enhanced_qc_res <-
  raw_data$data %>%
  parallel::mclapply(function(z) {

    data.frame(truncation = get_level_trunc(xts_obj = z),
               small_gaps = get_levels_sg(xts_obj = z),
               weekly_cycles = get_levels_wc(xts_obj = z),
               precision_rounding = get_levels_rpp(xts_obj = z))

  }, mc.cores = 50) %>%
  do.call(rbind, .)

all(raw_data_sa_xyz$ID == rownames(enhanced_qc_res))
raw_data_sa_xyz <- cbind(raw_data_sa_xyz, enhanced_qc_res)


# truncation
plot(raw_data_sa_xyz[raw_data_sa_xyz$truncation == 0, ]$LON,
     raw_data_sa_xyz[raw_data_sa_xyz$truncation == 0, ]$LAT,
     col = "skyblue", cex = 1, pch = 19, xlab = "", ylab = "",
     main = "truncation",
     ylim = c(45.5, 47.75), xlim = c(5.8, 10.5))
terra::lines(shp_esa_data, col = "gray50")
points(raw_data_sa_xyz[raw_data_sa_xyz$truncation == 1, ]$LON,
       raw_data_sa_xyz[raw_data_sa_xyz$truncation == 1, ]$LAT,
       col = "lightgreen", cex = .75, pch = 19)
points(raw_data_sa_xyz[raw_data_sa_xyz$truncation == 2, ]$LON,
       raw_data_sa_xyz[raw_data_sa_xyz$truncation == 2, ]$LAT,
       col = "red", cex = .5, pch = 19)
legend(9.5, 46.1, legend=c("Level 0", "Level 1", "Level 2"),
       col = c("skyblue", "lightgreen", "red"),
       x.intersp = 1,
       y.intersp = 1,
       pch = 19,
       box.lty = 0)


# small_gaps
plot(raw_data_sa_xyz[raw_data_sa_xyz$small_gaps == 0, ]$LON,
     raw_data_sa_xyz[raw_data_sa_xyz$small_gaps == 0, ]$LAT,
     col = "skyblue", cex = 1, pch = 19, xlab = "", ylab = "",
     main = "small_gaps",
     ylim = c(45.5, 47.75), xlim = c(5.8, 10.5))
terra::lines(shp_esa_data, col = "gray50")
points(raw_data_sa_xyz[raw_data_sa_xyz$small_gaps == 1, ]$LON,
       raw_data_sa_xyz[raw_data_sa_xyz$small_gaps == 1, ]$LAT,
       col = "lightgreen", cex = .75, pch = 19)
points(raw_data_sa_xyz[raw_data_sa_xyz$small_gaps == 2, ]$LON,
       raw_data_sa_xyz[raw_data_sa_xyz$small_gaps == 2, ]$LAT,
       col = "red", cex = .5, pch = 19)
legend(9.5, 46.1, legend=c("Level 0", "Level 1", "Level 2"),
       col = c("skyblue", "lightgreen", "red"),
       x.intersp = 1,
       y.intersp = 1,
       pch = 19,
       box.lty = 0)


# weekly_cycles
plot(raw_data_sa_xyz[raw_data_sa_xyz$weekly_cycles == 0, ]$LON,
     raw_data_sa_xyz[raw_data_sa_xyz$weekly_cycles == 0, ]$LAT,
     col = "skyblue", cex = 1, pch = 19, xlab = "", ylab = "",
     main = "weekly_cycles",
     ylim = c(45.5, 47.75), xlim = c(5.8, 10.5))
terra::lines(shp_esa_data, col = "gray50")
points(raw_data_sa_xyz[raw_data_sa_xyz$weekly_cycles == 1, ]$LON,
       raw_data_sa_xyz[raw_data_sa_xyz$weekly_cycles == 1, ]$LAT,
       col = "lightgreen", cex = .75, pch = 19)
points(raw_data_sa_xyz[raw_data_sa_xyz$weekly_cycles == 2, ]$LON,
       raw_data_sa_xyz[raw_data_sa_xyz$weekly_cycles == 2, ]$LAT,
       col = "red", cex = .5, pch = 19)
legend(9.5, 46.1, legend=c("Level 0", "Level 1", "Level 2"),
       col = c("skyblue", "lightgreen", "red"),
       x.intersp = 1,
       y.intersp = 1,
       pch = 19,
       box.lty = 0)


# precision_rounding
plot(raw_data_sa_xyz[raw_data_sa_xyz$precision_rounding == 0, ]$LON,
     raw_data_sa_xyz[raw_data_sa_xyz$precision_rounding == 0, ]$LAT,
     col = "skyblue", cex = 1, pch = 19, xlab = "", ylab = "",
     main = "precision_rounding",
     ylim = c(45.5, 47.75), xlim = c(5.8, 10.5))
terra::lines(shp_esa_data, col = "gray50")
points(raw_data_sa_xyz[raw_data_sa_xyz$precision_rounding == 1, ]$LON,
       raw_data_sa_xyz[raw_data_sa_xyz$precision_rounding == 1, ]$LAT,
       col = "lightgreen", cex = .75, pch = 19)
points(raw_data_sa_xyz[raw_data_sa_xyz$precision_rounding == 2, ]$LON,
       raw_data_sa_xyz[raw_data_sa_xyz$precision_rounding == 2, ]$LAT,
       col = "red", cex = .5, pch = 19)
legend(9.5, 46.1, legend=c("Level 0", "Level 1", "Level 2"),
       col = c("skyblue", "lightgreen", "red"),
       x.intersp = 1,
       y.intersp = 1,
       pch = 19,
       box.lty = 0)


best_stations <- enhanced_qc_res %>%
  apply(1, function(x) { sum(x)})
raw_data_sa_xyz <- cbind(raw_data_sa_xyz, best_stations)

hist(raw_data_sa_xyz$best_stations,
     main = "best stations? (aggregation of all levels)",
     xlab = "all levels",
     breaks = 8, right = FALSE)

# summary plot
custom.col <- c("#C4961A", "#FFDB6D", "#F4EDCA",
                "#D16103", "#52854C", "#4E84C4", "#293352")

plot(raw_data_sa_xyz[raw_data_sa_xyz$best_stations < 2, ]$LON,
     raw_data_sa_xyz[raw_data_sa_xyz$best_stations < 2, ]$LAT,
     col = "skyblue", cex = 1, pch = 19, xlab = "", ylab = "",
     main = "best stations? (aggregation of all levels)",
     ylim = c(45.5, 47.75), xlim = c(5.8, 10.5))
terra::lines(shp_esa_data, col = "gray20",)
points(
  raw_data_sa_xyz[
    raw_data_sa_xyz$best_stations >= 2 & raw_data_sa_xyz$best_stations < 5,
  ]$LON,
  raw_data_sa_xyz[
    raw_data_sa_xyz$best_stations >= 2 & raw_data_sa_xyz$best_stations < 5,
  ]$LAT,
  col = "lightgreen", cex = .25, pch = 19
)
points(raw_data_sa_xyz[raw_data_sa_xyz$best_stations >= 5, ]$LON,
       raw_data_sa_xyz[raw_data_sa_xyz$best_stations >= 5, ]$LAT,
       col = "red", cex = .25, pch = 19)
legend(9.5, 46.1, legend = c("< 2", "2 - 5", "> 5"),
       col = c("skyblue", "lightgreen", "red"),
       x.intersp = 1,
       y.intersp = 1,
       pch = 19,
       box.lty = 0)

raw_data_sa_xyz$best_stations_cut <- cut(
  raw_data_sa_xyz$best_stations,
  breaks = c(-Inf, 2, 5, Inf), right = FALSE,
  labels = c("a", "b", "c")
)

table_summary_enhanced_qc <- aggregate(
   ID ~ best_stations_cut,
   data = raw_data_sa_xyz,
   FUN = length
)
cbind(
  table_summary_enhanced_qc,
  table_summary_enhanced_qc$ID * 100 / sum(table_summary_enhanced_qc$ID)
)
