rm(list = ls())

library(xts)
source("R/utils/pipe.R")
source("R/02_quality-control/rounding-and-precision.R")
source("R/02_quality-control/weekly-cycles.R")
source("R/02_quality-control/small-gaps.R")
source("R/02_quality-control/truncation.R")
source(
  file.path(
    "https:/", "raw.githubusercontent.com",
    "adrHuerta", "enhanced_qc",
    "refs", "heads", "main", "src",
    "enhanced_qc_fun.R"
  )
)

raw_data <- readRDS(
  file.path(
    "data",
    "processed",
    "point",
    "datos_SA_1960-2015_v1.2_02.RDS"
  )
)

raw_data_xyz_01 <- raw_data$xyz[raw_data$xyz$SIZE == 1, ]
raw_data_data_01 <- raw_data$data[, raw_data$xyz[raw_data$xyz$SIZE == 1, ]$ID]


# rounding-and-precision-patterns
exp_ts <- raw_data$data[, raw_data_xyz_01$ID[100]]
lattice::xyplot(exp_ts, type = "p", cex = .1)
## visual inspection
decimal_plots_eqc(xts_ts = exp_ts)
## automated inspection
get_ndec(xts_obj = exp_ts)
get_dec_patterns(xts_obj = exp_ts)
get_levels_rpp(xts_obj = exp_ts)


# weekly-cycles
exp_ts <- raw_data$data[, "id_24010020"]
lattice::xyplot(exp_ts, type = "p", cex = .1)
## visual inspection
wd_plot_eqc(xts_ts = exp_ts)
## automated inspection
get_wc(xts_obj = exp_ts)
wd_value(xts_obj = exp_ts)
get_wd_fraction(xts_obj = exp_ts)
get_levels_wc(xts_obj = exp_ts)


# small-gaps
exp_ts <- raw_data$data[, "id_BO2057"]
lattice::xyplot(exp_ts, type = "p", cex = .1)
## visual inspection
point_plots_eqc_thld(xts_ts = exp_ts)
## automated inspection
get_ngaps(xts_obj = exp_ts, make_plot = TRUE)
get_levels_sg(xts_obj = exp_ts)


# truncation
## this algorithm was more complicated to implement
## than the others, so it is not so efficient
## some false negative and false positive can be expected
## but can detect the best time series (check with ch data)
exp_ts <- raw_data$data[, "id_BO2057"]
lattice::xyplot(exp_ts, type = "p", cex = .1)
## visual inspection
point_plots_eqc(xts_ts = exp_ts)
## automated inspection
get_lenght_borderline(xts_obj = exp_ts, make_plot = TRUE)
get_level_trunc(xts_obj = exp_ts)
