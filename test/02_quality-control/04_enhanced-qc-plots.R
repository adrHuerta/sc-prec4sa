rm(list = ls())

library(lattice)
library(xts)
source("R/utils/pipe.R")
source("R/02_quality-control/rounding-and-precision.R")
source("R/02_quality-control/weekly-cycles.R")
source("R/02_quality-control/small-gaps.R")
source("R/02_quality-control/truncation.R")
source("R/02_quality-control/enhanced-qc-plots.R")


# raw_data <- readRDS(
#   file.path(
#     "data",
#     "processed",
#     "point",
#     "datos_CH_1863-2020_v2.RDS"
#   )
# )

# raw_data_00 <- xts::xts(
#   raw_data$data[, "AIE"],
#   seq(as.Date("1960-01-01"), as.Date("2015-12-31"), by = "day")
# )

# raw_data <- readRDS(
#   file.path(
#     "data",
#     "processed",
#     "point",
#     "datos_ESParagon_1950_2020_v3.RDS"
#   )
# )

# raw_data_01 <- xts::xts(
#   raw_data$data[, "X2026"],
#   seq(as.Date("1960-01-01"), as.Date("2015-12-31"), by = "day")
# )

# saveRDS(
#   cbind(
#     raw_data_00,
#     raw_data_01
#   ),
#   file.path(
#     "data",
#     "processed",
#     "point",
#     "enhanced-qc-plots.RDS"
#   )
# )

raw_data <- readRDS(
  file.path(
    "data",
    "processed",
    "point",
    "enhanced-qc-plots.RDS"
  )
)

# lattice plots because are faster than ggplot2
# you can also use the other functions with this data
enhanced_qc_plus_plot(xts_obj = raw_data[, "raw_data_00"])
enhanced_qc_plus_plot(xts_obj = raw_data[, "raw_data_01"])

# pdf(file.path("output", "05_visualization", "fig_nn-eqc-example-00.pdf"))
# enhanced_qc_plus_plot(xts_obj = raw_data[, "raw_data_00"])
# dev.off()
# pdf(file.path("output", "05_visualization", "fig_nn-eqc-example-01.pdf"))
# enhanced_qc_plus_plot(xts_obj = raw_data[, "raw_data_01"])
# dev.off()