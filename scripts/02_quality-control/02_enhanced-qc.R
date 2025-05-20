rm(list = ls())

suppressMessages(library(xts))
source("R/utils/pipe.R")
source("R/02_quality-control/rounding-and-precision.R")
source("R/02_quality-control/weekly-cycles.R")
source("R/02_quality-control/small-gaps.R")
source("R/02_quality-control/truncation.R")

raw_data <- readRDS(
  file.path(
    "data",
    "processed",
    "point",
    "datos_SA_1960-2015_v1.2_02.RDS"
  )
)

raw_data_xyz <- raw_data$xyz[raw_data$xyz$SIZE == 1, ]
raw_data_data <- raw_data$data[,
  raw_data$xyz[raw_data$xyz$SIZE == 1, ]$ID
]

enhanced_qc_res <-
  raw_data_data %>%
  parallel::mclapply(function(z) {

    data.frame(truncation = get_level_trunc(xts_obj = z),
               small_gaps = get_levels_sg(xts_obj = z),
               weekly_cycles = get_levels_wc(xts_obj = z),
               precision_rounding = get_levels_rpp(xts_obj = z),
               lenght_decimals_patterns = get_dec_patterns_mode(xts_obj = z),
               wet_day = wd_value(xts_obj = z))

  }, mc.cores = 50) %>%
  do.call(rbind, .)

# adding more parameters due to that SA
# has a diverse range of wet days and 
# problems with the precision 

enhanced_qc_res <- transform(
  enhanced_qc_res,
  truncation2 = ifelse(wet_day < 15, 0, truncation),
  small_gaps2 = ifelse(wet_day < 15, 0, small_gaps),
  weekly_cycles2 = ifelse(wet_day < 5, 0, weekly_cycles),
  precision_rounding2 = ifelse(wet_day < 15, 0, precision_rounding)
)

enhanced_qc_res <- transform(
  enhanced_qc_res,
  small_gaps3 = ifelse(lenght_decimals_patterns > 25, 0, small_gaps2)
)

enhanced_qc_res <- enhanced_qc_res[, c("truncation2", "small_gaps3", "weekly_cycles2", "precision_rounding2")]
colnames(enhanced_qc_res) <- c("truncation", "small_gaps", "weekly_cycles", "precision_rounding")

all(raw_data_xyz$ID == rownames(enhanced_qc_res))
raw_data_xyz <- cbind(raw_data_xyz, enhanced_qc_res)

# best stations
## truncation == 2 | small_gaps == 2 | weekly_cycles == 2 are flagged for the next steps
raw_data_xyz <- transform(
  raw_data_xyz,
  EQC = ifelse(truncation == 2 | small_gaps == 2 | weekly_cycles == 2, 0, 1)
)

saveRDS(
  raw_data_xyz,
  file.path(
    "output",
    "02_quality-control",
    "eqc-01-summary.RDS"
  )
)

all(colnames(raw_data_data) == raw_data_xyz$ID)
raw_data <- list(
  data = raw_data_data,
  xyz = raw_data_xyz[,
   c("ID", "NAME", "LON", "LAT", "ALT", "COUNTRY", "SOURCE", "ECOREGIONS", "SIZE", "EQC")
  ]
)                  

saveRDS(
  raw_data,
  file.path(
    "data",
    "processed",
    "point",
    "datos_SA_1960-2015_v1.2_03.RDS"
  )
)