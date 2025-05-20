rm(list = ls())

suppressMessages(library(xts))
source("R/utils/pipe.R")

final_database_path <- file.path("output", "06_database")
dir.create(file.path(final_database_path, "rds"), showWarnings = FALSE, recursive = TRUE)

xyz <- read.csv(file.path(final_database_path, "xyz", "xyz.csv"))
data_pr <- data.frame(
  long_path = dir(file.path(final_database_path, "data_full"), recursive = TRUE)
)
data_pr <- transform(data_pr,
                     ID_files = gsub("[.csv]", "", substr(long_path, 5, nchar(long_path))))

data_pr <- data_pr[match(xyz$ID, data_pr$ID_files), ]

column_df <- c("raw_obs", "qc_obs", "mod_pred", "bc_pred", "err", "obs_mod", "obs_bc", "hmg_obs_mod", "hmg_obs_bc")
for(i in  column_df){
  
  print(i)
  
  time_series_i <- parallel::mclapply(
    data_pr$long_path,
    function(j) {
      
      read.csv(file.path(final_database_path, "data_full", j))[, i]
      
    }, mc.cores = 20
  )
  
  time_series_i <- do.call(cbind, time_series_i)
  time_series_i <- xts::xts(time_series_i,
                            seq(as.Date("1960-01-01"), as.Date("2015-12-31"), by = "day"))
  names(time_series_i) <- data_pr$ID_files
  
  saveRDS(
    list(xyz = xyz,
         data = time_series_i),
    file = file.path(
      final_database_path,
      "rds",
      paste("SC-PREC4SA_", i, ".RDS", sep = "")
      
    )
  )
  
}

