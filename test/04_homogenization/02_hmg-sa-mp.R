rm(list = ls())

library(xts)
library(ggplot2)
source("R/utils/pipe.R")
source("R/utils/get_nearby_points.R")
source("R/04_homogenization/detec-correc-helpers.R")
source("R/04_homogenization/detec-cleaning.R")
source("R/04_homogenization/detec-tests.R")
source("R/04_homogenization/correc-tests.R")
source("R/04_homogenization/detec-correc-workflow.R")


# using obs-bc
raw_data <- readRDS(
  file.path(
    "data",
    "processed",
    "point",
    "datos_SA_1960-2015_v1.2_04_obs-bc.RDS"
  )
)

## station from MPN ecoregion
idd_s <- raw_data$xyz[!is.na(raw_data$xyz$EQC) &
                        raw_data$xyz$EQC == 1 &
                        raw_data$xyz$ECOREGIONS == "MPN", ]$ID

## stages for homogenization
param_spt <- list(list(lmt_xy = 1e+6,
                       lmt_n = 8),
                  list(lmt_xy = 1e+6,
                       lmt_n = 8),
                  list(lmt_xy = 1e+6,
                       lmt_n = 8))


raw_data_data <- raw_data$data[, idd_s]
raw_data_xyz <- raw_data$xyz[match(idd_s, raw_data$xyz$ID), ]

data_tobe_homogenized <- raw_data_data
temp_stage_raw_bc <- list()

for (stage in seq_along(param_spt)) {

  print(stage)

  res_hmg_list <- parallel::mclapply(
    seq_along(idd_s),
    function(id_id) {

      # 1° nearby points
      get_nearby_points(xy_target = idd_s[id_id],
                        xy_database = raw_data_xyz,
                        lmt_xy = param_spt[[stage]]$lmt_xy,
                        lmt_n = param_spt[[stage]]$lmt_n) %>%

        # 2° build matrix
        build_matrix_hmg(stations = .,
                         xts_database = data_tobe_homogenized) %>%

        # 3° compute indices
        compute_indices(target_data = .) %>%

        # 4° apply break detection
        detection_test(target_data = .) %>%

        # 5° apply correction
        homogenization_correction(target_data = .)


    },  mc.cores = 52
  )

  # saving results of stage
  temp_stage_raw_bc[[stage]] <- res_hmg_list

  # adding new qc_obs + mod_pred to data_tobe_filled
  data_tobe_hmg_stage <-
      lapply(
        res_hmg_list,
        function(ijx) {
          ijx$hmg_time_serie
        }
      ) %>%
      do.call(cbind, .)
  colnames(data_tobe_hmg_stage) <- idd_s
  data_tobe_homogenized[, idd_s] <- data_tobe_hmg_stage

}


# using obs-mod

raw_data <- readRDS(
  file.path(
    "data",
    "processed",
    "point",
    "datos_SA_1960-2015_v1.2_04_obs-mod.RDS"
  )
)

## station from MPN ecoregion
idd_s <- raw_data$xyz[!is.na(raw_data$xyz$EQC) &
                        raw_data$xyz$EQC == 1 &
                        raw_data$xyz$ECOREGIONS == "MPN", ]$ID

## stages for homogenization
param_spt <- list(list(lmt_xy = 1e+6,
                       lmt_n = 8),
                  list(lmt_xy = 1e+6,
                       lmt_n = 8),
                  list(lmt_xy = 1e+6,
                       lmt_n = 8))


raw_data_data <- raw_data$data[, idd_s]
raw_data_xyz <- raw_data$xyz[match(idd_s, raw_data$xyz$ID), ]

data_tobe_homogenized <- raw_data_data
temp_stage_raw_mod <- list()

for (stage in seq_along(param_spt)) {

  print(stage)

  res_hmg_list <- parallel::mclapply(
    seq_along(idd_s),
    function(id_id) {

      # 1° nearby points
      get_nearby_points(xy_target = idd_s[id_id],
                        xy_database = raw_data_xyz,
                        lmt_xy = param_spt[[stage]]$lmt_xy,
                        lmt_n = param_spt[[stage]]$lmt_n) %>%

        # 2° build matrix
        build_matrix_hmg(stations = .,
                         xts_database = data_tobe_homogenized) %>%

        # 3° compute indices
        compute_indices(target_data = .) %>%

        # 4° apply break detection
        detection_test(target_data = .) %>%

        # 5° apply correction
        homogenization_correction(target_data = .)


    },  mc.cores = 52
  )

  # saving results of stage
  temp_stage_raw_mod[[stage]] <- res_hmg_list

  # adding new qc_obs + mod_pred to data_tobe_filled
  data_tobe_hmg_stage <-
      lapply(
        res_hmg_list,
        function(ijx) {
          ijx$hmg_time_serie
        }
      ) %>%
      do.call(cbind, .)
  colnames(data_tobe_hmg_stage) <- idd_s
  data_tobe_homogenized[, idd_s] <- data_tobe_hmg_stage

}



# checking some results
# prcptot and r1mm
prcptot_r1mmm_hmg <- lapply(
  c(3),
  function(stage) {

    prcptot_mod <- lapply(
      temp_stage_raw_mod[[stage]],
      function(idx) {

        xts::apply.yearly(idx$hmg_time_serie, sum)

      }
    ) %>%
      do.call(cbind, .) %>%
      apply(1, mean)

    r1mmm_mod <- lapply(
      temp_stage_raw_mod[[stage]],
      function(idx) {

        xts::apply.yearly(idx$hmg_time_serie, function(x) length(x[x >= 0.1]))

      }
    ) %>%
      do.call(cbind, .) %>%
      apply(1, mean)

    mod_df <-
    data.frame(prcptot = prcptot_mod,
               r1mm = r1mmm_mod,
               time_step = seq(1960, 2015, 1),
               type_database = "hmg-mod")

    prcptot_bc <- lapply(
      temp_stage_raw_bc[[stage]],
      function(idx) {

        xts::apply.yearly(idx$hmg_time_serie, sum)

      }
    ) %>%
      do.call(cbind, .) %>%
      apply(1, mean)

    r1mmm_bc <- lapply(
      temp_stage_raw_bc[[stage]],
      function(idx) {

        xts::apply.yearly(idx$hmg_time_serie, function(x) length(x[x >= 0.1]))

      }
    ) %>%
      do.call(cbind, .) %>%
      apply(1, mean)

    bc_df <-
    data.frame(prcptot = prcptot_bc,
               r1mm = r1mmm_bc,
               time_step = seq(1960, 2015, 1),
               type_database = "hmg-bc")

    rbind(mod_df, bc_df)
 }
)

prcptot_r1mmm_raw <- lapply(
  c(1),
  function(stage) {

    prcptot_mod <- lapply(
      temp_stage_raw_mod[[stage]],
      function(idx) {

        xts::apply.yearly(idx$raw_time_serie, sum)

      }
    ) %>%
      do.call(cbind, .) %>%
      apply(1, mean)

    r1mmm_mod <- lapply(
      temp_stage_raw_mod[[stage]],
      function(idx) {

        xts::apply.yearly(idx$raw_time_serie, function(x) length(x[x >= 0.1]))

      }
    ) %>%
      do.call(cbind, .) %>%
      apply(1, mean)

    mod_df <-
    data.frame(prcptot = prcptot_mod,
               r1mm = r1mmm_mod,
               time_step = seq(1960, 2015, 1),
               type_database = "raw-mod")

    prcptot_bc <- lapply(
      temp_stage_raw_bc[[stage]],
      function(idx) {

        xts::apply.yearly(idx$raw_time_serie, sum)

      }
    ) %>%
      do.call(cbind, .) %>%
      apply(1, mean)

    r1mmm_bc <- lapply(
      temp_stage_raw_bc[[stage]],
      function(idx) {

        xts::apply.yearly(idx$raw_time_serie, function(x) length(x[x >= 0.1]))

      }
    ) %>%
      do.call(cbind, .) %>%
      apply(1, mean)

    bc_df <-
    data.frame(prcptot = prcptot_bc,
               r1mm = r1mmm_bc,
               time_step = seq(1960, 2015, 1),
               type_database = "raw-bc")

    rbind(mod_df, bc_df)
 }
)

prcptot_r1mmm <- rbind(prcptot_r1mmm_hmg[[1]], prcptot_r1mmm_raw[[1]])
prcptot_r1mmm$type_database <- factor(
  prcptot_r1mmm$type_database,
  levels = c("raw-mod", "hmg-mod", "raw-bc", "hmg-bc")
)

prcptot_r1mmm <-
reshape2::melt(prcptot_r1mmm,
               id.vars = c("time_step", "type_database"),
               variable.name = "variable")
prcptot_r1mmm$ecoregion <- "MPN"

ggplot(prcptot_r1mmm[prcptot_r1mmm$variable == "r1mm", ],
       aes(x = time_step,
           y = value,
           colour = factor(type_database))) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = c("#4E84C4", "#293352", "#FFDB6D", "#C4961A")) +
  # scale_linetype_manual(values = c(1, 4, 1, 4)) + 
  scale_x_continuous(limits = c(1960, 2015), breaks = seq(1960, 2015, 5)) + 
  facet_wrap(~ ecoregion) +
  ylab("R1mm") + xlab("") +
  theme_bw()

ggplot(prcptot_r1mmm[prcptot_r1mmm$variable == "prcptot",],
       aes(x = time_step, 
           y = value, 
           colour = factor(type_database))) + 
  geom_line(linewidth = 1) + 
  scale_colour_manual(values = c("#4E84C4", "#293352", "#FFDB6D", "#C4961A")) +
  # scale_linetype_manual(values = c(1, 4, 1, 4)) + 
  scale_x_continuous(limits = c(1960, 2015), breaks = seq(1960, 2015, 5)) + 
  facet_wrap(~ ecoregion) + 
  ylab("PRCPTOT") + xlab("") +
  theme_bw()
