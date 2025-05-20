rm(list = ls())

library(xts)
library(ggplot2)
source("R/utils/pipe.R")
source("R/utils/get_nearby_points.R")
source("R/03_gap-filling/gf-machinery.R")
source("R/03_gap-filling/gf-models.R")
source("R/03_gap-filling/gf-helpers.R")
source("R/03_gap-filling/gf-metrics.R")


raw_data <- readRDS(
  file.path(
    "data",
    "processed",
    "point",
    "datos_SA_1960-2015_v1.2_03_tc.RDS"
  )
)

era5land_data <- readRDS(
  file.path(
    "data",
    "processed",
    "point",
    "datos_era5land_ecoregion_sa_qmap.RDS"
  )
)

# gap-filling

raw_data <- list(
  data = cbind(raw_data$data, era5land_data$data),
  xyz = rbind(raw_data$xyz, era5land_data$xyz)
)

## station from MPN ecoregion
idd_s <- raw_data$xyz[!is.na(raw_data$xyz$EQC) &
                        raw_data$xyz$EQC == 1 &
                        raw_data$xyz$ECOREGIONS == "MPN", ]$ID

## PCA covariables
pca_cov <- raw_data$xyz[,
  -match(c("ID", "NAME", "LON", "LAT", "ALT",
           "COUNTRY", "SOURCE", "ECOREGIONS",
           "SIZE", "EQC", "elevation", "latitude", "longitude"),
         colnames(raw_data$xyz))
]
pca_cov <- pca_cov[,
  -match(c("vrm", "tri", "tpi", "slope"),
         colnames(pca_cov))
]
pca_cov <- prcomp(scale(pca_cov))
pca_cov <- pca_cov$x[, 1:4]

new_sts <- raw_data$xyz[, c("ID", "LON", "LAT", "ALT")]
new_sts <- cbind(new_sts, pca_cov)

## stages for gap-filling
param_spt <- list(list(lmt_xy = 150 * 1000,
                       lmt_n = 16),
                  list(lmt_xy = 250 * 1000,
                       lmt_n = 16),
                  list(lmt_xy = 350 * 1000,
                       lmt_n = 16))

## models for gap-filling
selected_gf_models <- list("glm" = fillData_glm,
                           "svm" = fillData_svm,
                           "rf" = fillData_rf,
                           "xgboost" = fillData_xgboost,
                           "nn" = fillData_nn)

gap_filling_results <- lapply(
  seq_along(selected_gf_models),
  function(id_model) {

    print(names(selected_gf_models)[id_model])
    data_tobe_filled <- raw_data$data
    temp_stage_raw <- list()

    # stages
    for (stage in seq_along(param_spt)) {

      print(stage)

      res_gf_list <- parallel::mclapply(
        seq_along(idd_s),
        function(id_id) {

          # 1. get nearby points
          get_nearby_points(xy_target = idd_s[id_id],
                            xy_database = new_sts,
                            lmt_xy = param_spt[[stage]]$lmt_xy,
                            lmt_n = NA) %>%

            # 2. build matrix based on nearby points
            build_matrix(stations = .,
                         xts_database = data_tobe_filled,
                         xy_database = new_sts,
                         covars = colnames(new_sts)[-1]) %>%

            # 3. gap filling (mod_pre and err estimation,
            # max and min stations to be used)
            gap_filling(target_data = .,
                        lmt_min = 8,
                        lmt_max = param_spt[[stage]]$lmt_n,
                        FUN = selected_gf_models[[id_model]]) %>%

            # Here we merge qc_obs + mod_pred to be used in the next stage
            blending(target_data = .)

        }, mc.cores = 52
      )

      # saving results of stage
      temp_stage_raw[[stage]] <- res_gf_list

      # adding new qc_obs + mod_pred to data_tobe_filled
      data_tobe_filled_stage <-
        lapply(
          res_gf_list,
          function(ijx) {
            ijx$obs_pred
          }
        ) %>%
        do.call(cbind, .)
      colnames(data_tobe_filled_stage) <- idd_s
      data_tobe_filled[, idd_s] <- data_tobe_filled_stage

    }

    # 4. bias correction (quantile mapping of mod_pred -> bc_pred)
    # using mod_pred of the last stage (3)
    results_gf <- parallel::mclapply(
      seq_along(idd_s),
      function(adx) {

        adx_df <- temp_stage_raw[[3]][[adx]]
        adx_df$obs <- as.numeric(raw_data$data[, idd_s[adx]])
        adx_df <- bias_correction_reddPrec(target_data = adx_df)
        adx_df <- transform(adx_df,
                            obs_mod_pred = ifelse(is.na(obs), mod_pred, obs),
                            obs_bc_pred = ifelse(is.na(obs), bc_pred, obs))

        data.frame(adx_df,
                   station = idd_s[adx])

      }, mc.cores = 52
    ) %>% do.call(rbind, .)

    data.frame(results_gf, model = names(selected_gf_models)[id_model])

  }
)
gap_filling_results <- do.call(rbind, gap_filling_results)
gap_filling_results$model <- factor(
  gap_filling_results$model,
  levels = names(selected_gf_models)
)

# gap-filling results

gf_val_metrics <- parallel::mclapply(
  levels(gap_filling_results$model),
  function(id_model) {

    id_model_df <- gap_filling_results[gap_filling_results$model == id_model, ]

    id_model_res <- lapply(
      unique(id_model_df$station),
      function(id_id) {

        # 5. apply metrics

        id_id_df <- id_model_df[id_model_df$station == id_id, ]
        id_id_res_mod <- apply_metrics(
          target_data = id_id_df[, c("time_step", "obs", "mod_pred")]
        )
        id_id_res_bc <- apply_metrics(
          target_data = id_id_df[, c("time_step", "obs", "bc_pred")]
        )

        mod_out <- data.frame(id_id_res_mod, station = id_id, bc = "mod_pred")
        bc_out <- data.frame(id_id_res_bc, station = id_id, bc = "bc_pred")
        rbind(mod_out, bc_out)

      }
    )

    id_model_res <- do.call(rbind, id_model_res)
    data.frame(id_model_res, model = id_model)

  }, mc.cores = 5
)

gf_val_metrics <- do.call(rbind, gf_val_metrics)
gf_val_metrics <- gf_val_metrics[complete.cases(gf_val_metrics), ]
gf_val_metrics <- reshape2::melt(
  gf_val_metrics,
  id.vars = c("n_data", "station", "model", "bc"),
  variable.name = "metrics"
)

gf_val_metrics$bc <- factor(
  gf_val_metrics$bc, levels = c("mod_pred", "bc_pred")
)
gf_val_metrics$model <- factor(
  gf_val_metrics$model, levels = c("glm", "svm", "rf", "xgboost", "nn")
)
gf_val_metrics$metrics <- factor(
  gf_val_metrics$metrics,
  levels = c("dr", "mae", "rmse",
             "nmae", "nrmse", "zero_per",
             "accuracy", "precision", "recall",
             "f1", "balanced_accuracy", "g_mean")
)

gf_val_metrics_hline <- data.frame(
  metrics = levels(gf_val_metrics$metrics),  # Create data for lines
  hline = c(0.5, NA, NA, NA, NA, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
)

## boxplot of metrics
ggplot(gf_val_metrics,
       aes(x = model, y = value, colour = bc)) +
  geom_point(position = position_jitterdodge()) +
  geom_boxplot() +
  scale_x_discrete(drop = FALSE) +
  geom_hline(data = gf_val_metrics_hline,
             aes(yintercept = hline),
             linetype = "dashed",
             color = "black",
             alpha = 0.5) +
  theme_bw() +
  facet_wrap( ~factor(metrics,
                      c("dr", "mae", "rmse", "nmae", "nrmse", "zero_per",
                        "accuracy", "precision", "recall",
                        "f1", "balanced_accuracy", "g_mean")),
             ncol = 6,
             scales = "free_y") +
  theme(legend.position = "bottom")


## uncertainty

gf_err <- parallel::mclapply(
  levels(gap_filling_results$model),
  function(id_model) {

    id_model_df <- gap_filling_results[gap_filling_results$model == id_model,]

    id_model_res <- lapply(
      unique(id_model_df$station),
      function(id_id) {

        id_id_df <- id_model_df[id_model_df$station == id_id, ]
        mod_out <- data.frame(
          uncertainty = id_id_df[, "err"],
          station = id_id, bc = "mod_pred"
        )
        mod_out

      }
    )

    id_model_res <- do.call(rbind, id_model_res)
    data.frame(id_model_res, model = id_model
    )

  }, mc.cores = 5
)

gf_err <- do.call(rbind, gf_err)
gf_err$model <- factor(
  gf_err$model,
  levels = c("glm", "svm", "rf", "xgboost", "nn")
)

ggplot(gf_err,
       aes(x = model, y = uncertainty, colour = bc)) +
  geom_boxplot(outliers = FALSE) +
  scale_x_discrete(drop = FALSE) +
  theme_bw() +
  theme(legend.position = "bottom")


## time_series

eval_ts_plots(target_data = gap_filling_results,
              station_id = "id_AR_058",
              model_id = "glm",
              make_plot = TRUE)

eval_ts_plots(target_data = gap_filling_results,
              station_id = "id_AR_005",
              model_id = "rf",
              make_plot = TRUE)

eval_ts_plots(target_data = gap_filling_results,
              station_id = "id_AR_005",
              model_id = "rf",
              make_plot = FALSE) %>%
  lapply(xts::apply.monthly, sum) %>%
  do.call(cbind, .) %>%
  lattice::xyplot(type = "l", cex = .1)

eval_ts_plots(target_data = gap_filling_results,
              station_id = "id_AR_005",
              model_id = "rf",
              make_plot = FALSE) %>%
  lapply(xts::apply.monthly, sum) %>%
  lapply(xts::apply.yearly, sum) %>%
  do.call(cbind, .) %>%
  lattice::xyplot(type = "l", cex = .1)

eval_ts_plots(target_data = gap_filling_results,
              station_id = "id_AR_005",
              model_id = "rf",
              make_plot = FALSE) %>%
  lapply(xts::apply.monthly, sum) %>%
  lapply(xts::apply.yearly, sum) %>%
  do.call(cbind, .) %>%
  as.matrix() %>%
  lattice::splom()
