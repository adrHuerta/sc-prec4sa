rm(list = ls())

library(xts)
library(ggplot2)
source("R/utils/pipe.R")
source("R/utils/get_nearby_points.R")
source("R/03_gap-filling/gf-machinery.R")
source("R/03_gap-filling/gf-models.R") # neuralnet xgboost randomForest e1071
source("R/03_gap-filling/gf-helpers.R")
source("R/03_gap-filling/gf-metrics.R") # metrica


ch_data <- readRDS(
  file.path("data",
            "processed",
            "point",
            "ch_gf_data.RDS")
)
colnames(ch_data$sts)[2:3] <- c("LON", "LAT")

# gap-filling

## using first 4 PCs of topo covariables
ch_data_covs <- prcomp(scale(ch_data$covs[, -c(9:11)]))
ch_data_covs <- cbind(ch_data$sts, ch_data_covs$x[, 1:4])

selected_ID <- sample(ch_data_covs$ID, 50)
selected_gf_models <- list("glm" = fillData_glm,
                           "svm" = fillData_svm,
                           "rf" = fillData_rf,
                           "xgboost" = fillData_xgboost,
                           "nn" = fillData_nn)

gap_filling_results <- lapply(

  # gf model
  seq_along(selected_gf_models),
  function(id_model) {

    print(names(selected_gf_models)[id_model])

    # id station
    results_id <- parallel::mclapply(
      seq_along(selected_ID),
      function(id_id) {

        # 1. get nearby points
        get_nearby_points(xy_target = selected_ID[id_id],
                          xy_database = ch_data$sts,
                          lmt_xy = NA,
                          lmt_n = NA) %>%

          # 2. build matrix based on nearby points
          build_matrix(stations = .,
                       xts_database = ch_data$data,
                       xy_database = ch_data_covs,
                       covars = colnames(ch_data_covs)[-1]) %>%

          # 3. gap filling (mod_pre and err estimation,
          # max and min stations to be used)
          gap_filling(target_data = .,
                      lmt_min = 8,
                      lmt_max = 15,
                      FUN = selected_gf_models[[id_model]]) %>%

          # 4. bias correction (quantile mapping of mod_pred -> bc_pred)
          bias_correction_reddPrec(target_data = .) %>%

          data.frame(., station = selected_ID[id_id])

      }, mc.cores = 50
    )

    results_id <- do.call(rbind, results_id)
    data.frame(results_id,
               model = names(selected_gf_models)[id_model])

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
             "nmae", "nrmse", "wet_day",
             "accuracy", "precision", "recall",
             "f1", "bcc", "g_mean")
)

gf_val_metrics_hline <- data.frame(
  metrics = levels(gf_val_metrics$metrics),  # Create data for lines
  hline = c(0.5, NA, NA, NA, NA, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
)

## boxplot of metrics
ggplot(gf_val_metrics,
       aes(x = model, y = value, colour = bc)) +
  # geom_point(position = position_jitterdodge()) +
  geom_boxplot() +
  scale_x_discrete(drop = FALSE, guide = guide_axis(n.dodge = 2)) +
  geom_hline(data = gf_val_metrics_hline,
             aes(yintercept = hline),
             linetype = "dashed",
             color = "black",
             alpha = 0.5) +
  theme_bw() +
  facet_wrap(~factor(metrics,
                      c("dr", "mae", "rmse", "nmae", "nrmse", "wet_day",
                        "accuracy", "precision", "recall",
                        "f1", "bcc", "g_mean")),
             ncol = 6,
             scales = "free_y") +
  theme(legend.position = "bottom") +
  labs(colour = "Model output:")

# boxplot of metrics (without wet_day)
gf_val_metrics_s <- gf_val_metrics[gf_val_metrics$metrics != "wet_day", ]
gf_val_metrics_s$metrics <- factor(
  gf_val_metrics_s$metrics,
  levels = c("dr", "mae", "rmse",
             "nmae", "nrmse",
             "accuracy", "precision", "recall",
             "f1", "bcc", "g_mean")
)

gf_val_metrics_hline_s <- data.frame(
  metrics = levels(gf_val_metrics_s$metrics),  # Create data for lines
  hline = c(0.5, NA, NA, NA, NA, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
)

ch_gf_res <-
ggplot(gf_val_metrics_s,
       aes(x = model, y = value, colour = bc)) +
  # geom_point(position = position_jitterdodge()) +
  geom_boxplot() +
  scale_x_discrete(drop = FALSE, guide = guide_axis(n.dodge = 2)) +
  geom_hline(data = gf_val_metrics_hline_s,
             aes(yintercept = hline),
             linetype = "dashed",
             color = "black",
             alpha = 0.5) +
  theme_bw() +
  facet_wrap(~factor(metrics,
                      c("dr", "mae", "rmse", "nmae", "nrmse",
                        "accuracy", "precision", "recall",
                        "f1", "bcc", "g_mean")),
             ncol = 4,
             scales = "free_y") +
  theme(legend.position = "bottom") +
  labs(colour = "Model output:")


ggsave(
  file.path("output", "05_visualization", "fig_nn-gf-ch.pdf"),
  plot = ch_gf_res +
  theme(panel.spacing.y = unit(0.2, "lines"),
        strip.background = element_blank(),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        strip.placement = "outside") +
  ylab(""),
  device = "pdf",
  dpi = 500, scale = 1,
  width = 6, height = 5, units = "in"
)

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
