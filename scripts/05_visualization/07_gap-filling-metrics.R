rm(list = ls())

source("R/utils/pipe.R")
source("R/03_gap-filling/gf-metrics.R")
suppressMessages(library(ggplot2))

gf_results_path <- file.path("output", "03_gap-filling", "bc_pred")

gf_metrics_path <- file.path("output", "03_gap-filling", "metrics")
ecr <- c("NAS", "PAD", "CAS", "SAS", "AOL", "EHL", "GCH", "PPS", "MPN")

## check if everything was gap-filled

gf_all_files <- dir(gf_results_path, recursive = TRUE, full.names = TRUE)

gf_all <- parallel::mclapply(
  gf_all_files,
  function(idx) {
    idxa <- readRDS(idx)
    sum(!is.na(idxa$bc_pred))
  }, mc.cores = 100
) %>%
  unlist()

all_done <- all(
  gf_all == length(seq(as.Date("1960-01-01"), as.Date("2015-12-31"), by = "day"))
)

if (!all_done) {
  stop("Not all data was gap-filled")
}

# no negative values is not tested because if it happed
# it would be a problem with the gap-filling process itself


## apply metrics

for (ecr_i in ecr) {

  gf_ecr_i_metric_path <- file.path(gf_metrics_path, ecr_i)
  dir.create(gf_ecr_i_metric_path, showWarnings = FALSE, recursive = TRUE)

  gf_ecr_i_path <- file.path(gf_results_path, ecr_i)
  gf_ecr_i_path_files <- dir(gf_ecr_i_path, full.names = TRUE)

  parallel::mclapply(
    seq_along(gf_ecr_i_path_files),
    function(idx) {

      idx_df <- gf_ecr_i_path_files[idx]
      idx_df <- readRDS(idx_df)

      idx_res_mod <- apply_metrics(
        target_data = idx_df[, c("time_step", "obs", "mod_pred")]
      )
      mod_out <- data.frame(idx_res_mod,
                            station = unique(idx_df$station),
                            bc = "mod_pred",
                            ecr = ecr_i)

      idx_res_bc <- apply_metrics(
        target_data = idx_df[, c("time_step", "obs", "bc_pred")]
      )

      bc_out <- data.frame(idx_res_bc,
                           station = unique(idx_df$station),
                           bc = "bc_pred",
                           ecr = ecr_i)

      saveRDS(
        rbind(mod_out, bc_out),
        file.path(gf_ecr_i_metric_path,
                  paste(formatC(idx, width = 4, format = "d", flag = "0"),
                        "_",
                        unique(idx_df$station),
                        ".RDS",
                        sep = ""))
      )


    }, mc.cores = 100
  )

}

gf_val_metrics <- dir(gf_metrics_path, recursive = TRUE, full.names = TRUE)
gf_val_metrics <- parallel::mclapply(
  gf_val_metrics,
  function(idx) {
    idx <- readRDS(idx)
    idx
  }, mc.cores = 100
) %>%
  do.call(rbind, .)

gf_val_metrics <- gf_val_metrics[complete.cases(gf_val_metrics), ]
gf_val_metrics <- reshape2::melt(
  gf_val_metrics,
  id.vars = c("n_data", "station", "bc", "ecr"),
  variable.name = "metrics"
)

gf_val_metrics$ecr <- factor(
  gf_val_metrics$ecr,
  levels = c("NAS", "PAD", "CAS", "SAS", "AOL", "EHL", "GCH", "PPS", "MPN")
)

gf_val_metrics$bc <- factor(
  gf_val_metrics$bc, levels = c("mod_pred", "bc_pred")
)

### non-categorical metrics

gf_val_metrics_noncategorical <- gf_val_metrics[
  gf_val_metrics$metrics %in% c("dr", "mae", "rmse",
                              "nmae", "nrmse"),
]

gf_val_metrics_noncategorical$metrics <- factor(
  gf_val_metrics_noncategorical$metrics,
  levels = c("dr", "mae", "rmse",
             "nmae", "nrmse")
)

# custom_noncategorical <- list(
#   scale_y_continuous(limits = c(0.25, 1), breaks = c(0.25, 0.5, 0.75, 1),
#                      labels = scales::label_number(accuracy = 0.01)),
#   scale_y_continuous(limits = c(0, 10),
#                      labels = scales::label_number(accuracy = 0.1)),
#   scale_y_continuous(limits = c(0, 20),
#                      labels = scales::label_number(accuracy = 0.1)),
#   scale_y_continuous(limits = c(0, 0.1),
#                      labels = scales::label_number(accuracy = 0.01)),
#   scale_y_continuous(limits = c(0, 2),
#                      labels = scales::label_number(accuracy = 0.01))
# )


# ## boxplot of metrics
# noncategorical_bxplt_caxis <-
# ggplot(gf_val_metrics_noncategorical,
#        aes(x = ecr, y = value, colour = bc)) +
#   geom_boxplot(outliers = FALSE, lwd = .5, width = 0.9) +
#   scale_x_discrete(drop = FALSE) +
#   theme_bw() +
#   facet_wrap(~metrics,
#              ncol = 1,
#              scales = "free_y",
#              strip.position = "left") +
#   ggh4x::facetted_pos_scales(y = custom_noncategorical) +
#   theme(legend.position = "bottom") +
#   xlab("") + ylab("") +
#   theme(panel.spacing.y = unit(0.1, "lines"),
#         strip.background = element_blank(),
#         axis.text.x = element_text(size = 8),
#         axis.text.y = element_text(size = 8),
#         strip.text.y.left = element_text(angle = 90),
#         strip.placement = "outside") +
#   theme(legend.box.spacing = unit(0, "pt"),
#         legend.margin = margin(0, 0, 0, 0),
#         legend.title = element_blank())

# ggsave(
#   file.path("output", "05_visualization", "gf-noncategorical_bxplt_caxis.pdf"),
#   plot = noncategorical_bxplt_caxis,
#   device = "pdf",
#   dpi = 500, scale = 1,
#   width = 6, height = 6, units = "in"
# )


# noncategorical_bxplt <-
#   ggplot(gf_val_metrics_noncategorical,
#          aes(x = ecr, y = value, colour = bc)) +
#   geom_point(position = position_jitterdodge(dodge.width = 1),
#              size = 0.1, alpha = .25) +
#   geom_boxplot(outliers = FALSE, lwd = .3, width = 0.9) +
#   scale_x_discrete(drop = FALSE) +
#   theme_bw() +
#   facet_wrap(~metrics,
#              ncol = 1,
#              scales = "free_y",
#              strip.position = "left") +
#   theme(legend.position = "bottom") + 
#   xlab("") + ylab("") +
#   theme(panel.spacing.y = unit(0.1, "lines"),
#         strip.background = element_blank(),
#         axis.text.x = element_text(size = 8),
#         axis.text.y = element_text(size = 8),
#         strip.text.y.left = element_text(angle = 90),
#         strip.placement = "outside") +
#   theme(legend.box.spacing = unit(0, "pt"),
#         legend.margin = margin(0, 0, 0, 0),
#         legend.title = element_blank())

# ggsave(
#   file.path("output", "05_visualization", "gf-noncategorical_bxplt.pdf"),
#   plot = noncategorical_bxplt,
#   device = "pdf",
#   dpi = 500, scale = 1,
#   width = 6, height = 6, units = "in"
# )


gf_val_metrics_categorical <- gf_val_metrics[
  gf_val_metrics$metrics %in% c("wet_day",
                                "accuracy", "precision", "recall",
                                "f1", "bcc", "g_mean"),
]

gf_val_metrics_categorical$metrics <- factor(
  gf_val_metrics_categorical$metrics,
  levels = c("wet_day",
             "accuracy", "precision", "recall",
             "f1", "bcc", "g_mean")
)

# categorical_bxplt_caxis <-
#   ggplot(gf_val_metrics_categorical,
#          aes(x = ecr, y = value, colour = bc)) +
#   geom_boxplot(outliers = FALSE, lwd = .5, width = 0.9) +
#   scale_x_discrete(drop = FALSE) +
#   theme_bw() +
#   facet_wrap(~metrics,
#              ncol = 1,
#              scales = "free_y",
#              strip.position = "left") +
#   theme(legend.position = "bottom") +
#   xlab("") + ylab("") +
#   theme(panel.spacing.y = unit(0.2, "lines"),
#         strip.background = element_blank(),
#         axis.text.x = element_text(size = 8),
#         axis.text.y = element_text(size = 8),
#         strip.text.y.left = element_text(angle = 90),
#         strip.placement = "outside") +
#   theme(legend.position = "none")

# ggsave(
#   file.path("output", "05_visualization", "gf-categorical_bxplt_caxis.pdf"),
#   plot = categorical_bxplt_caxis,
#   device = "pdf",
#   dpi = 500, scale = 1,
#   width = 6, height = 8.5, units = "in"
# )

# categorical_bxplt <-
#   ggplot(gf_val_metrics_categorical,
#          aes(x = ecr, y = value, colour = bc)) +
#   geom_point(position = position_jitterdodge(dodge.width = 1),
#              size = 0.1, alpha = .25) +
#   geom_boxplot(outliers = FALSE, lwd = .3, width = 0.9) +
#   scale_x_discrete(drop = FALSE) +
#   theme_bw() +
#   facet_wrap(~metrics,
#              ncol = 1,
#              scales = "free_y",
#              strip.position = "left") +
#   theme(legend.position = "bottom") +
#   xlab("") + ylab("") +
#   theme(panel.spacing.y = unit(0.2, "lines"),
#         strip.background = element_blank(),
#         axis.text.x = element_text(size = 8),
#         axis.text.y = element_text(size = 8),
#         strip.text.y.left = element_text(angle = 90),
#         strip.placement = "outside") +
#   theme(legend.position = "none")

# ggsave(
#   file.path("output", "05_visualization", "gf-categorical_bxplt.pdf"),
#   plot = categorical_bxplt,
#   device = "pdf",
#   dpi = 500, scale = 1,
#   width = 6, height = 8.5, units = "in"
# )

##

mean_metrics_noncategorical <-
gf_val_metrics_noncategorical %>%
  .[, -c(1, 2)] %>%
  aggregate(
    value ~ bc + ecr + metrics, data = .,
    FUN = function(x){
      round(mean(x), 2)
    }
  ) %>%
  reshape2::dcast(metrics + bc ~ ecr)

mean_metrics_categorical <-
  gf_val_metrics_categorical %>%
  .[, -c(1, 2)] %>%
  aggregate(
    value ~ bc + ecr + metrics, data = .,
    FUN = function(x){
      round(mean(x), 2)
    }
  ) %>%
  reshape2::dcast(metrics + bc ~ ecr)

mean_metrics_noncategorical$SA <- apply(
  mean_metrics_noncategorical[, -c(1, 2)],
  1,
  function(idx) {
    round(mean(idx, na.rm = TRUE), 2)
  }
)

mean_metrics_categorical$SA <- apply(
  mean_metrics_categorical[, -c(1, 2)],
  1,
  function(idx) {
    round(mean(idx, na.rm = TRUE), 2)
  }
)


rbind(mean_metrics_noncategorical,
      mean_metrics_categorical) %>%
  write.csv(
    file.path("output",
              "05_visualization",
              "tab_05-gf-median-metrics.csv"),
    row.names = FALSE
  )


# spatial dr and balanced_accuracy

shp_esa_data <- terra::vect(
  file.path("data",
            "processed",
            "vector",
            "sa_eco_l3_2_paper.shp")
)

raw_data_xyz <- readRDS(
  file.path(
    "data",
    "processed",
    "point",
    "datos_SA_1960-2015_v1.2_03.RDS"
  )
)

raw_data_xyz <- raw_data_xyz$xyz


gf_val_metrics_dr_bca <- gf_val_metrics[
  gf_val_metrics$metrics %in% c("dr", "bcc"),
]

gf_val_metrics_dr_bca$metrics <- factor(
  gf_val_metrics_dr_bca$metrics,
  levels = c("dr", "bcc")
)

gf_val_metrics_dr_bca$value_cut <- cut(
  gf_val_metrics_dr_bca$value,
  breaks = c(-Inf, 0.5, 0.6, 0.7, 0.8, 0.9, Inf),
  labels = c("< .5", ".5-.6", ".6-.7", ".7-.8", ".8-.9", "> .9"),
  right = FALSE
)

colnames(gf_val_metrics_dr_bca)[2] <- "ID"

gf_val_metrics_dr_bca <- merge(
  gf_val_metrics_dr_bca,
  raw_data_xyz[, c("ID", "LON", "LAT")],
  by = "ID"
)

head(gf_val_metrics_dr_bca)

dr_bca_bxplt <-
  ggplot() +
  tidyterra::geom_spatvector(data = shp_esa_data,
                  colour = "black",
                  fill = NA,
                  size = .05) +
  geom_point(data = gf_val_metrics_dr_bca,
             aes(x = LON, y = LAT, colour = value_cut),
             size = .25) +
  viridis::scale_color_viridis("",
                               direction = -1,
                               discrete = TRUE,
                               drop = FALSE) +
  theme_bw() +
  facet_grid(metrics ~ bc, switch = "y") +
  guides(color = guide_legend(override.aes = list(size = 0.85))) +
  xlab("") + ylab("") +
  theme(axis.title = element_blank(),
        strip.background = element_blank(),
        legend.text = element_text(size = 8.5),
        legend.box.background = element_blank(),
        legend.key.spacing.y = unit(0, "cm"),
        legend.key.size = unit(0, 'cm'),
        legend.box = "vertical",
        legend.justification = c(0, 0),
        legend.position = c(0.725, 0.0010),
        legend.background = element_blank(),
        legend.title.position = "left",
        legend.title = element_text(size = 10, angle = 90, hjust = 0.5)) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))


# ggsave(
#   file.path("output", "05_visualization", "gf-dr-bcc-plot.pdf"),
#   plot = dr_bca_bxplt,
#   device = "pdf",
#   dpi = 500, scale = 1,
#   width = 5, height = 5, units = "in"
# )


# metrics vs wet_day

gf_val_metrics_dr_bca <- gf_val_metrics[
  gf_val_metrics$metrics %in% c("dr", "bcc"),
]

colnames(gf_val_metrics_dr_bca)[2] <- "ID"

gf_val_metrics_dr_bca <- merge(
  gf_val_metrics_dr_bca,
  raw_data_xyz[, c("ID", "LON", "LAT")],
  by = "ID"
)

gf_val_metrics_wet_day <- gf_val_metrics[
  gf_val_metrics$metrics %in% c("wet_day") & gf_val_metrics$bc == "bc_pred",
  c("station", "metrics", "value")
]

colnames(gf_val_metrics_wet_day) <- c("ID", "metrics", "wet_day")
gf_val_metrics_wet_day <- gf_val_metrics_wet_day[, c("ID", "wet_day")]

gf_val_metrics_dr_bca_xy <- merge(
  gf_val_metrics_dr_bca,
  gf_val_metrics_wet_day,
  by = "ID"
)

wetday_vs_dr_bcc <-
ggplot(gf_val_metrics_dr_bca_xy,
       aes(x = wet_day * 100, y = value),
       size = .01) +
  geom_point(colour = "gray50", alpha = 0.05) +
  geom_hline(yintercept = 0.5, col = "black", linetype = "dotted", size = 0.5) +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), se = FALSE) +
  facet_grid(metrics ~ bc, switch = "y") +
  scale_y_continuous(limits = c(0.2, 1)) +
  xlab("wet days (%)") + ylab("") +
  theme_bw() +
  theme(strip.background = element_blank(),
        axis.title = element_text(size = 8.5),
        axis.text = element_text(size = 7),
        strip.text.x = element_text(size = 8.5),
        strip.text.y = element_text(size = 8.5),
        strip.placement = "outside",
        plot.margin = unit(c(0, 0.25, 0, -0.1), "cm"))

# ggsave(
#   file.path("output", "05_visualization", "gf-wd-vs-dr-bcc-plot.pdf"),
#   plot = wetday_vs_dr_bcc,
#   device = "pdf",
#   dpi = 500, scale = 1,
#   width = 4, height = 4, units = "in"
# )

merged_plot <- cowplot::plot_grid(
  dr_bca_bxplt, wetday_vs_dr_bcc, ncol = 2,
  labels = c("a)", "b)"),
  label_colour = "gray20",
  label_size = 9,
  label_fontface = "plain"
)

ggsave(
  file.path("output",
            "05_visualization",
            "fig_08-gf-dr-bcc.pdf"),
  plot = merged_plot,
  device = "pdf",
  dpi = 500, scale = 1,
  width = 6.25, height = 4.5, units = "in"
)
