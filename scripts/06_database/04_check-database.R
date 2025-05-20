# rm(list = ls())

# suppressMessages(library(xts))
# suppressMessages(library(ggplot2))
# source("R/utils/pipe.R")

# dtb_results_path <- file.path("output", "06_database", "data_full")

# ecr <- c("NAS", "PAD", "CAS", "SAS", "AOL", "EHL", "GCH", "PPS", "MPN")

# dbt_mean <- list()
# for (ecr_i in seq_along(ecr)) {

#   dtb_ecri_analysis_path <- file.path(dtb_results_path, ecr[ecr_i])
#   dtb_ecri_analysis_path_files <- dir(dtb_ecri_analysis_path, full.names = TRUE)

#   # prcptot and r1mm
#   prcptot_r1mm <- parallel::mclapply(
#     dtb_ecri_analysis_path_files,
#     function(id_id) {

#       daily_data <- read.csv(id_id, header = TRUE)
#       daily_data <- xts::xts(
#         daily_data[, -1],
#         as.Date(daily_data$time_step)
#       )

#       prcptot <- lapply(
#         daily_data[, c("obs_mod", "obs_bc", "hmg_obs_mod", "hmg_obs_bc")]
#         , function(x) xts::apply.yearly(x, sum)
#       )
#       prcptot <- do.call(cbind, prcptot)


#       prcptot <- reshape2::melt(
#         data.frame(as.data.frame(prcptot),
#                    time = 1960:2015,
#                    var = "prcptot"), c("time", "var")
#       )

#       r1mm <- lapply(
#         daily_data[, c("obs_mod", "obs_bc", "hmg_obs_mod", "hmg_obs_bc")]
#         , function(x) xts::apply.yearly(x, function(y) length(y[y >= 0.1]))
#       )
#       r1mm <- do.call(cbind, r1mm)

#       r1mm <- reshape2::melt(
#         data.frame(as.data.frame(r1mm),
#                    time = 1960:2015,
#                    var = "r1mm"), c("time", "var")
#       )


#       rbind(r1mm, prcptot)

#     }, mc.cores = 200
#   )
#   prcptot_r1mm <- do.call(rbind, prcptot_r1mm)

#   dbt_mean[[ecr_i]] <- data.frame(
#     aggregate(value ~ time + variable + var, data = prcptot_r1mm, mean),
#     ecoregion = ecr[ecr_i]
#   )

# }



# # prcptot and r1mm
# prcptot_r1mmm <- do.call(rbind, dbt_mean)

# prcptot_r1mmm$variable <- factor(
#   prcptot_r1mmm$variable,
#   levels = c("obs_mod", "hmg_obs_mod", "obs_bc", "hmg_obs_bc")
# )

# prcptot_r1mmm$ecoregion <- factor(
#   prcptot_r1mmm$ecoregion,
#   levels = c("NAS", "PAD", "CAS", "SAS", "AOL", "EHL", "GCH", "PPS", "MPN")
# )


# prcptot_plot <-
# ggplot(prcptot_r1mmm[prcptot_r1mmm$var == "prcptot", ],
#        aes(x = time,
#            y = value,
#            colour = variable)) +
#   geom_line(linewidth = 1) +
#   scale_colour_manual(values = c("#4E84C4", "#293352", "#FFDB6D", "#C4961A")) +
#   guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
#   scale_x_continuous(limits = c(1960, 2015), breaks = seq(1960, 2015, 5)) +
#   facet_wrap(~ ecoregion, scales = "free_y", ncol = 2) +
#   ylab("Total precipitation (PRCPTOT)") + xlab("") +
#   theme_bw() +
#   theme(legend.position = c(0.75, 0.075)) +
#   theme(panel.spacing.y = unit(0, "lines"),
#         strip.background = element_blank(),
#         axis.text.x = element_text(size = 8),
#         axis.text.y = element_text(size = 8),
#         strip.placement = "outside")  +
#   theme(legend.box.spacing = unit(0, "pt"),
#         legend.margin = margin(0, 0, 0, 0),
#         legend.title = element_blank())

# ggsave(
#   file.path("output", "05_visualization", "hmg-prcptot-series-csv.pdf"),
#   plot = prcptot_plot,
#   device = "pdf",
#   dpi = 500, scale = 1,
#   width = 8, height = 6.25, units = "in"
# )


# r1mm_plot <-
# ggplot(prcptot_r1mmm[prcptot_r1mmm$var == "r1mm", ],
#        aes(x = time,
#            y = value,
#            colour = variable)) +
#   geom_line(linewidth = 1) +
#   scale_colour_manual(values = c("#4E84C4", "#293352", "#FFDB6D", "#C4961A")) +
#   guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
#   scale_x_continuous(limits = c(1960, 2015), breaks = seq(1960, 2015, 5)) +
#   facet_wrap(~ ecoregion, scales = "free_y", ncol = 2) +
#   ylab("Number of wet days (R1mm)") + xlab("") +
#   theme_bw() +
#   theme(legend.position = c(0.75, 0.075)) +
#   theme(panel.spacing.y = unit(0, "lines"),
#         strip.background = element_blank(),
#         axis.text.x = element_text(size = 8),
#         axis.text.y = element_text(size = 8),
#         strip.placement = "outside")  +
#   theme(legend.box.spacing = unit(0, "pt"),
#         legend.margin = margin(0, 0, 0, 0),
#         legend.title = element_blank())

# ggsave(
#   file.path("output", "05_visualization", "hmg-r1mm-series-csv.pdf"),
#   plot = r1mm_plot,
#   device = "pdf",
#   dpi = 500, scale = 1,
#   width = 8, height = 6.25, units = "in"
# )

# ###


# dbt_mean <- list()
# for (ecr_i in seq_along(ecr)) {

#   dtb_ecri_analysis_path <- file.path(dtb_results_path, ecr[ecr_i])
#   dtb_ecri_analysis_path_files <- dir(dtb_ecri_analysis_path, full.names = TRUE)

#   # prcptot and r1mm
#   prcptot_r1mm <- parallel::mclapply(
#     dtb_ecri_analysis_path_files,
#     function(id_id) {

#       daily_data <- read.csv(id_id, header = TRUE)
#       daily_data <- xts::xts(
#         daily_data[, -1],
#         as.Date(daily_data$time_step)
#       )

#       prcptot <- lapply(
#         daily_data[, c("obs_mod", "obs_bc", "hmg_obs_mod", "hmg_obs_bc", "mod_pred", "bc_pred", "qc_obs")]
#         , function(x) xts::apply.yearly(x, function(y) sum(y))
#       )

#       prcptot <- do.call(cbind, prcptot)


#       prcptot <- reshape2::melt(
#         data.frame(as.data.frame(prcptot),
#                    time = 1960:2015,
#                    var = "prcptot"), c("time", "var")
#       )

#       r1mm <- lapply(
#         daily_data[, c("obs_mod", "obs_bc", "hmg_obs_mod", "hmg_obs_bc", "mod_pred", "bc_pred", "qc_obs")]
#         , function(x) xts::apply.yearly(x, function(y){
#             if(anyNA(y)) {
#                 NA
#             } else {
#                 length(y[y >= 0.1])
#             }
#         })
#       )
#       r1mm <- do.call(cbind, r1mm)

#       r1mm <- reshape2::melt(
#         data.frame(as.data.frame(r1mm),
#                    time = 1960:2015,
#                    var = "r1mm"), c("time", "var")
#       )


#       rbind(r1mm, prcptot)

#     }, mc.cores = 200
#   )
#   prcptot_r1mm <- do.call(rbind, prcptot_r1mm)

#   dbt_mean[[ecr_i]] <- data.frame(
#     aggregate(value ~ time + variable + var, data = prcptot_r1mm, mean, na.action = NULL),
#     ecoregion = ecr[ecr_i]
#   )

# }



# # prcptot and r1mm
# prcptot_r1mmm <- do.call(rbind, dbt_mean)

# prcptot_r1mmm$variable <- factor(
#   prcptot_r1mmm$variable,
#   levels = c("obs_mod", "hmg_obs_mod", "obs_bc", "hmg_obs_bc", "mod_pred", "bc_pred", "qc_obs")
# )

# prcptot_r1mmm$ecoregion <- factor(
#   prcptot_r1mmm$ecoregion,
#   levels = c("NAS", "PAD", "CAS", "SAS", "AOL", "EHL", "GCH", "PPS", "MPN")
# )


# prcptot_plot <-
# ggplot(prcptot_r1mmm[prcptot_r1mmm$var == "prcptot", ],
#        aes(x = time,
#            y = value,
#            colour = variable)) +
#   geom_line(linewidth = 1) +
#   scale_colour_manual(values = c("#4E84C4", "#293352", "#FFDB6D", "#C4961A", "#4BB446", "#AF46B4", "black")) +
#   guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
#   scale_x_continuous(limits = c(1960, 2015), breaks = seq(1960, 2015, 5)) + 
#   facet_wrap(~ ecoregion, scales = "free_y", ncol = 2) +
#   ylab("Total precipitation (PRCPTOT)") + xlab("") +
#   theme_bw() +
#   theme(legend.position = c(0.75, 0.075)) +
#   theme(panel.spacing.y = unit(0, "lines"),
#         strip.background = element_blank(),
#         axis.text.x = element_text(size = 8),
#         axis.text.y = element_text(size = 8),
#         strip.placement = "outside")  +
#   theme(legend.box.spacing = unit(0, "pt"),
#         legend.margin = margin(0, 0, 0, 0),
#         legend.title = element_blank())

# ggsave(
#   file.path("output", "05_visualization", "hmg-prcptot-series-csv-all.pdf"),
#   plot = prcptot_plot,
#   device = "pdf",
#   dpi = 500, scale = 1,
#   width = 8, height = 6.25, units = "in"
# )


# r1mm_plot <-
# ggplot(prcptot_r1mmm[prcptot_r1mmm$var == "r1mm", ],
#        aes(x = time,
#            y = value,
#            colour = variable)) +
#   geom_line(linewidth = 1) +
#   scale_colour_manual(values = c("#4E84C4", "#293352", "#FFDB6D", "#C4961A", "#4BB446", "#AF46B4", "black")) +
#   guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
#   scale_x_continuous(limits = c(1960, 2015), breaks = seq(1960, 2015, 5)) +
#   facet_wrap(~ ecoregion, scales = "free_y", ncol = 2) +
#   ylab("Number of wet days (R1mm)") + xlab("") +
#   theme_bw() +
#   theme(legend.position = c(0.75, 0.075)) +
#   theme(panel.spacing.y = unit(0, "lines"),
#         strip.background = element_blank(),
#         axis.text.x = element_text(size = 8),
#         axis.text.y = element_text(size = 8),
#         strip.placement = "outside")  +
#   theme(legend.box.spacing = unit(0, "pt"),
#         legend.margin = margin(0, 0, 0, 0),
#         legend.title = element_blank())

# ggsave(
#   file.path("output", "05_visualization", "hmg-r1mm-series-csv-all.pdf"),
#   plot = r1mm_plot,
#   device = "pdf",
#   dpi = 500, scale = 1,
#   width = 8, height = 6.25, units = "in"
# )
