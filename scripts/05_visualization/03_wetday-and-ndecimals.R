rm(list = ls())

source("R/utils/pipe.R")
suppressMessages(library(ggplot2))
suppressMessages(library(tidyterra))
suppressMessages(library(terra))

raw_data <- readRDS(
  file.path("data",
            "processed",
            "point",
            "datos_SA_1960-2015_v1.2_01.RDS")
)

shp_esa_data <- vect(
  file.path("data",
            "processed",
            "vector",
            "sa_eco_l3_2_paper.shp")
)

# ndecimals function
nplaces <- function(idd) {
  sapply(idd, function(y) {
    nchar(
      sub("^-?\\d*\\.?","",
        format(y, scientific = FALSE)
      )
    )
  }
  )
}

n_decimals <- parallel::mclapply(
  raw_data$data,
  function(xxx) {

    xxx <- zoo::coredata(xxx)
    xxx <- xxx[complete.cases(xxx)]
    if (length(xxx) < 1) {

      NA

    } else {

      paste0(as.character(sort(unique(nplaces(xxx)))),
             collapse = "")

    }
  }, mc.cores = 100
)

wd_by_station <- parallel::mclapply(
  raw_data$data,
  function(xxx) {

    sample_ts <- xxx
    sample_ts <- sample_ts[!is.na(sample_ts)]
    round(length(sample_ts[sample_ts >= 0.1]) / length(sample_ts), 2)

  }, mc.cores = 50
)

wd_by_station <- unlist(wd_by_station)
raw_data$xyz$WetDay <- wd_by_station * 100
raw_data$xyz$Wet_Day <- cut(raw_data$xyz$WetDay, right = TRUE,
                            breaks = c(-Inf, 5,
                                       10, 20,
                                       30, 50,
                                       70, Inf),
                            labels = c("<= 5", "5 - 10",
                                       "10 - 20", "20 - 30",
                                       "30 - 50", "50 - 70",
                                       ">= 70"))

n_decimals <- unlist(n_decimals)
raw_data$xyz$Scale <- n_decimals
raw_data$xyz$Scale <- factor(raw_data$xyz$Scale,
                             levels = c("0", "01", "1"),
                             labels = c("[0]", "[0, 1]", "[1]"))


n_decimals_plot <-
  ggplot() +
  geom_spatvector(data = shp_esa_data,
                  colour = "black",
                  fill = NA,
                  size = .01) +
  geom_point(data = raw_data$xyz,
             aes(x = LON,
                 y = LAT,
                 colour = Scale,
                 fill = Scale,
                 size = Scale,
                 alpha = Scale),
             shape = 21) +
  scale_fill_manual(values = c("red", "gray70", "blue")) +
  scale_colour_manual(values = c("red", "gray70", "blue")) +
  scale_size_manual(values = c("[0]" = 1, "[0, 1]" = .5, "[1]" = 1)) +
  scale_alpha_manual(values = c("[0]" = 1, "[0, 1]" = .2, "[1]" = 1)) +
  scale_x_continuous(limits = c(-82, -33),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(-59, 14),
                     expand = c(0, 0)) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  theme_bw() +
  theme(axis.title = element_blank(),
        legend.text = element_text(size = 8.5),
        legend.box.background = element_blank(),
        legend.key.spacing.y = unit(0, "cm"),
        legend.key.size = unit(0, "cm"),
        legend.box = "vertical",
        legend.justification = c(0, 0),
        legend.position = c(0.575, 0.1),
        legend.background = element_blank(),
        legend.title.position = "left",
        legend.title = element_text(size = 10, angle = 90, hjust = 0.5)) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())

wet_day_plot <-
  ggplot() +
  geom_spatvector(data = shp_esa_data,
                  colour = "black",
                  fill = NA,
                  size = .01) +
  geom_point(data = raw_data$xyz,
             aes(x = LON,
                 y = LAT,
                 colour = Wet_Day,
                 fill = Wet_Day),
             size = .5,
             shape = 21) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(7, name = "RdYlBu")) +
  scale_colour_manual(values = RColorBrewer::brewer.pal(7, name = "RdYlBu")) +
  scale_x_continuous(limits = c(-82, -33),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(-59, 14),
                     expand = c(0, 0)) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  theme_bw() +
  theme(axis.title = element_blank(),
        legend.text = element_text(size = 8.5),
        legend.box.background = element_blank(),
        legend.key.spacing.y = unit(0, "cm"),
        legend.key.size = unit(0, "cm"),
        legend.box = "vertical",
        legend.justification = c(0, 0),
        legend.position = c(0.575, 0.075),
        legend.background = element_blank(),
        legend.title.position = "left",
        legend.title = element_text(size = 10, angle = 90, hjust = 0.5)) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank()) +
  labs(fill = "wet days (%)", colour = "wet days (%)")

final_plot <-
  cowplot::plot_grid(
    wet_day_plot +
      annotate("text", x = -79.5, y = 12, label = " a)",
               colour = "gray20", size = 4),
    n_decimals_plot +
      annotate("text", x = -79.5, y = 12, label = " b)",
               colour = "gray20", size = 4),
    ncol = 2
  )

ggsave(
  file.path("output", "05_visualization", "fig_03-wetday-and-ndecimals.pdf"),
  plot = final_plot,
  device = "pdf",
  dpi = 500, scale = 1,
  width = 5, height = 4, units = "in"
)
