rm(list = ls())

suppressMessages(library(xts))
suppressMessages(library(PCAtools))
suppressMessages(library(corrplot))
suppressMessages(library(cowplot))
source("R/utils/pipe.R")

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


raw_data <- list(
  data = cbind(raw_data$data, era5land_data$data),
  xyz = rbind(raw_data$xyz, era5land_data$xyz)
)

all(colnames(raw_data$data) == raw_data$xyz$ID)


sts_target <- raw_data$xyz[,
  -match(c("ID", "NAME", "LON", "LAT", "ALT",
           "COUNTRY", "SOURCE", "ECOREGIONS",
           "SIZE", "EQC", "elevation",
           "latitude", "longitude"),
         colnames(raw_data$xyz))
]

# comparing PCA from PCAtools and prcomp
scaled_target <- scale(sts_target)
data_pca <- prcomp(scaled_target)
data_pca2 <- pca(t(scaled_target))

sum(
  (cumsum(summary(data_pca)$importance[2, ])  * 100) -
    round(cumsum(data_pca2$variance), 3)
)

# practically the same values -> data_pca2 == data_pca
# working with data_pca2

screeplot(data_pca2)

horn <- parallelPCA(t(scaled_target))
horn$n

elbow <- findElbowPoint(data_pca2$variance)
elbow

screeplot(data_pca2,
          vline = c(horn$n, elbow)) +
  annotate("text", x = horn$n + 0.1, y = 50,
           label = "Horn\'s", size = 3, vjust = 2) +
  annotate("text", x = elbow + 0.1, y = 50,
           label = "Elbow method", size = 3, vjust = -1)



corrplot(cor(scaled_target),
         method = "number",
         number.cex = .75)

corrplot(cor(data_pca2$rotated, scaled_target),
         method = "number",
         number.cex = .75)

# removing some covariables by autocorrelation
# https://stats.stackexchange.com/questions/50537/should-one-remove-highly-correlated-variables-before-doing-pca

check_multicollinearity <- spatialRF::auto_cor(
  x = scaled_target,
  cor.threshold = 0.8,
)

corrplot(check_multicollinearity$cor,
         method = "number",
         number.cex = .75,
         mar = c(0, 0, 0, 0))

scaled_target2 <- check_multicollinearity$selected.variables.df
data_pca3 <- pca(t(scaled_target2))

horn <- parallelPCA(t(scaled_target2))
horn$n

elbow <- findElbowPoint(data_pca3$variance)
elbow

scre1 <-
  screeplot(data_pca3,
            vline = c(horn$n)) +
  annotate("text", x = horn$n - 0.5, y = 100,
           label = "Horn\'s", size = 5, vjust = 2) +
  theme(plot.title = element_blank()) +
  theme(plot.margin = grid::unit(c(-2, 1, -5, 1), "mm")) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 13))


corrplot(cor(data_pca3$rotated, scaled_target2),
         method = "number",
         number.cex = .75,
         mar = c(0, 0, 0, 0))


par(mfrow = c(1, 2))
corrplot(check_multicollinearity$cor,
         method = "number",
         number.cex = .75,
         mar = c(0, 0, 0, 0))
corrplot(cor(data_pca3$rotated, scaled_target2),
         method = "number",
         number.cex = .75,
         mar = c(0, 0, 0, 0))
cor1 <- recordPlot()

final_plot <-
  plot_grid(
    cor1,
    scre1,
    ncol = 1,
    rel_widths = c(0.1, 0.3),
    rel_heights = c(0.6, 0.4),
    labels = c("a)", "b)")
  )

save_plot(
  file.path("output",
            "05_visualization",
            "fig_07-pca-analysis-topo.pdf"),
  final_plot,
  base_width = 10,
  base_height = 8,
  dpi = 300
)
