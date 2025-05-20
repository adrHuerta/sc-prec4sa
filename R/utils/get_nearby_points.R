get_nearby_points <- function(xy_target,
                              xy_database,
                              lmt_xy = 400000,
                              lmt_n = 10) {

  target <- xy_database[match(xy_target, xy_database$ID), c("LON", "LAT")]
  nearby <- xy_database[, c("LON", "LAT")]

  lmt_n <- ifelse(is.na(lmt_n), nrow(xy_database) - 1, lmt_n)
  lmt_xy <- ifelse(is.na(lmt_xy), Inf, lmt_xy)

  out <- xy_database[, c("ID", "LON", "LAT")]
  out$distance <- geosphere::distHaversine(target, nearby)
  out <- out[out$distance < lmt_xy, ]
  out <- out[order(out$distance), ]
  out <- out[1:(lmt_n + 1), ]
  out <- out[complete.cases(out), ]
  out <- as.character(out$ID)

  return(out)

}