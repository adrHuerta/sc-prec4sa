get_modobs_xyz <- function(df_coord_obs,
                           grid_mod) {

  cells_xy <- terra::extract(grid_mod,
                             df_coord_obs[, c("LON", "LAT")],
                             cells = TRUE)[, 3]
  coord_xy <- terra::xyFromCell(grid_mod,
                                cells_xy)
  colnames(coord_xy) <- c("LON", "LAT")

  xyz_mod <- data.frame(ID = df_coord_obs$ID,
                        cell = cells_xy,
                        coord_xy)
  xyz_mod$cell <- factor(xyz_mod$cell)
  xyz_mod <- by(
    xyz_mod, xyz_mod$cell,
    function(ij) {

      if (nrow(ij) > 1) {

        dist_xy <- geosphere::distHaversine(
          df_coord_obs[
            match(ij[, c("ID")], df_coord_obs$ID), c("LON", "LAT")
          ],
          ij[, c("LON", "LAT")]
        )
        ij$dist_xy <- dist_xy
        ij[which.min(ij$dist_xy), c("ID", "cell", "LON", "LAT")]

      } else {

        ij

      }
    }
  )

  xyz_mod <- do.call(rbind, xyz_mod)
  xyz_mod$ID_obs <- xyz_mod$ID
  xyz_mod$ID <- paste(xyz_mod$ID, "mod", sep = "_")
  xyz_mod$cell <- as.numeric(levels(xyz_mod$cell))[xyz_mod$cell]
  xyz_mod <- xyz_mod[, c("ID_obs", "ID", "cell", "LON", "LAT")]
  rownames(xyz_mod) <- NULL

  return(xyz_mod)

}

qmap_era5land <- function(ts_obs,
                          ts_model,
                          window_c = 7) {

  ts_data <- cbind(ts_obs, ts_model)
  colnames(ts_data) <- c("obs", "model")

  # building daily climatology, centering at 01-01
  dailyVar <- sort(unique(format(time(ts_data), format = "%m-%d")))
  tail0 <- dailyVar[(length(dailyVar) - window_c + 1):length(dailyVar)]
  tail1 <- dailyVar[1:window_c]
  dailyVar <- c(tail0, dailyVar, tail1)

  dailyVar <-
    mapply(function(x, y){

      dailyVar[x:y]

    }, x = 1:366, y = (window_c * 2 + 1):length(dailyVar), SIMPLIFY = FALSE)

  dailyVar_qmap <-
    lapply(dailyVar, function(daily_var_i){

      # chunk data
      ts_data_i_abs <- ts_data[
        format(time(ts_data), format = "%m-%d") %in% daily_var_i
      ]
      # centroid date
      ts_data_i_abs_centroid <- ts_data_i_abs[
        format(time(ts_data_i_abs), format = "%m-%d") %in%
          daily_var_i[window_c + 1]
      ]
      # mean value and anomaly
      ts_data_i_cc_anom <- zoo::coredata(
        ts_data_i_abs[complete.cases(ts_data_i_abs), ]
      )
      is_full_of_zero <- all(ts_data_i_cc_anom[, "obs"] == 0)

      if (is_full_of_zero) {

        model_cc <- rep(0, nrow(ts_data_i_abs_centroid))
        xts::xts(model_cc, time(ts_data_i_abs_centroid))

      } else {

        qm_fit <- qmap::fitQmapRQUANT(
          ts_data_i_cc_anom[, "obs"],
          ts_data_i_cc_anom[, "model"],
          qstep = 0.1,
          nboot = 1,
          wet.day = TRUE
        )
        # applying in anomaly values and reversing to absolute values
        model_cc <- doQmapRQUANT_f(
          ts_data_i_abs_centroid[, "model"],
          qm_fit,
          type = "linear"
        )

        xts::xts(model_cc, time(ts_data_i_abs_centroid))

      }

    })

  round(
    do.call("rbind", dailyVar_qmap),
    1
  )

}

# original functions from qmap package
## ties = "ordered"

doQmapRQUANT_f <- function(x,fobj,...){ 
  if(!any(class(fobj)=="fitQmapRQUANT"))
    stop("class(fobj) should be fitQmapRQUANT")  
  UseMethod("doQmapRQUANT")
}

doQmapRQUANT.default <- function(x,fobj,slope.bound=c(lower=0,upper=Inf),
                                 type=c("linear","linear2","tricub"),...){
  type <- match.arg(type)
  fobj$par$slope <- c(max(fobj$par$slope["lower",],slope.bound["lower"]),
                      min(fobj$par$slope["upper",],slope.bound["upper"]))
  wet <-  if(!is.null(fobj$wet.day)){
    x>=fobj$wet.day
  } else {
    rep(TRUE,length(x))
  }
  out <- rep(NA,length.out=length(x)) 
  if(type%in%c("linear","linear2")){
    out[wet] <- approx(x=fobj$par$modq[,1], y=fobj$par$fitq[,1],
                       xout=x[wet], method="linear",
                       rule=2, ties= "ordered")$y
    if(type=="linear2"){
      if (any(k <- x > max(fobj$par$modq)))
        out[k]  <- max(fobj$par$fitq) +
          fobj$par$slope[2]*(x[k] - max(fobj$par$modq))
      if (any(k <- x < min(fobj$par$modq)))
        out[k]  <- min(fobj$par$fitq) +
          fobj$par$slope[1]*(x[k] - min(fobj$par$modq))
    } else if(type=="linear"){
      nq <- nrow(fobj$par$modq)
      largex <- x>fobj$par$modq[nq,1]
      if(any(largex)){
        max.delta <- fobj$par$modq[nq,1] - fobj$par$fitq[nq,1]
        out[largex] <- x[largex] - max.delta
      }
    }
  } else if(type=="tricub"){
    sfun <- splinefun(x=fobj$par$modq[,1], y=fobj$par$fitq[,1],
                      method="monoH.FC", ties = "ordered")
    out[wet] <- sfun(x[wet])
  } 
  out[!wet] <- 0
  if(!is.null(fobj$wet.day))
    out[out<0] <- 0
  return(out)
}


doQmapRQUANT.matrix <- function(x,fobj,...){
  if(ncol(x)!=ncol(fobj$par$modq))
    stop("'ncol(x)' and 'nrow(fobj$par$modq)' should be eaqual\n")  
  NN <- ncol(x)
  hind <- 1:NN
  names(hind) <- colnames(x)
  hf <- list()
  class(hf) <- class(fobj)
  xx <- sapply(hind,function(i){
    ## hf <- fobj
    hf$par$modq <- matrix(fobj$par$modq[,i],ncol=1)
    hf$par$fitq <- matrix(fobj$par$fitq[,i],ncol=1)
    hf$par$slope <- matrix(fobj$par$slope[,i],ncol=1,
                           dimnames=list(c("lower","upper"),NULL))
    hf$wet.day <- fobj$wet.day[i]
    tr <- try(doQmapRQUANT.default(x[,i],hf,...),silent=TRUE)
    if(class(tr)=="try-error"){
      warning("Quantile mapping for ",names(hind)[i],
              " failed NA's produced.")
      tr <- rep(NA,nrow(x))
    }
    return(tr)
  })
  rownames(xx) <- rownames(x)
  return(xx)
}

doQmapRQUANT.data.frame <- function(x,fobj,...){
  x <- as.matrix(x)
  x <- doQmapRQUANT.matrix(x,fobj,...)
  x <- as.data.frame(x)
  return(x)
}
