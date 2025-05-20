get_levels_sg <- function(xts_obj) {

  df_ngaps <- get_ngaps(xts_obj = xts_obj)
  df_ngaps_levels <- df_ngaps[,
    -match(c("year", "year_size"), colnames(df_ngaps))
  ]
  df_ngaps_levels[df_ngaps_levels > 0] <- NA
  df_ngaps_levels[df_ngaps_levels >= 0] <- 1

  ngaps <- max(colSums(df_ngaps_levels, na.rm = TRUE))
  percent_ngaps <- ngaps * 100 / nrow(df_ngaps_levels)

  if (percent_ngaps < 1) {

    response <- 0

  } else if (percent_ngaps < 20) {

    response <- 1

  } else {

    response <- 2

  }

  return(response)

}



get_ngaps <- function(xts_obj,
                      lmn_yday = 365 * 80/100,
                      make_plot = FALSE) {

  size_y <- xts::apply.yearly(xts_obj, function(x) sum(!is.na(x)))

  var_01 <- xts::apply.yearly(
    xts_obj,
    function(x) sum(!is.na(x[x < 1 & x > 0]))
  )
  var_12 <- xts::apply.yearly(
    xts_obj,
    function(x) sum(!is.na(x[x < 2 & x > 1]))
  )
  var_23 <- xts::apply.yearly(
    xts_obj,
    function(x) sum(!is.na(x[x < 3 & x > 2]))
  )
  var_34 <- xts::apply.yearly(
    xts_obj,
    function(x) sum(!is.na(x[x < 4 & x > 3]))
  )
  var_45 <- xts::apply.yearly(
    xts_obj,
    function(x) sum(!is.na(x[x < 5 & x > 4]))
  )

  var_df <- data.frame(year = as.numeric(format(time(var_01), "%Y")),
                       var01 = as.numeric(var_01),
                       var12 = as.numeric(var_12),
                       var23 = as.numeric(var_23),
                       var34 = as.numeric(var_34),
                       var45 = as.numeric(var_45),
                       year_size = as.numeric(size_y))

  var_df <- var_df[var_df$year_size > lmn_yday, ]
  rownames(var_df) <- NULL

  if (isTRUE(make_plot)) {

    var_df_plot <- xts::xts(
      var_df[, -match(c("year", "year_size"), colnames(var_df))],
      as.Date(paste(var_df$year, "-01-01", sep = ""))
    )

    print(plot(var_df_plot,
               lwd = 7,
               pch = 1,
               type = "o",
               main = colnames(xts_obj))
    )

  }

  return(var_df)
  
}
