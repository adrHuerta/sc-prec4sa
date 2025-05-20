get_ts_borderline <- function(xts_obj) {

  xts_obj1 <- xts_obj[complete.cases(xts_obj)]

  border_filter <- zoo::rollapply(
    zoo::as.zoo(xts_obj1),
    width = 180,
    function(x) max(x, na.rm = TRUE),
    align = "center",
    partial = TRUE
  )

  border_filter <- zoo::rollapply(
    border_filter,
    width = 90,
    function(x) max(x, na.rm = TRUE),
    align = "center",
    partial = TRUE
  )

  border_filter <- zoo::rollapply(
    border_filter,
    width = 180 * 1,
    function(x) median(x, na.rm = TRUE),
    align = "center",
    partial = TRUE
  )

  border_filter <- round(border_filter, digits = -1)

  return(border_filter)

}

get_lenght_borderline <- function(xts_obj, make_plot = FALSE) {

  border_line <- get_ts_borderline(xts_obj = xts_obj)
  border_line_nona <- border_line[complete.cases(border_line)]

  border_line_df <- data.frame(
    value = factor(as.numeric(border_line_nona)),
    year = as.numeric(format(time(border_line_nona), "%Y"))
  )
  border_line_df  <- reshape2::dcast(
    border_line_df,
    year ~ value,
    fun.aggregate = length
  )
  border_line_df$size <- as.numeric(
    xts::apply.yearly(
      border_line_nona,
      function(idd) sum(!is.na(idd))
    )
  )

  if (isTRUE(make_plot)) {

    print(plot(xts_obj, type = "p", main = colnames(xts_obj)))
    print(lines(xts::as.xts(border_line), col = "red", lwd = 2))

  }

  return(border_line_df)

}

get_level_trunc <- function(xts_obj) {

  df_border <- get_lenght_borderline(xts_obj = xts_obj)
  df_flagg <-
    df_border[, -match(c("year", "size"), colnames(df_border))] -
    df_border$size
  df_flagg[df_flagg != 0] <- NA

  trunc_lenght <- sapply(df_flagg, function(idd) max(rle(idd)$lengths))
  trunc_lenght <- max(trunc_lenght)

  if ( trunc_lenght < 3) {

    response <- 0

  } else {

    if (trunc_lenght >= 3 & trunc_lenght < 5) {

      response <- 1

    } else {

      response <- 2

    }

  }

  return(response)

}