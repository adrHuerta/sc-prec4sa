enhanced_qc_plus_plot <- function(xts_obj) {

  #### theme for plots ####

  theme.novpadding <-
    list(par.main.text = list(font = 1, just = "center"),
         layout.heights =
           list(main = .1,
                top.padding = 0,
                main.key.padding = 0,
                key.axis.padding = 0,
                axis.xlab.padding = -1,
                xlab.key.padding = 0,
                key.sub.padding = 0,
                bottom.padding = 3),
         layout.widths =
           list(left.padding = 0,
                key.ylab.padding = 0,
                ylab.axis.padding = 0,
                axis.key.padding = 0,
                right.padding = 1))

  theme.novpadding2 <-
    list(par.main.text = list(font = 1, just = "center"),
         layout.heights =
           list(main = 0.1,
                top.padding = 0,
                main.key.padding = 0,
                key.axis.padding = 0,
                axis.xlab.padding = -1,
                xlab.key.padding = 0,
                key.sub.padding = 0,
                bottom.padding = 2),
         layout.widths =
           list(left.padding = 0,
                key.ylab.padding = 0,
                ylab.axis.padding = 0,
                axis.key.padding = 0,
                right.padding = -1))


  #### enhanced qc plots #####

  # simple time serie
  xts_ts_plt <- lattice::xyplot(
    xts_obj, type = "p", cex = .1, pch = 19, col = "black",
    xlab = "", ylab = "(mm)", main = "time series",
    xlim = c(time(xts_obj)[1], time(xts_obj)[length(time(xts_obj))]),
    ylim = c(min(xts_obj, na.rm = TRUE), max(xts_obj, na.rm = TRUE)),
    par.settings = theme.novpadding,
    scales = list(y = list(rot = 90))
  )

  # truncation shape

  xts_trunc <- get_ts_borderline(xts_obj = xts_obj)
  xts_trunc <- lattice::xyplot(
    xts_trunc, type = "l", cex = .1, pch = 19, col = "red", lwd = 3,
    xlab = "", ylab = "(mm)", main = "truncation",
    xlim = c(time(xts_trunc)[1], time(xts_trunc)[length(time(xts_trunc))]),
    par.settings = theme.novpadding,
    scales = list(y = list(rot = 90))
  )


  # simple time serie but with threshold
  xts_ts_thrs_plt <- lattice::xyplot(
    xts_obj, type = "p", cex = .1, pch = 19, ylim = c(0, 5), col = "black",
    xlab = "", ylab = "(mm)", main = "time series (threshold = 5 mm)",
    xlim = c(time(xts_obj)[1], time(xts_obj)[length(time(xts_obj))]),
    par.settings = theme.novpadding,
    scales = list(y = list(rot = 90))
  )


  # small gaps
  xts_small_gaps <- get_ngaps(xts_obj = xts_obj)
  xts_small_gaps <- xts::xts(
    xts_small_gaps[, -match(c("year", "year_size"), colnames(xts_small_gaps))],
    as.Date(paste(xts_small_gaps$year, "-01-01", sep = ""))
  )

  ymax <- max(xts_small_gaps) * 1.05
  ymin <- min(xts_small_gaps) * 1.05

  xts_small_gaps <- lattice::xyplot(
    xts_small_gaps[, 1], ylim=c(ymin,ymax), col = 1,
    xlab = "", ylab = "number of values", main = "small gaps",
    par.settings = theme.novpadding,
    scales = list(y = list(rot = 90)), lwd = 3
  ) +
    latticeExtra::as.layer(
      lattice::xyplot(xts_small_gaps[, 2], col = 2, lwd = 3)
    ) +
    latticeExtra::as.layer(
      lattice::xyplot(xts_small_gaps[, 3], col = 3, lwd = 3)
    ) +
    latticeExtra::as.layer(
      lattice::xyplot(xts_small_gaps[, 4], col = 4, lwd = 3)
    ) +
    latticeExtra::as.layer(
      lattice::xyplot(xts_small_gaps[, 5], col = 5, lwd = 3)
    )

  # weekly cycle

  xts_wd_data <- get_pweek_from_xts(xts_obj = xts_obj)
  xts_wd_data$frac_wd <- xts_wd_data$frac_wd / 100
  xts_wd_plt <- lattice::barchart(
    frac_wd ~ week,
    data = xts_wd_data,
    group = bin_test,
    xlab = " ",
    ylab = "wet day fraction (>= 1 mm) ",
    main = "weekly cycle",
    ylim = c(0, sum(xts_wd_data$count_wd)/sum(xts_wd_data$count) + 0.06),
    auto.key = list(space = "bottom", columns = 2,
                    padding.text = 0, between.columns = 0,
                    text.width = 4, pch = 20, between = .5, cex = .75, size = 1,
                    rectangles = TRUE, points = FALSE),
    scales = list(y = list(rot = 90, tck = c(1, 0))),
    par.settings = list(superpose.polygon = list(col = c("red", "gray50"))),
    panel = function(...) {
      panel.barchart(...)
      args <- list(...)
      lattice::panel.text(
        args$x, args$y + 0.02,
        xts_wd_data$count,
        pos = 4, offset = -1, alpha = .5, cex = 1
      )
      lattice::panel.abline(
        h = sum(xts_wd_data$count_wd)/sum(xts_wd_data$count),
        lty = 2, lwd = 1, col = "black"
      )
    }
  )

  xts_wd_plt <- update(xts_wd_plt, par.settings = theme.novpadding2)


  # decimal frequency time serie

  dec_df_freq <- get_dec_from_xts(xts_obj = xts_obj, removeNAs = FALSE)
  dec_df_freq <- reshape2::melt(table(dec_df_freq))
  dec_df_freq$dec <- factor(
    dec_df_freq$dec,
    levels = c("x.0", "x.1", "x.2",
               "x.3", "x.4", "x.5",
               "x.6", "x.7", "x.8", "x.9")
  )

  rhg_cols <- c("black", "yellow", "orange",
                "red", "darkslateblue", "darkgray",
                "magenta", "blue", "cyan", "darkgreen")

  xts_ts_dec_plt <- lattice::barchart(
   value ~ year, data = dec_df_freq,
   group = dec, stack = TRUE, horizontal = FALSE,
   xlab = " ", ylab = "frequency (days/year)",
   main = "precision and rounding patterns",
   lwd = .1,
   auto.key = list(space = 'bottom', columns = 10, padding.text = 0,
                   between.columns = 0, text.width = 4,
                   pch = 20, between = .5, cex = .75, size = 1, lwd = .1),
    par.settings = list(
        superpose.polygon = list(col = rhg_cols, lwd = .1)),
    scales = list(y = list(rot = 90, tck = c(1, 0))),
    axis = function(side, ...) {
      if (side == "bottom")
        lattice::panel.axis(
                            at = seq(
                              min(seq_along(dec_df_freq$year)),
                              max(seq_along(dec_df_freq$year)),
                              by = 10
                            ),
                            label = seq(
                              min(dec_df_freq$year),
                              max(dec_df_freq$year), by = 10
                            ),
                            outside = TRUE, rot = 0, tck = 0)
      else
        lattice::axis.default(side, ...)
    }
  )

  xts_ts_dec_plt <- update(xts_ts_dec_plt, par.settings = theme.novpadding2)


  print(
    gridExtra::grid.arrange(
      xts_ts_plt, xts_trunc,
      xts_ts_thrs_plt, xts_small_gaps,
      xts_ts_dec_plt, xts_wd_plt,
      ncol = 2,
      top = grid::textGrob(names(xts_obj), gp = grid::gpar(fontsize = 17))
    )
  )

}

# for ploting

get_pweek_from_xts <- function(xts_obj) {

  if (dim(xts_obj)[1] < 1) {

    data.frame(year = NA,
               dec = NA)
  } else {

    wd_fraction(xts_obj)

  }
}

wd_fraction <- function(xts_obj) {

  original_locale <- Sys.getlocale("LC_TIME")  # Save the current locale
  Sys.setlocale("LC_TIME", "C")                # Set locale to English

  out_df <- data.frame(
    value = as.numeric(xts_obj),
    week = weekdays(time(xts_obj))
  )
  out_df_wd <- out_df[out_df$value >= 0.1, ]

  length_values <- aggregate(
   value ~ week,
   data = out_df,
   function(x) length(x[!is.na(x)]),
   na.action = NULL
  )

  if (all(is.na(out_df_wd$value))) {

    lenght_wd <- length_values

  } else {

    lenght_wd <- aggregate(
      value ~ week,
      data = out_df_wd,
      function(x) length(x[!is.na(x)]),
      na.action = NULL
    )

  }

  out_df <- merge(length_values, lenght_wd, by = "week")
  colnames(out_df) <- c("week", "count", "count_wd")
  out_df <- transform(
    out_df,
    week = factor(
      week,
      levels = c("Monday", "Tuesday", "Wednesday",
                 "Thursday", "Friday", "Saturday", "Sunday"),
      labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
      ordered = TRUE
    )

  )
  out_df <- transform(out_df, frac_wd = count_wd/count)

  if (is.nan(sum(out_df$frac_wd)) | is.na(sum(out_df$frac_wd))) {

    out_df$bin_test <- NA
    out_df$bin_test <- factor(
      out_df$bin_test, 
      levels = c("Rejected Ho", "No Rejected Ho")
    )

  } else {

    for (i in 1:nrow(out_df)) {

      test_bt <- binom.test(out_df$count_wd[i], out_df$count[i],
                            p = sum(out_df$count_wd)/sum(out_df$count),
                            alternative = "two.sided",
                            conf.level = 0.95)

      out_df$bin_test[i] <- ifelse(
        !((test_bt$conf.int[1] < sum(out_df$count_wd)/sum(out_df$count)) &
            (sum(out_df$count_wd)/sum(out_df$count) < test_bt$conf.int[2])),
        "Rejected Ho",
        "No Rejected Ho"
      )

    }

    out_df$bin_test <- factor(
      out_df$bin_test,
      levels = c("Rejected Ho", "No Rejected Ho")
    )

  }

  out_df <- out_df[order(out_df$week), ]
  rownames(out_df) <- NULL
  out_df$frac_wd <- round(out_df$frac_wd * 100, 1)

  Sys.setlocale("LC_TIME", original_locale)    # Reset to the original locale
  return(out_df)

}