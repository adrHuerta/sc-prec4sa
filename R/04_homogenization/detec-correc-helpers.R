
compute_quantiles <- function(ts,
                              length.out = 100) {

  out <- quantile(ts,
                  seq(0, 1, length.out = length.out),
                  type = 8)
  return(out)
}

pp2transf <- function(ppvalues) {

  values2 <- ppvalues ^ (1 / 3)
  values3 <- log(values2 + 0.01)

  return(values3)
}

transf2pp <- function(transfvalues) {

  values2 <- exp(transfvalues) - 0.01
  values3 <- values2 ^ (3)
  values3 <- round(values3, 1)

  return(values3)
}