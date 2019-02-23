# Copyright 2019 Robert Carnell

#' Quantile for Ordered Factors
#'
#' @param x an ordered factor
#' @param probs the desired quatiles
#' @param ... arugments passed on
#'
#' @return ordered factor levels at the desired quantiles
#' @export
#' @method quantile ordered
#' @importFrom assertthat assert_that
#' @importFrom stats quantile
#'
#' @examples
#' quantile(ordered(rep(c("C","B","A"), each=30), levels=c("C","B","A")),
#'          probs <- seq(0, 1, 0.25))
quantile.ordered <- function(x, probs = seq(0, 1, 0.25), ...)
{
  assertthat::assert_that(is.ordered(x), msg = "This method is meant to be called on ordered factors")
  tt <- table(x)
  ttcum <- cumsum(tt / sum(tt))
  ret <- character(length(probs))
  for (i in seq_along(probs))
  {
    for (j in seq_along(ttcum))
    {
      if (probs[i] <= ttcum[j])
      {
        ret[i] <- names(ttcum)[j]
        break
      }
    }
    assertthat::assert_that(ret[i] != "", msg = "did not find quantile in the data")
  }
  return(ordered(ret, levels = levels(x)))
}
