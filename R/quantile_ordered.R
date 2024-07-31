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
#' @importFrom stats quantile
#'
#' @examples
#' quantile(ordered(rep(c("C","B","A"), each=30), levels=c("C","B","A")),
#'          probs <- seq(0, 1, 0.25))
quantile.ordered <- function(x, probs = seq(0, 1, 0.25), ...)
{
  if (!is.ordered(x)) {
    stop("This method is meant to be called on ordered factors")
  }
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
    if (ret[i] == "") {
      stop("did not find quantile in the data in quantile.ordered")
    }
  }
  return(ordered(ret, levels = levels(x)))
}
