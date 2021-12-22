# Copyright 2019 Robert Carnell

#' Plot a Tornado Plot object
#'
#' @param x a \code{tornado_plot} object
#' @param ... future arguments
#'
#' @method plot tornado_plot
#'
#' @return the plot
#' @export
#'
#' @examples
#' gtest <- lm(mpg ~ cyl*wt*hp, data = mtcars)
#' tp <- tornado(gtest, type = "PercentChange", alpha = 0.10, xlabel = "MPG")
#' plot(tp)
plot.tornado_plot <- function(x, ...)
{
  assertthat::assert_that("ggplot" %in% class(x$plot))
  plot(x$plot)
}
