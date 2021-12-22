# Copyright 2021 Robert Carnell

#' print data in a \code{tornado_plot}
#'
#' @param x the object to be printed
#' @param ... further arguments passed to \code{print.data.frame}
#'
#' @export
#'
#' @examples
#' gtest <- lm(mpg ~ cyl*wt*hp, data = mtcars)
#' tp <- tornado(gtest, type = "PercentChange", alpha = 0.10, xlabel = "MPG")
#' print(tp)
print.tornado_plot <- function(x, ...)
{
  print(x$data)
}

