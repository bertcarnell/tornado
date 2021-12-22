# Copyright 2019 Robert Carnell

#' Plot an Importance Plot object
#'
#' @param x a \code{importance_plot} object
#' @param ... future arguments
#'
#' @return the plot
#' @export
#'
#' @method plot importance_plot
#'
#' @examples
#' gtest <- lm(mpg ~ cyl + wt + hp + gear + carb, data = mtcars)
#' gtestreduced <- lm(mpg ~ 1, data = mtcars)
#' imp <- importance(gtest, gtestreduced)
#' plot(imp)
plot.importance_plot <- function(x, ...)
{
  if (all(c("gtable", "gTree", "grob", "gDesc") %in% class(x$plot)))
  {
    grid::grid.newpage()
    grid::grid.draw(x$plot)
  } else if (all(c("gg", "ggplot") %in% class(x$plot)))
  {
    plot(x$plot)
  } else
  {
    stop("plot type not recognized in plot.importance_plot")
  }
}
