# Copyright 2019 Robert Carnell

#' Plot an Importance Plot object
#'
#' @param x an importance_plot object
#' @param ... future arguments
#'
#' @return an importance plot
#' @export
#'
#' @examples
#' gtest <- glm(mpg ~ cyl*wt*hp + gear + carb, data=mtcars, family=gaussian)
#' gtestreduced <- glm(mpg ~ 1, data=mtcars, family=gaussian)
#' g <- importance(gtest, gtestreduced)
#' plot(g)
plot.importance_plot <- function(x, ...)
{
  grid::grid.newpage()
  grid::grid.draw(x$plot)
}
