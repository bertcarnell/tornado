# Copyright 2021 Robert Carnell

#' print data in an \code{importance_plot}
#'
#' @param x the object to be printed
#' @param ... further arguments passed to \code{print.data.frame}
#'
#' @export
#'
#' @examples
#' gtest <- glm(vs ~ wt + disp + gear, data=mtcars, family=binomial(link="logit"))
#' gtestreduced <- glm(vs ~ 1, data=mtcars, family=binomial(link="logit"))
#' g <- importance(gtest, gtestreduced)
#' print(g)
print.importance_plot <- function(x, ...)
{
  print(x$data)
}

