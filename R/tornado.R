# Copyright 2019 Robert Carnell

#' Generic tornado method
#'
#' @param x a model object
#' @param ... other arguments passed to methods for specific classes
#'
#' @return a plot object
#' @export
tornado <- function(x, ...)
{
  UseMethod("tornado", x)
}
