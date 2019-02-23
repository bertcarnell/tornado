# Copyright 2019 Robert Carnell

#' Generic tornado method
#'
#' @param model a model object
#' @param ... further arguments to tornado
#'
#' @return a plot object
#' @export
tornado <- function(model, ...)
{
  UseMethod("tornado", model)
}
