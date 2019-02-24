# Copyright 2019 Robert Carnell

#' Generic Importance Plot
#'
#' @param model_final a model object
#' @param ... arguments passed to other methods
#'
#' @return an object of type importance_plot
#' @export
importance <- function(model_final, ...)
{
  UseMethod("importance", model_final)
}
