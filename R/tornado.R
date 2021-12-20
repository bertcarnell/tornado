# Copyright 2019 Robert Carnell

#' Generic tornado plotting method
#'
#' @description A tornado plot is a visualization of the range of outputs expected
#' from a variety of inputs, or alternatively, the sensitivity of the output to the
#' range of inputs.  The center of the tornado is plotted at the response
#' expected from the mean of each input variable.  For a given variable, the width
#' of the tornado is determined by the range of the variable, a multiplicative factor
#' of the variable, or a quantile of the variable.  Variables are ordered vertically
#' with the widest bar at the top and narrowest at the bottom.  Only one variable
#' is moved from its mean value at a time.  Factors or categorical variables have also
#' been added to these plots by plotting dots at the resulting output as each
#' factor is varied through all of its levels.  The base factor level is chosen
#' as the input variable for the center of the tornado.
#'
#' @param model a model object
#' @param type PercentChange, percentiles, or ranges
#' @param alpha the level of change
#' @param alt.order an alternate order for the plot
#' @param dict a dictionary to translate variables for the plot
#' @param xlabel a label for the x-axis
#' @param sensitivity_colors a two element character vector of the bar colors for a
#' lower value and upper value
#' @param geom_bar_control a list of \code{ggplot2::geom_bar} options
#' @param ... further arugments, not used
#'
#' @return a plot object
#'
#' @seealso \code{\link{tornado.lm}} \code{\link{tornado.glm}} \code{\link{tornado.glmnet}} \code{\link{tornado.survreg}} \code{\link{tornado.train}}
#'
#' @export
tornado <- function(model, type, alpha, alt.order, dict, xlabel, sensitivity_colors,
                    geom_bar_control, ...)
{
  UseMethod("tornado", model)
}
