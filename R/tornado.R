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
#' @param type \code{PercentChange}, \code{percentiles}, or \code{ranges}
#' @param alpha the level of change
#' @param dict a dictionary to translate variables for the plot.  The dictionary
#' must be a list or data.frame with elements \code{old} and \code{new}.  The
#' \code{old} element must contain each variable in the model.
#' @param ... further arguments, not used
#'
#' @return a \code{tornado_plot} object
#' \item{type}{the type of tornado plot}
#' \item{data}{the data required for the plot}
#' \item{family}{the model family if available}
#'
#' @seealso \code{\link{tornado.lm}}, \code{\link{tornado.glm}}, \code{\link{tornado.cv.glmnet}}, \code{\link{tornado.survreg}}, \code{\link{tornado.coxph}}, \code{\link{tornado.train}}
#'
#' @export
tornado <- function(model, type, alpha, dict, ...)
{
  UseMethod("tornado", model)
}
