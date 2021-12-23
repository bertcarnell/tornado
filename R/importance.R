# Copyright 2019 Robert Carnell

#' Generic Importance Plot
#'
#' @param model_final a model object
#' @param ... arguments passed to other methods
#'
#' @return an object of type \code{importance_plot}
#' \item{type}{the type of importance plot}
#' \item{data}{the importance data required for the plot}
#' @export
#'
#' @seealso \code{\link{importance.glm}} \code{\link{importance.lm}} \code{\link{importance.cv.glmnet}} \code{\link{importance.survreg}}
importance <- function(model_final, ...)
{
  UseMethod("importance", model_final)
}
