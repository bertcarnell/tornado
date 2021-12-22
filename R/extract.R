# Copyright 2021 Robert Carnell

#' Generic for extracting a plot or data
#'
#' @rdname extract
#'
#' @param x an object of type \code{importance_plot} or \code{tornado_plot}
#' @param ... arguments passed to other methods
#'
#' @return a plot object or data object
#' @export
extractPlot <- function(x, ...)
{
  UseMethod("extractPlot", x)
}

#' @rdname extract
#' @export
extractPlot.importance_plot <- function(x, ...)
{
  return(x$plot)
}

#' @rdname extract
#' @export
extractPlot.tornado_plot <- function(x, ...)
{
  return(x$data)
}

#' @rdname extract
#' @export
extractData <- function(x, ...)
{
  UseMethod("extractData", x)
}

#' @rdname extract
#' @export
extractData.importance_plot <- function(x, ...)
{
  return(x$data)
}

#' @rdname extract
#' @export
extractData.tornado_plot <- function(x, ...)
{
  return(x$data)
}
