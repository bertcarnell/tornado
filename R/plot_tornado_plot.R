# Copyright 2019 Robert Carnell

#' Plot a Tornado Plot object
#'
#' @param x a \code{tornado_plot} object
#' @param plot boolean to determine if the plot is displayed, or just returned
#' @param nvar the number of variables to plot
#' @param xlabel a label for the x-axis
#' @param sensitivity_colors a two element character vector of the bar colors for a
#' lower value and upper value
#' @param geom_bar_control a list of \code{ggplot2::geom_bar} options
#' @param geom_point_control a list of \code{ggplot2::geom_point}
#' @param ... future arguments
#'
#' @method plot tornado_plot
#'
#' @importFrom scales percent
#' @import ggplot2
#' @importFrom rlang .data
#'
#' @return the plot
#' @export
#'
#' @examples
#' gtest <- lm(mpg ~ cyl*wt*hp, data = mtcars)
#' tp <- tornado(gtest, type = "PercentChange", alpha = 0.10, xlabel = "MPG")
#' plot(tp)
plot.tornado_plot <- function(x, plot=TRUE, nvar=NA, xlabel="Model Response",
                              sensitivity_colors=c("grey", "#69BE28"),
                              geom_bar_control=list(width = NULL),
                              geom_point_control=list(fill = "black", col = "black"),
                              ...)
{
  if (!(is.list(geom_bar_control) & is.list(geom_point_control))) {
    stop("The geom_bar_control and geom_point_control parameters must be a list")
  }

  # if geom_bar_control contains fill, then delete it and warn
  ind <- which(names(geom_bar_control) == "fill")
  if (length(ind) >= 1)
  {
    geom_bar_control <- geom_bar_control[-ind]
    warning("geom_bar_control fill argument is set by the sensitivity_colors argument")
  }

  # if geom_point_control does not include fill, then add it.  Fill has to be there to work with
  #   the bar plot
  if (!("fill" %in% names(geom_point_control)))
  {
    geom_point_control$fill <- "black"
    warning("geom_point_control fill argument was added")
  }

  is_percentage <- FALSE
  if (is.data.frame(x$data$factordat))
  {
    pretty_break <- pretty(c(x$data$plotdat$value + c(x$data$pmeans), x$data$factordat$value + c(x$data$pmeans)), n = 5) - c(x$data$pmeans)
    if (all(c(x$data$plotdat$value + c(x$data$pmeans), x$data$factordat$value + c(x$data$pmeans)) >= 0) &&
        all(c(x$data$plotdat$value + c(x$data$pmeans), x$data$factordat$value + c(x$data$pmeans)) <= 1)) {
      is_percentage <- TRUE
    }
  } else
  {
    pretty_break <- pretty(x$data$plotdat$value + c(x$data$pmeans), n = 5) - c(x$data$pmeans)
    if (all(c(x$data$plotdat$value + c(x$data$pmeans)) >= 0) &&
        all(c(x$data$plotdat$value + c(x$data$pmeans)) <= 1)) {
      is_percentage <- TRUE
    }
  }

  ggp <- ggplot(x$data$plotdat, aes(x = .data$variable, y = .data$value, fill = .data$Level)) +
    do.call(geom_bar, args = c(list(position = "identity", stat = "identity"), geom_bar_control)) +
    coord_flip() +
    ylab(xlabel) +
    xlab("") +
    scale_fill_manual(values = sensitivity_colors) +
    theme_bw() +
    geom_hline(yintercept = 0, lty = 2)

  if (is.data.frame(x$data$factordat))
  {
    ggp <- ggp + do.call(geom_point, args = c(list(mapping = aes(x = .data$variable, y = .data$value),
                                                   data = x$data$factordat),
                                              geom_point_control))
  }

  if ((x$type %in% c("cv.glmnet", "glm") & x$family %in% c("binomial", "quasibinomial")) | is_percentage)
  {
    ggp <- ggp +
      scale_y_continuous(breaks = pretty_break,
                         labels = scales::percent(c(pretty_break) + c(x$data$pmeans)))#,
                         #limits = c(max(min(pretty_break), -x$data$pmeans),
                         #            min(max(pretty_break), 1 - x$data$pmeans)))
  } else
  {
    ggp <- ggp + scale_y_continuous(breaks = pretty_break,
                                    labels = format(pretty_break + c(x$data$pmeans), digits = 4))

  }
  if (plot) plot(ggp) else return(ggp)
}
