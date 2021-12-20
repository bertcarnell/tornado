# Copyright 2019 Robert Carnell

#' Linear Model Tornado Diagram
#'
#' @inherit tornado description
#'
#' @inheritParams tornado
#' @param geom_point_control a list of \code{ggplot2::geom_point}
#'
#' @return the plot invisibly
#' @export
#' @method tornado lm
#' @importFrom scales percent
#' @import ggplot2
#'
#' @examples
#' gtest <- lm(mpg ~ cyl*wt*hp, data = mtcars)
#' tornado(gtest, type = "PercentChange", alpha = 0.10, xlabel = "MPG")
tornado.lm <- function(model, type="PercentChange", alpha=0.10,
                       alt.order=NA, dict=NA, xlabel="Response Rate",
                       sensitivity_colors=c("grey", "#69BE28"),
                       geom_bar_control=list(width = NULL),
                       geom_point_control=list(fill = "black", col = "black"),
                       ...)
{
  # model = gtest
  # type = "PercentChange"
  # alpha = 0.10
  # alt.order = NA
  # dict = NA
  # xlabel = "MPG"

  extraArguments <- list(...)
  ret <- .create_plot_data(model = model, modeldata = model$model,
                           type = type, alpha = alpha,
                           alt.order = alt.order, dict = dict)

  plotdat <- ret$plotdat
  pmeans <- ret$pmeans
  factordat <- ret$factor_plotdat

  if (is.data.frame(factordat))
  {
    pretty_break <- pretty(c(plotdat$value, factordat$value), n = 5)
  } else
  {
    pretty_break <- pretty(plotdat$value, n = 5)
  }

  ggp <- ggplot(plotdat, aes_string(x = "variable", y = "value", fill = "Level")) +
    do.call(geom_bar, args = c(list(position = "identity", stat = "identity"), geom_bar_control)) +
    coord_flip() +
    ylab(xlabel) +
    xlab("") +
    scale_fill_manual(values = sensitivity_colors) +
    theme_bw()

  if (is.data.frame(factordat))
  {
    ggp <- ggp + do.call(geom_point, args = c(list(mapping = aes_string(x = "variable", y = "value"),
                                                   data = factordat),
                                              geom_point_control))
  }

  ggp <- ggp + scale_y_continuous(breaks = pretty_break,
                                  labels = format(pretty_break + pmeans, digits = 4))

  return(ggp)
}
