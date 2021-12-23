# Copyright 2019 Robert Carnell

#' Linear Model Tornado Diagram
#'
#' @inherit tornado description
#'
#' @inheritParams tornado
#'
#' @inherit tornado return
#' @export
#' @method tornado lm
#' @importFrom stats family
#'
#' @examples
#' gtest <- lm(mpg ~ cyl*wt*hp, data = mtcars)
#' torn <- tornado(gtest, type = "PercentChange", alpha = 0.10)
#' plot(torn, xlabel = "MPG")
tornado.lm <- function(model, type="PercentChange", alpha=0.10,
                       dict=NA, ...)
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
                           alt.order = NA, dict = dict)

  return(structure(list(data = list(plotdat = ret$plotdat,
                                    pmeans = ret$pmeans,
                                    factordat = ret$factor_plotdat),
                        type = "lm",
                        family = stats::family(model)$family),
                   class = "tornado_plot"))
}
