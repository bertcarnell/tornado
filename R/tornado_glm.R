# Copyright 2019 Robert Carnell

#' GLM Tornado Diagram
#'
#' @inherit tornado description
#'
#' @inheritParams tornado
#'
#' @inherit tornado return
#' @export
#' @method tornado glm
#' @importFrom stats family
#'
#' @seealso \code{\link{tornado}}
#'
#' @examples
#' gtest <- glm(mpg ~ cyl*wt*hp, data = mtcars, family = gaussian)
#' torn <- tornado(gtest, type = "PercentChange", alpha = 0.10)
#' plot(torn, xlabel = "MPG")
tornado.glm <- function(model, type="PercentChange", alpha=0.10,
                        dict=NA, ...)
{
  extraArguments <- list(...)
  ret <- .create_plot_data(model = model, modeldata = model$data,
                           type = type, alpha = alpha,
                           dict = dict)

  return(structure(list(data = list(plotdat = ret$plotdat,
                                    pmeans = ret$pmeans,
                                    factordat = ret$factor_plotdat),
                        type = "glm",
                        family = stats::family(model)$family),
                   class = "tornado_plot"))
}
