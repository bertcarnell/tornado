# Copyright 2019 Robert Carnell

#' Survreg Tornado Diagram
#'
#' @inherit tornado description
#'
#' @inheritParams tornado
#' @param modeldata the data used to fit the model
#'
#' @inherit tornado return
#' @export
#' @method tornado survreg
#' @import survival
#'
#' @seealso \code{\link{tornado}}
#'
#' @examples
#' gtest <- survival::survreg(survival::Surv(futime, fustat) ~ ecog.ps + rx,
#'                            survival::ovarian,
#'                            dist='weibull', scale=1)
#' torn <- tornado(gtest, modeldata = survival::ovarian, type = "PercentChange",
#'              alpha = 0.10, xlabel = "futime")
#' plot(torn, xlabel = "Survival Time")
tornado.survreg <- function(model, type="PercentChange", alpha=0.10,
                        dict=NA, modeldata, ...)
{
  # mydat <- survival::ovarian
  # mydat$resid.ds <- factor(mydat$resid.ds)
  # model <- survival::survreg(survival::Surv(futime, fustat) ~ ecog.ps + rx + resid.ds, mydat, dist = 'weibull', scale = 1)
  # modeldata <- mydat
  # type <- "PercentChange"
  # alpha <- 0.10
  # alt.order <- NA
  # dict <- NA
  # xlabel = "Survival Time"

  extraArguments <- list(...)
  ret <- .create_plot_data(model = model, modeldata = modeldata,
                           type = type, alpha = alpha,
                           alt.order = NA, dict = dict)

  return(structure(list(data = list(plotdat = ret$plotdat,
                                    pmeans = ret$pmeans,
                                    factordat = ret$factor_plotdat),
                        type = "survreg",
                        family = NA),
                   class = "tornado_plot"))
}
