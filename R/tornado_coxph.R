# Copyright 2019 Robert Carnell

#' Cox Proportional Hazards Tornado Diagram
#'
#' @inherit tornado description
#'
#' @inheritParams tornado
#' @param modeldata the data used to fit the model
#'
#' @inherit tornado return
#'
#' @export
#' @method tornado coxph
#'
#' @import survival
#'
#' @examples
#' gtest <- survival::coxph(survival::Surv(stop, event) ~ rx + size + number,
#'                            survival::bladder)
#' torn <- tornado(gtest, modeldata = survival::bladder, type = "PercentChange",
#'              alpha = 0.10)
#' plot(torn, xlabel = "Risk")
tornado.coxph <- function(model, type="PercentChange", alpha=0.10,
                        dict=NA, modeldata, ...)
{
  # model <- survival::coxph(survival::Surv(stop, event) ~ rx + size + number, survival::bladder)
  # modeldata <- survival::bladder
  # type <- "PercentChange"
  # alpha <- 0.10
  # alt.order <- NA
  # dict <- NA
  # xlabel = "Survival Time"
  # geom_bar_control=list(width = NULL)
  # geom_point_control=list(fill = "black", col = "black")
  # sensitivity_colors=c("grey", "#69BE28")

  extraArguments <- list(...)
  ret <- .create_plot_data(model = model, modeldata = modeldata,
                           type = type, alpha = alpha,
                           alt.order = NA, dict = dict,
                           predict_type = "risk")

  return(structure(list(data = list(plotdat = ret$plotdat,
                                    pmeans = ret$pmeans,
                                    factordat = ret$factor_plotdat),
                        type = "coxph",
                        family = NA),
                   class = "tornado_plot"))
}
