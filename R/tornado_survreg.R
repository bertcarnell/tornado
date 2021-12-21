# Copyright 2019 Robert Carnell

#' Survreg Tornado Diagram
#'
#' @inherit tornado description
#'
#' @inheritParams tornado
#' @param modeldata the data used to fit the model
#' @param geom_point_control a list of \code{ggplot2::geom_point}
#'
#' @inherit tornado return
#' @export
#' @method tornado survreg
#' @import ggplot2
#' @import survival
#'
#' @examples
#' gtest <- survival::survreg(survival::Surv(futime, fustat) ~ ecog.ps + rx,
#'                            survival::ovarian,
#'                            dist='weibull', scale=1)
#' tornado(gtest, modeldata = survival::ovarian, type = "PercentChange",
#'         alpha = 0.10, xlabel = "futime")
tornado.survreg <- function(model, type="PercentChange", alpha=0.10,
                        alt.order=NA, dict=NA, xlabel="Survival Time",
                        sensitivity_colors=c("grey", "#69BE28"),
                        geom_bar_control=list(width = NULL),
                        geom_point_control=list(fill = "black", col = "black"),
                        modeldata,
                        ...)
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
