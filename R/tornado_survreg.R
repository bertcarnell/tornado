# Copyright 2019 Robert Carnell

#' Survreg Tornado Diagram
#'
#' @param model a survreg object
#' @param modeldata the data used to fit the model
#' @param type PercentChange, percentiles, or ranges
#' @param alpha the level of change
#' @param alt.order an alternate order for the plot
#' @param dict a dictionary to translate variables for the plot
#' @param xlabel a label for the x-axis
#' @param ... further arguments
#'
#' @return the plot invisibly
#' @export
#' @method tornado survreg
#' @importFrom scales percent
#' @import ggplot2
#' @import survival
#'
#' @examples
#' gtest <- survival::survreg(survival::Surv(futime, fustat) ~ ecog.ps + rx, survival::ovarian,
#'                            dist='weibull', scale=1)
#' tornado(gtest, survival::ovarian, type = "PercentChange", alpha = 0.10, xlabel = "futime")
tornado.survreg <- function(model, modeldata, type="PercentChange", alpha=0.10,
                        alt.order=NA, dict=NA, xlabel="Response Rate",
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
    geom_bar(position = "identity", stat = "identity") +
    coord_flip() +
    ylab(xlabel) +
    xlab("") +
    scale_fill_manual(values = c("grey", "#69BE28")) +
    theme_bw()

  if (is.data.frame(factordat))
  {
    ggp <- ggp + geom_point(aes_string(x = "variable", y = "value"), data = factordat, fill = "black")
  }

  ggp <- ggp + scale_y_continuous(breaks = pretty_break,
                                  labels = format(pretty_break + pmeans, digits = 4))
  return(ggp)
}
