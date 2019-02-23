# Copyright 2019 Robert Carnell

#' GLM Tornado Diagram
#'
#' @param model a glm object
#' @param type PercentChange, percentiles, or ranges
#' @param alpha the level of change
#' @param alt.order an alternate order for the plot
#' @param dict a dictionary to translate variables for the plot
#' @param xlabel a label for the x-axis
#'
#' @return the plot invisibly
#' @export
#' @method tornado glm
#' @importFrom scales percent
#' @import ggplot2
#'
#' @examples
#' gtest <- glm(mpg ~ cyl*wt*hp, data = mtcars, family = gaussian)
#' tornado(gtest, type = "PercentChange", alpha = 0.10, xlabel = "MPG")
tornado.glm <- function(model, type="PercentChange", alpha=0.10,
                        alt.order=NA, dict=NA, xlabel="Response Rate")
{
  ret <- .create_plot_data(model = model, modeldata = model$data,
                           type = type, alpha = alpha,
                           alt.order = alt.order, dict = dict)
  plotdat <- ret$plotdat
  pmeans <- ret$pmeans

  pretty_break <- pretty(plotdat$value, n = 5)

  ggp <- ggplot(plotdat, aes(x = variable, y = value, fill = Level)) +
    geom_bar(position = "identity", stat = "identity") +
    coord_flip() +
    ylab(xlabel) +
    xlab("") +
    scale_fill_manual(values = c("grey", "#69BE28")) +
    theme_bw()

  if (family(model)$family %in% c("binomial", "quasibinomial"))
  {
    ggp <- ggp +
      scale_y_continuous(breaks = pretty_break,
                         labels = scales::percent(pretty_break + pmeans),
                         limits = c(max(min(pretty_break),-pmeans), min(max(pretty_break), 1 - pmeans)))
  } else
  {
    ggp <- ggp + scale_y_continuous(breaks = pretty_break,
                                    labels = format(pretty_break + pmeans, digits = 4))
  }
  return(ggp)
}
