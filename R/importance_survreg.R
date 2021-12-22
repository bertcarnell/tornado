# Copyright 2019 Robert Carnell

#' Create a variable importance plot for a survreg model
#'
#' @inheritParams importance
#' @param model_data the data used to fit the model
#' @param dict a plotting dictionary for models terms
#' @param nperm the number of permutations used to calculate the importance
#' @param geom_bar_control list of arguments to control the plotting of \code{ggplot2::geom_bar}
#'
#' @inherit importance return
#' @export
#'
#' @import survival
#' @import ggplot2
#'
#' @examples
#' model_final <- survival::survreg(survival::Surv(futime, fustat) ~ ecog.ps*rx + age,
#'                        data = survival::ovarian,
#'                        dist = "weibull")
#' importance(model_final, model_data = survival::ovarian, nperm = 500)
importance.survreg <- function(model_final, model_data, dict=NA, nperm = 500,
                               geom_bar_control = list(fill = "#69BE28"), ...)
{
  #model_final <- survreg(Surv(futime, fustat) ~ ecog.ps*rx + age, ovarian, dist="weibull")
  #model_data <- ovarian
  #nperm <- 500

  otherVariables <- list(...)
  vars <- rownames(attr(formula(model_final), "factors"))[-1]
  n <- nrow(model_data)
  baseLikelihood <- model_final$loglik[2]

  # randomly permute each variable
  importances <- numeric(length(vars))
  for (j in seq_along(vars))
  {
    v <- vars[j]
    model_data_new <- model_data
    temp <- numeric(nperm)
    for (i in 1:nperm)
    {
      model_data_new[, v] <- model_data_new[sample(1:n, n, replace = FALSE), v]
      model_new <- survival::survreg(formula(model_final), model_data_new,
                          dist = model_final$dist)
      temp[i] <- model_new$loglik[2]
    }
    importances[j] <- mean(temp)
  }
  importances_final <- 1 - exp(pmin(0, importances - baseLikelihood))

  dat2 <- data.frame(variable = vars,
                     value = importances_final)
  #69BE28 = light green
  #427730 = dark green

  ggp <- ggplot(dat2, aes_string(x = "variable", y = "value")) +
    geom_bar(stat = "identity", fill = "#69BE28") +
    coord_flip() +
    theme_bw() +
    xlab("") +
    scale_y_continuous(labels = scales::percent) +
    ylab("Change in Likelihood Ratio when Variable is permuted (Importance)")

  return(structure(list(plot = ggp, data = dat2), class = "importance_plot"))
}
