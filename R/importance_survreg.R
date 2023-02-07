# Copyright 2019 Robert Carnell

#' Create a variable importance plot for a survreg model
#'
#' @inheritParams importance
#' @param model_data the data used to fit the model
#' @param dict a plotting dictionary for models terms
#' @param nperm the number of permutations used to calculate the importance
#'
#' @inherit importance return
#' @export
#'
#' @seealso \code{\link{importance}}
#'
#' @import survival
#' @import ggplot2
#'
#' @examples
#' model_final <- survival::survreg(survival::Surv(futime, fustat) ~ ecog.ps*rx + age,
#'                        data = survival::ovarian,
#'                        dist = "weibull")
#' imp <- importance(model_final, survival::ovarian, nperm = 500)
#' plot(imp)
importance.survreg <- function(model_final, model_data, dict=NA, nperm = 500, ...)
{
  # model_final <- survival::survreg(survival::Surv(futime, fustat) ~ ecog.ps*rx + age, survival::ovarian, dist="weibull")
  # model_data <- survival::ovarian
  # nperm <- 100
  # dict = NA
  #
  # w <- sample(1:7, size = nrow(survival::ovarian), replace = TRUE)
  # model_final <- survival::survreg(survival::Surv(futime, fustat) ~ ecog.ps*rx + age, survival::ovarian, dist = "weibull", weights = w)

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
      if (is.null(model_final$weights))
      {
        model_new <- survival::survreg(formula(model_final), model_data_new,
                                       dist = model_final$dist)
      } else
      {
        model_new <- survival::survreg(formula(model_final), model_data_new,
                                       dist = model_final$dist,
                                       weights = model_final$weights)
      }
      temp[i] <- model_new$loglik[2]
    }
    importances[j] <- mean(temp)
  }
  importances_final <- 1 - exp(pmin(0, importances - baseLikelihood))

  dat2 <- data.frame(variable = vars,
                     value = importances_final)
  dat2 <- dat2[order(dat2$value, decreasing = TRUE),]
  dat2$variable <- factor(dat2$variable, levels = rev(dat2$variable))

  return(structure(list(data = dat2,
                        type = "survreg"),
                   class = "importance_plot"))
}
