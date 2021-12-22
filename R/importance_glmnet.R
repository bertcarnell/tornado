# Copyright 2019 Robert Carnell

#' Plot Variable Importance for a GLMNET model
#'
#' @inheritParams importance
#' @param model_data the data used to fit the model
#' @param form the model formula
#' @param dict a variable dictionary for plotting
#' @param nperm the number of permutations used to calculate the importance
#'
#' @inherit importance return
#' @export
#'
#' @importFrom stats model.matrix model.frame
#' @importFrom assertthat assert_that
#' @import ggplot2
#'
#' @examples
#' if (requireNamespace("glmnet", quietly = TRUE))
#' {
#'   form <- formula(mpg ~ cyl*wt*hp)
#'   mf <- model.frame(mpg ~ cyl*wt*hp, data = mtcars)
#'   mm <- model.matrix(mf, mf)
#'   gtest <- glmnet::cv.glmnet(x = mm, y = mtcars$mpg, family = "gaussian")
#'   importance(gtest, mtcars, form, nperm = 100)
#' }
importance.cv.glmnet <- function(model_final, model_data, form, dict = NA, nperm = 500, ...)
{
  #form <- formula(mpg ~ cyl*wt*hp)
  #mf <- model.frame(mpg ~ cyl*wt*hp, data = mtcars)
  #mm <- model.matrix(mf, mf)
  #gtest <- glmnet::cv.glmnet(x = mm, y = mtcars$mpg, family = "gaussian")
  # tornado(gtest, mtcars, formula(mpg ~ cyl*wt*hp), s = "lambda.1se",
  #         type = "PercentChange", alpha = 0.10, xlabel = "MPG")
  # tornado.cv.glmnet <- function(model, modeldata, form, s="lambda.1se",
  #                               type="PercentChange", alpha=0.10,
  #                               alt.order=NA, dict=NA, xlabel="Response Rate",
  #                               ...)
  #model_final <- gtest
  #model_data <- mtcars
  #form <- formula(mpg ~ cyl*wt*hp)

  assertthat::assert_that(requireNamespace("glmnet", quietly = TRUE),
                          msg = "The glmnet package is required to use this method")

  otherVariables <- list(...)
  modelframe <- stats::model.frame(form, data = model_data)
  modelmatrix <- stats::model.matrix(form, modelframe)

  vars <- rownames(attr(terms(modelframe), "factors"))[-1]
  resp_var <- rownames(attr(terms(modelframe), "factors"))[1]
  n <- nrow(model_data)
  baseMeasure <- mean((predict(model_final, newx = modelmatrix) - model_data[,resp_var])^2)

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

      modelframe_new <- stats::model.frame(form, data = model_data_new)
      modelmatrix_new <- stats::model.matrix(form, modelframe_new)
      model_new <- glmnet::glmnet(x = modelmatrix_new, y = model_data[,resp_var],
                                  family = model_final$glmnet.fit$call$family)

      temp[i] <- mean((predict(model_new, newx = modelmatrix_new) - model_data[,resp_var])^2)
    }
    importances[j] <- mean(temp)
  }
  importances_final <- pmax(0, importances - baseMeasure) / baseMeasure

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
    ylab("Percent Increase in MSE when Variable is permuted (Importance)")

  return(ggp)
}
