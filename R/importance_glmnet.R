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
#' @seealso \code{\link{importance}}
#'
#' @importFrom stats model.matrix model.frame
#' @importFrom assertthat assert_that
#'
#' @examples
#' if (requireNamespace("glmnet", quietly = TRUE))
#' {
#'   form <- formula(mpg ~ cyl*wt*hp)
#'   mf <- model.frame(form, data = mtcars)
#'   mm <- model.matrix(mf, mf)
#'   gtest <- glmnet::cv.glmnet(x = mm, y = mtcars$mpg, family = "gaussian")
#'   imp <- importance(gtest, mtcars, form, nperm = 50)
#'   plot(imp)
#' }
importance.cv.glmnet <- function(model_final, model_data, form, dict = NA, nperm = 500,
                                 ...)
{
  # form <- formula(mpg ~ cyl*wt*hp)
  # mf <- model.frame(mpg ~ cyl*wt*hp, data = mtcars)
  # mm <- model.matrix(mf, mf)
  # model_final <- glmnet::cv.glmnet(x = mm, y = mtcars$mpg, family = "gaussian")
  # model_data <- mtcars
  # dict <- NA
  # nperm <- 100
  # geom_bar_control <- list(fill = "green")

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

  dat2 <- dat2[order(dat2$value, decreasing = TRUE),]
  dat2$variable <- factor(dat2$variable, levels = rev(dat2$variable))

  return(structure(list(data = dat2,
                        type = "cv.glmnet"),
                   class = "importance_plot"))
}
