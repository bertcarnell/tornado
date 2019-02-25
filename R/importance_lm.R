# Copyright 2019 Roberr Carnell

#' LM variable importance plot
#'
#' @param model_final a lm object
#' @param model_null a lm object for the null model
#' @param dict a dictionary to translate the model variables to plotting vriables
#' @param ... additional arguments
#'
#' @return a ggplot2 object
#' @export
#'
#' @examples
#' gtest <- lm(mpg ~ cyl*wt*hp + gear + carb, data=mtcars)
#' gtestreduced <- lm(mpg ~ 1, data=mtcars)
#' g <- importance(gtest, gtestreduced)
#'
#' gtest <- lm(mpg ~ cyl + wt + hp + gear + carb, data=mtcars)
#' gtestreduced <- lm(mpg ~ 1, data=mtcars)
#' g <- importance(gtest, gtestreduced)
importance.lm <- function(model_final, model_null, dict=NA, ...)
{
  # model_final <- lm(mpg ~ cyl*wt*hp + gear + carb, data=mtcars)
  # model_null <- lm(mpg ~ 1, data=mtcars)
  # dict <- NA

  extraArguments <- list(...)
  tab_summary <- .create_tab_summary(model_final = model_final,
                                     model_null = model_null,
                                     dict = dict,
                                     isDeviance = FALSE)

  combined <- .create_common_importance_plot(tab_summary, isDeviance = FALSE)

  return(structure(list(plot = combined,
                        data = tab_summary), class = "importance_plot"))
}
