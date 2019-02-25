# Copyright 2019 Roberr Carnell

#' GLM variable importance plot
#'
#' @param model_final a glm object
#' @param model_null a glm object for the null model
#' @param dict a dictionary to translate the model variables to plotting vriables
#' @param ... additional arguments
#'
#' @return a ggplot2 object
#' @export
#'
#' @examples
#' gtest <- glm(mpg ~ cyl*wt*hp + gear + carb, data=mtcars, family=gaussian)
#' gtestreduced <- glm(mpg ~ 1, data=mtcars, family=gaussian)
#' g <- importance(gtest, gtestreduced)
#'
#' gtest <- glm(mpg ~ cyl + wt + hp + gear + carb, data=mtcars, family=gaussian)
#' gtestreduced <- glm(mpg ~ 1, data=mtcars, family=gaussian)
#' g <- importance(gtest, gtestreduced)
#'
#' gtest <- glm(vs ~ wt + disp + gear, data=mtcars, family=binomial(link="logit"))
#' gtestreduced <- glm(vs ~ 1, data=mtcars, family=binomial(link="logit"))
#' g <- importance(gtest, gtestreduced)
importance.glm <- function(model_final, model_null, dict=NA, ...)
{
  # model_final <- glm(mpg ~ cyl*wt*hp + gear + carb, data=mtcars, family=gaussian)
  # model_null <- glm(mpg ~ 1, data=mtcars, family=gaussian)
  # dict <- NA

  extraArguments <- list(...)
  tab_summary <- .create_tab_summary(model_final = model_final,
                                     model_null = model_null,
                                     dict = dict,
                                     isDeviance = TRUE)

  combined <- .create_common_importance_plot(tab_summary, isDeviance = TRUE)

  return(structure(list(plot = combined,
                        data = tab_summary), class = "importance_plot"))
}
