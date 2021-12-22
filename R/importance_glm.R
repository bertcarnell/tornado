# Copyright 2019 Roberr Carnell

#' GLM variable importance plot
#'
#' @inheritParams importance
#' @param model_null a glm object for the null model
#' @param dict a dictionary to translate the model variables to plotting variables
#' @param col_imp_alone the color used for the deviance explained by each variable
#' alone
#' @param col_imp_cumulative the color used for the cumulative deviance explained
#'
#' @inherit importance return
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
importance.glm <- function(model_final, model_null, dict=NA,
                           col_imp_alone = "#69BE28",
                           col_imp_cumulative = "#427730", ...)
{
  # model_final <- glm(mpg ~ cyl*wt*hp + gear + carb, data=mtcars, family=gaussian)
  # model_null <- glm(mpg ~ 1, data=mtcars, family=gaussian)
  # dict <- NA

  extraArguments <- list(...)
  tab_summary <- .create_tab_summary(model_final = model_final,
                                     model_null = model_null,
                                     dict = dict,
                                     isDeviance = TRUE)

  combined <- .create_common_importance_plot(tab_summary, isDeviance = TRUE,
                                             col_imp_alone = col_imp_alone,
                                             col_imp_cumulative = col_imp_cumulative)

  return(structure(list(plot = combined,
                        data = tab_summary), class = "importance_plot"))
}
