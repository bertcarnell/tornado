# Copyright 2019 Roberr Carnell

#' GLM variable importance plot
#'
#' @inheritParams importance
#' @param model_null a glm object for the null model
#' @param dict a dictionary to translate the model variables to plotting variables
#'
#' @inherit importance return
#' @export
#'
#' @seealso \code{\link{importance}}
#'
#' @examples
#' gtest <- glm(mpg ~ cyl*wt*hp + gear + carb, data=mtcars, family=gaussian)
#' gtestreduced <- glm(mpg ~ 1, data=mtcars, family=gaussian)
#' imp <- importance(gtest, gtestreduced)
#' plot(imp)
#'
#' gtest <- glm(mpg ~ cyl + wt + hp + gear + carb, data=mtcars, family=gaussian)
#' gtestreduced <- glm(mpg ~ 1, data=mtcars, family=gaussian)
#' imp <- importance(gtest, gtestreduced)
#' plot(imp)
#'
#' gtest <- glm(vs ~ wt + disp + gear, data=mtcars, family=binomial(link="logit"))
#' gtestreduced <- glm(vs ~ 1, data=mtcars, family=binomial(link="logit"))
#' imp <- importance(gtest, gtestreduced)
#' plot(imp)
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

  dat2 <- .create_common_importance_data(tab_summary)

  return(structure(list(data = dat2,
                        type = "glm"),
                   class = "importance_plot"))
}
