# Copyright 2019 Roberr Carnell

#' Linear Model variable importance plot
#'
#' @inheritParams importance
#' @param model_null a \code{lm} object for the null model
#' @param dict a dictionary to translate the model variables to plotting variables
#'
#' @inherit importance return
#' @export
#'
#' @seealso \code{\link{importance}}
#'
#' @examples
#' gtest <- lm(mpg ~ cyl*wt*hp + gear + carb, data=mtcars)
#' gtestreduced <- lm(mpg ~ 1, data=mtcars)
#' imp <- importance(gtest, gtestreduced)
#' plot(imp)
#'
#' gtest <- lm(mpg ~ cyl + wt + hp + gear + carb, data=mtcars)
#' gtestreduced <- lm(mpg ~ 1, data=mtcars)
#' imp <- importance(gtest, gtestreduced)
#' plot(imp)
importance.lm <- function(model_final, model_null, dict=NA,
                          ...)
{
  # model_final <- lm(mpg ~ cyl*wt*hp + gear + carb, data=mtcars)
  # model_null <- lm(mpg ~ 1, data=mtcars)
  # dict <- NA

  extraArguments <- list(...)
  tab_summary <- .create_tab_summary(model_final = model_final,
                                     model_null = model_null,
                                     dict = dict,
                                     isDeviance = FALSE)

  dat2 <- .create_common_importance_data(tab_summary)

  return(structure(list(data = dat2,
                        type = "lm"),
                   class = "importance_plot"))
}
