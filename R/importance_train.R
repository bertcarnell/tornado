# Copyright 2019 Robert Carnell

#' Importance Plot for the caret::train objects
#'
#' @inheritParams importance
#'
#' @import ggplot2
#'
#' @inherit importance return
#' @export
#'
#' @examples
#' if (requireNamespace("glmnet", quietly = TRUE))
#' {
#'   model_final <- caret::train(x = subset(mtcars, select = -mpg), y = mtcars$mpg, method = "rf")
#'   imp <- importance(model_final)
#'   print(imp)
#'   plot(imp)
#' }
importance.train <- function(model_final, ...)
{
  # model_final <- caret::train(x = subset(mtcars, select = -mpg), y = mtcars$mpg, method = "rf")

  vimp <- caret::varImp(model_final, ...)
  # vimp <- caret::varImp(model_final)
  temp_names <- rownames(vimp$importance)[order(vimp$importance$Overall)]
  vimp$importance$names <- factor(rownames(vimp$importance), levels = temp_names)

  g <- ggplot(vimp$importance, aes_string(x = "Overall", y = "names")) +
    geom_bar(stat = "identity") +
    labs(x = "Importance", y = "") +
    theme_bw()

  return(structure(list(plot = g,
                        data = vimp$importance), class = "importance_plot"))
}
