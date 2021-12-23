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
#' @seealso \code{\link{importance}}
#'
#' @examples
#' if (requireNamespace("caret", quietly = TRUE) &
#'     requireNamespace("randomForest", quietly = TRUE))
#' {
#'   model_final <- caret::train(x = subset(mtcars, select = -mpg), y = mtcars$mpg, method = "rf")
#'   imp <- importance(model_final)
#'   plot(imp)
#' }
importance.train <- function(model_final, ...)
{
  # model_final <- caret::train(x = subset(mtcars, select = -mpg), y = mtcars$mpg, method = "rf")

  vimp <- caret::varImp(model_final, ...)
  dat2 <- vimp$importance
  dat2$names <- rownames(dat2)

  dat2 <- dat2[order(dat2$Overall, decreasing = TRUE),]
  dat2$names <- factor(dat2$names, levels = rev(dat2$names))

  return(structure(list(data = dat2,
                        type = "train"),
                   class = "importance_plot"))
}
