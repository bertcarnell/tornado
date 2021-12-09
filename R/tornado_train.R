# Copyright 2021 Robert Carnell

#' Caret Tornado Diagram
#'
#' @param model a \code{caret} object
#' @param type PercentChange, percentiles, or ranges
#' @param alpha the level of change
#' @param alt.order an alternate order for the plot
#' @param dict a dictionary to translate variables for the plot
#' @param xlabel a label for the x-axis
#' @param ... further arugments
#'
#' @return the plot
#' @export
#' @method tornado train
#' @import ggplot2
#' @importFrom stats model.frame terms model.matrix predict
#' @importFrom assertthat assert_that
#'
#' @examples
#' if (requireNamespace("caret", quietly = TRUE) &
#'     requireNamespace("randomForest", quietly = TRUE))
#' {
#'   gtest <- caret::train(x = subset(mtcars, select = -mpg), y = mtcars$mpg, method = "rf")
#'   tornado(gtest, type = "PercentChange", alpha = 0.10, xlabel = "MPG")
#' }
tornado.train <- function(model,
                           type="PercentChange", alpha=0.10,
                           alt.order=NA, dict=NA, xlabel="Response Rate",
                           ...)
{
  # model <- caret::train(x = subset(mtcars, select = -mpg), y = mtcars$mpg, method = "rf")
  # type <- "PercentChange"
  # alpha <- 0.10
  # dict <- NA
  # xlabel <- "MPG"
  # alt.order <- NA

  # mydat <- mtcars
  # mydat$cyl <- factor(mydat$cyl)
  # mydat$vs <- factor(mydat$vs)
  # model <- caret::train(x = subset(mydat, select = -mpg), y = mydat$mpg, method = "rf")
  # type <- "PercentChange"
  # alpha <- 0.10
  # dict <- NA
  # xlabel <- "MPG"
  # alt.order <- NA

  assertthat::assert_that(requireNamespace("caret", quietly = TRUE),
                          msg = "The caret package is required to use this method")

  extraArguments <- list(...)
  assertthat::assert_that(type %in% c("PercentChange","percentiles","ranges"),
                          msg = "type must be PercentChagne, percentiles, or ranges")

  assertthat::assert_that(model$modelType == "Regression",
                          msg = "Only Regression predictions are currently implemented")

  used_variables <- names(model$trainingData)[!grepl("[.]outcome", names(model$trainingData))]

  dict <- .create_dict(dict, used_variables)

  training_data <- subset(model$trainingData, select = used_variables)
  means <- .create_means(training_data)
  names_means <- names(means)
  lmeans <- length(means)

  # factors are held at their base value at this step
  ret <- .create_endpoints(training_data, means, type, alpha)
  endpoints <- ret$endpoints
  Level <- ret$Level
  base_Level <- c("A","B")

  # predict the mean response (type = "raw" by default)
  pmeans <- predict(model, newdata = means)

  # predict on the range of possibilities (type = "raw" by default)
  dat <- .create_data_low_high(means, endpoints)
  pdat <- predict(model, newdata = dat)

  if (is.na(alt.order))
  {
    bar_width <- abs(apply(matrix(c(pdat), nrow = 2, byrow = TRUE), 2, diff))
    alt.order <- order(bar_width, decreasing = FALSE)
  } else {
    assertthat::assert_that(length(alt.order) == lmeans)
  }

  plotdat <- data.frame(variable = rep(dict$Description.for.Presentation[match(names_means, dict$Orig.Node.Name)], times = 2),
                        value = c(pdat) - c(pmeans),
                        Level = rep(base_Level, each = lmeans),
                        stringsAsFactors = FALSE)
  plotdat$variable <- factor(plotdat$variable,
                             levels = dict$Description.for.Presentation[match(names_means, dict$Orig.Node.Name)][alt.order],
                             ordered = FALSE)
  plotdat$Level <- factor(plotdat$Level, levels = base_Level, ordered = FALSE,
                          labels = Level)

  factor_plotdat <- .create_factor_plot_data(training_data, means, pmeans, model, "raw")

  if (is.data.frame(factor_plotdat))
  {
    pretty_break <- pretty(c(plotdat$value, factor_plotdat$value), n = 5)
  } else
  {
    pretty_break <- pretty(plotdat$value, n = 5)
  }

  ggp <- ggplot(plotdat, aes_string(x = "variable", y = "value", fill = "Level")) +
    geom_bar(position = "identity", stat = "identity") +
    coord_flip() +
    ylab(xlabel) +
    xlab("") +
    scale_fill_manual(values = c("grey", "#69BE28")) +
    theme_bw()

  if (is.data.frame(factor_plotdat))
  {
    ggp <- ggp + geom_point(aes_string(x = "variable", y = "value"), data = factor_plotdat, fill = "black")
  }

  ggp <- ggp + scale_y_continuous(breaks = pretty_break,
                                  labels = format(c(pretty_break) + c(pmeans), digits = 4))
  return(ggp)
}
