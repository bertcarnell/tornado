# Copyright 2021 Robert Carnell

#' Caret Tornado Diagram
#'
#' @inherit tornado description
#'
#' @inheritParams tornado
#' @param class_number for classification models, which number of the class that
#' will be plotted
#'
#' @inherit tornado return
#'
#' @export
#'
#' @method tornado train
#'
#' @importFrom assertthat assert_that
#'
#' @seealso \code{\link{tornado}}
#'
#' @examples
#' if (requireNamespace("caret", quietly = TRUE) &
#'     requireNamespace("randomForest", quietly = TRUE))
#' {
#'   gtest <- caret::train(x = subset(mtcars, select = -mpg), y = mtcars$mpg, method = "rf")
#'   torn <- tornado(gtest, type = "PercentChange", alpha = 0.10)
#'   plot(torn, xlabel = "MPG")
#' }
tornado.train <- function(model,
                          type="PercentChange", alpha=0.10,
                          dict=NA, class_number=NA, ...)
{
  ### Regression
  # model <- caret::train(x = subset(mtcars, select = -mpg), y = mtcars$mpg, method = "rf")
  # type <- "PercentChange"
  # alpha <- 0.10
  # dict <- NA
  # xlabel <- "MPG"
  # class_number <- NA
  # geom_bar_control=list(width = NULL)
  # geom_point_control=list(fill = "black")

  # mydat <- mtcars
  # mydat$cyl <- factor(mydat$cyl)
  # mydat$vs <- factor(mydat$vs)
  # model <- caret::train(x = subset(mydat, select = -mpg), y = mydat$mpg, method = "rf")
  # type <- "PercentChange"
  # alpha <- 0.10
  # dict <- NA
  # xlabel <- "MPG"
  # class_number <- NA
  # geom_bar_control=list(width = NULL)
  # geom_point_control=list(fill = "black")

  ### Classification
  # mydat <- mtcars
  # mydat$cyl <- factor(mydat$cyl)
  # mydat$vs <- factor(mydat$vs)
  # model <- caret::train(x = subset(mydat, select = -vs), y = mydat$vs, method = "rf")
  # type <- "PercentChange"
  # alpha <- 0.10
  # dict <- NA
  # xlabel <- "Probability of Class 1"
  # class_number <- 1
  # geom_bar_control=list(width = 0.1)
  # geom_point_control=list(fill = "orange")

  assertthat::assert_that(requireNamespace("caret", quietly = TRUE),
                          msg = "The caret package is required to use this method")

  extraArguments <- list(...) # not used

  assertthat::assert_that(type %in% c("PercentChange","percentiles","ranges"),
                          msg = "type must be PercentChagne, percentiles, or ranges")

  if (model$modelType == "Regression")
  {
    predict_type <- "raw"
  } else
  {
    predict_type <- "prob"
    if (is.na(class_number))
      class_number <- 1
  }

  # assertthat::assert_that(model$modelType == "Regression",
  #                         msg = "Only Regression predictions are currently implemented")

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
  pmeans <- predict(model, newdata = means, type = predict_type)

  # predict on the range of possibilities (type = "raw" by default)
  dat <- .create_data_low_high(means, endpoints)
  pdat <- predict(model, newdata = dat, type = predict_type)

  if (model$modelType != "Regression")
  {
    # if the model is a classificaiton model, get the base probabilities
    pmeans <- unlist(pmeans[class_number])
    pdat <- unlist(pdat[[class_number]])
  }

  bar_width <- abs(apply(matrix(c(pdat), nrow = 2, byrow = TRUE), 2, diff))
  alt.order <- order(bar_width, decreasing = FALSE)

  plotdat <- data.frame(variable = rep(dict$Description.for.Presentation[match(names_means, dict$Orig.Node.Name)], times = 2),
                        value = c(pdat) - c(pmeans),
                        Level = rep(base_Level, each = lmeans),
                        stringsAsFactors = FALSE)
  plotdat$variable <- factor(plotdat$variable,
                             levels = dict$Description.for.Presentation[match(names_means, dict$Orig.Node.Name)][alt.order],
                             ordered = FALSE)
  plotdat$Level <- factor(plotdat$Level, levels = base_Level, ordered = FALSE,
                          labels = Level)

  factor_plotdat <- .create_factor_plot_data(training_data, means, pmeans, model, predict_type)

  if (model$modelType != "Regression")
  {
    temp_names <- names(factor_plotdat)
    temp_names[1 + class_number] <- "value"
    names(factor_plotdat) <- temp_names
  }

  return(structure(list(data = list(plotdat = plotdat,
                                    pmeans = pmeans,
                                    factordat = factor_plotdat),
                        type = "train",
                        family = NA),
                   class = "tornado_plot"))
}
