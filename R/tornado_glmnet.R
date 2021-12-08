# Copyright 2019 Robert Carnell

#' GLMNET Tornado Diagram
#'
#' @param model a \code{cv.glmnet} object
#' @param modeldata the raw data used to fit the glmnet model
#' @param form the model formula
#' @param s Value(s) of the penalty parameter \code{lambda} at which predictions are required. Default is the value \code{s="lambda.1se"} stored on the CV object. Alternatively \code{s="lambda.min"} can be used. If s is numeric, it is taken as the value(s) of \code{lambda} to be used.
#' @param type PercentChange, percentiles, or ranges
#' @param alpha the level of change
#' @param alt.order an alternate order for the plot
#' @param dict a dictionary to translate variables for the plot
#' @param xlabel a label for the x-axis
#' @param ... further arugments
#'
#' @return the plot
#' @export
#' @method tornado cv.glmnet
#' @importFrom scales percent
#' @import ggplot2
#' @importFrom stats model.frame terms model.matrix predict
#'
#' @examples
#' if (requireNamespace("glmnet", quietly = TRUE))
#' {
#'   mf <- model.frame(mpg ~ cyl*wt*hp, data=mtcars)
#'   mm <- model.matrix(mf, mf)
#'   gtest <- glmnet::cv.glmnet(x = mm, y= mtcars$mpg, family = "gaussian")
#'   tornado(gtest, mtcars, formula(mpg ~ cyl*wt*hp), s="lambda.1se",
#'           type = "PercentChange", alpha = 0.10, xlabel = "MPG")
#' }
tornado.cv.glmnet <- function(model, modeldata, form, s="lambda.1se",
                           type="PercentChange", alpha=0.10,
                           alt.order=NA, dict=NA, xlabel="Response Rate",
                           ...)
{
  # form <- formula(mpg ~ cyl*wt*hp)
  # modeldata <- mtcars
  # model <- cv.glmnet(x = modelmatrix, y = mtcars$mpg, family = "gaussian")
  #
  # modelframe <- model.frame(form, data = mtcars)
  # modelmatrix <- model.matrix(form, modelframe)
  # type <- "PercentChange"
  # alpha <- 0.10
  # dict <- NA

  assertthat::assert_that(requireNamespace("glmnet", quietly = TRUE),
                          msg = "The glmnet package is required to use this method")

  extraArguments <- list(...)
  assertthat::assert_that(is.data.frame(modeldata),
                          msg = "The data must be contained in a data.frame")
  assertthat::assert_that(type %in% c("PercentChange","percentiles","ranges"),
                          msg = "type must be PercentChagne, percentiles, or ranges")

  modelframe <- model.frame(form, data = modeldata)
  used_variables <- rownames(attr(terms(modelframe), "factors"))

  if (length(dict) == 1 && is.na(dict))
  {
    dict <- data.frame(Orig.Node.Name = used_variables,
                       Description.for.Presentation = used_variables)
    if (any(grepl("x_[0-9][0-9]_[0-9][0-9]_[0-9][0-9]_", dict$Orig.Node.Name)))
    {
      dict$Description.for.Presentation <- gsub("[xX]_[0-9][0-9]_[0-9][0-9]_[0-9][0-9]_", "", dict$Description.for.Presentation)
    }
  } else
  {
    assertthat::assert_that(all(names(dict) == c("Orig.Node.Name", "Description.for.Presentation")))
    assertthat::assert_that(all(used_variables %in% dict$Orig.Node.Name))
  }

  training_data <- subset(modeldata, select = used_variables)
  means <- .create_means(training_data)
  names_means <- names(means)
  lmeans <- length(means)

  ret <- .create_endpoints(training_data, means, type, alpha)
  endpoints <- ret$endpoints
  Level <- ret$Level
  base_Level <- c("A","B")

  # predict the mean response
  newx <- model.matrix(form, model.frame(form, data = means))
  # glmnet::predict.cv.glmnet is not exported, just call predict
  pmeans <- predict(model, newx = newx, s = s, type = "response")

  # predict on the range of possibilities
  dat <- .create_data_low_high(means, endpoints)
  newx <- model.matrix(form, model.frame(form, data = dat))
  # glmnet::predict.cv.glmnet is not exported, just call predict
  pdat <- predict(model, newx, s = s, type = "response")

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

  pretty_break <- pretty(plotdat$value, n = 5)

  ggp <- ggplot(plotdat, aes_string(x = "variable", y = "value", fill = "Level")) +
    geom_bar(position = "identity", stat = "identity") +
    coord_flip() +
    ylab(xlabel) +
    xlab("") +
    scale_fill_manual(values = c("grey", "#69BE28")) +
    theme_bw()

  if (model$glmnet.fit$call$family %in% c("binomial", "quasibinomial"))
  {
    ggp <- ggp +
      scale_y_continuous(breaks = pretty_break,
                         labels = scales::percent(c(pretty_break) + c(pmeans)),
                         limits = c(max(min(pretty_break),-pmeans), min(max(pretty_break), 1 - pmeans)))
  } else
  {
    ggp <- ggp + scale_y_continuous(breaks = pretty_break,
                                    labels = format(c(pretty_break) + c(pmeans), digits = 4))
  }
  return(ggp)
}
