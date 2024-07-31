# Copyright 2019 Robert Carnell

#' GLMNET Tornado Diagram
#'
#' @inherit tornado description
#'
#' @inheritParams tornado
#' @param modeldata the raw data used to fit the glmnet model
#' @param form the model formula
#' @param s Value(s) of the penalty parameter \code{lambda} at which predictions are required. Default is the value \code{s="lambda.1se"} stored on the CV object. Alternatively \code{s="lambda.min"} can be used. If s is numeric, it is taken as the value(s) of \code{lambda} to be used.
#'
#' @inherit tornado return
#' @export
#' @method tornado cv.glmnet
#' @importFrom stats model.frame terms model.matrix predict
#'
#' @seealso \code{\link{tornado}}
#'
#' @examples
#' if (requireNamespace("glmnet", quietly = TRUE))
#' {
#'   form <- formula(mpg ~ cyl*wt*hp)
#'   mf <- model.frame(form, data = mtcars)
#'   mm <- model.matrix(form, data = mf)
#'   gtest <- glmnet::cv.glmnet(x = mm, y= mtcars$mpg, family = "gaussian")
#'   torn <- tornado(gtest, modeldata = mtcars, form = formula(mpg ~ cyl*wt*hp), s = "lambda.1se",
#'                   type = "PercentChange", alpha = 0.10)
#'   plot(torn, xlabel = "MPG")
#' }
tornado.cv.glmnet <- function(model,
                           type="PercentChange", alpha=0.10,
                           dict=NA, modeldata, form, s="lambda.1se", ...)
{
  # form <- formula(mpg ~ cyl*wt*hp)
  # modeldata <- mtcars
  # mf <- model.frame(form, data = mtcars)
  # mm <-  model.matrix(form, data = mf)
  # model <- glmnet::cv.glmnet(x = mm, y = mtcars$mpg, family = "gaussian")
  # type <- "PercentChange"
  # alpha <- 0.10
  # dict <- NA
  # s <- "lambda.1se"

  if (!requireNamespace("glmnet", quietly = TRUE)) {
    stop("The glmnet package is required to use this method")
  }

  extraArguments <- list(...)

  if (!is.data.frame(modeldata)) {
    stop("The data must be contained in a data.frame")
  }
  if (!(type %in% .allowed_types)) {
    stop(paste("type must be in ", paste0(.allowed_types, collapse=",")))
  }
  if (!(s %in% c("lambda.1se", "lambda.min"))) {
    stop("The value of the penalty parameter, s, must be lambda.1se or lambda.min")
  }

  modelframe <- model.frame(form, data = modeldata)
  used_variables <- rownames(attr(terms(modelframe), "factors"))
  # the response variable is first
  #used_variables <- used_variables[-1]

  dict <- .create_dict(dict, used_variables)

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

  plotdat <- data.frame(variable = rep(dict$new[match(names_means, dict$old)], times = 2),
                        value = c(pdat) - c(pmeans),
                        Level = rep(base_Level, each = lmeans),
                        stringsAsFactors = FALSE)

  # need to remove the response variable from the output plot
  response_variable <- used_variables[1]
  ind_response <- which(plotdat$variable == response_variable)

  if (length(ind_response) == 0) {
    stop("Unexpected error in response variable selection")
  }
  plotdat <- plotdat[-ind_response,]

  ind_response_mean <- which(names_means == response_variable)

  if (length(ind_response_mean) == 0) {
    stop("Unexpected error in response variable selection")
  }
  names_means <- names_means[-ind_response_mean]

  bar_width <- abs(apply(matrix(plotdat$value, nrow = 2, byrow = TRUE), 2, diff))
  alt.order <- order(bar_width, decreasing = FALSE)

  plotdat$variable <- factor(plotdat$variable,
                             levels = dict$new[match(names_means, dict$old)][alt.order],
                             ordered = FALSE)
  plotdat$Level <- factor(plotdat$Level, levels = base_Level, ordered = FALSE,
                          labels = Level)


  return(structure(list(data = list(plotdat = plotdat,
                                    pmeans = pmeans,
                                    factordat = NA),
                        type = "cv.glmnet",
                        family = model$glmnet.fit$call$family),
                   class = "tornado_plot"))
}
