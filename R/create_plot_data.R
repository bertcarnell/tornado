# Copyright 2019 Robert Carnell
#' @include create_endpoints.R

#' Internal Method to create Plot Data in tornado plots
#'
#' @param model a glm object
#' @param modeldata the data that the model was fit with
#' @param type PercentChange, percentiles, or ranges
#' @param alpha the level of change
#' @param dict a dictionary to translate variables for the plot
#' @param predict_type The \code{type} argument passed to \code{predict}
#'
#' @return the data to create the tornado plot
#'
#' @importFrom stats terms weights
#' @noRd
.create_plot_data <- function(model, modeldata, type="PercentChange", alpha=0.10,
                              dict=NA, predict_type = "response")
{
  # if (FALSE)
  # {
  #   # no factors
  #   model <- lm(mpg ~ cyl*wt*hp, data = mtcars)
  #   modeldata <- mtcars
  #   type <- "PercentChange"
  #   alpha <- 0.10
  #   dict <- NA
  #   # some factors
  #   modeldata <- mtcars
  #   modeldata$cyl <- as.factor(mtcars$cyl)
  #   modeldata$vs <- as.factor(mtcars$vs)
  #   model <- lm(mpg ~ cyl*wt*hp + vs, data = modeldata)
  #   type <- "PercentChange"
  #   alpha <- 0.10
  #   dict <- NA
  #   # one factors
  #   modeldata <- mtcars
  #   modeldata$cyl <- as.factor(mtcars$cyl)
  #   model <- lm(mpg ~ cyl*wt*hp + vs, data = modeldata)
  #   type <- "PercentChange"
  #   alpha <- 0.10
  #   dict <- NA
  #   # one factors
  #   modeldata <- mtcars
  #   modeldata$cyl <- as.factor(mtcars$cyl)
  #   model <- lm(mpg ~ cyl*wt*hp + vs, data = modeldata)
  #   type <- "ranges"
  #   alpha <- 0.10
  #   dict <- NA
  # }
  if (!is.data.frame(modeldata)) {
    stop("The data must be contained in a data.frame")
  }
  if (!(type %in% .allowed_types)) {
    stop(paste0("type must be in ", paste(.allowed_types, collapse=",")))
  }

  used_variables <- rownames(attr(stats::terms(model), "factors"))[-1]

  dict <- .create_dict(dict, used_variables)

  training_data <- subset(modeldata, select = used_variables)
  if (is.null(stats::weights(model))) {
    model_weights <- NA
  } else {
    model_weights <- stats::weights(model)
  }
  means <- .create_means(training_data, model_weights)
  names_means <- names(means)
  lmeans <- length(means)

  ret <- .create_endpoints(training_data, means, type, alpha, model_weights)
  endpoints <- ret$endpoints
  Level <- ret$Level
  base_Level <- c("A","B")

  # predict the mean response
  pmeans <- predict(model, newdata = means, type = predict_type)

  # predict on the range of possibilities
  dat <- .create_data_low_high(means, endpoints)
  pdat <- predict(model, dat, type = predict_type)

  bar_width <- abs(apply(matrix(c(pdat), nrow = 2, byrow = TRUE), 2, diff))
  alt.order <- order(bar_width, decreasing = FALSE)

  plotdat <- data.frame(variable = rep(dict$new[match(names_means, dict$old)], times = 2),
                        value = pdat - pmeans,
                        Level = rep(base_Level, each = lmeans),
                        stringsAsFactors = FALSE)
  plotdat$variable <- factor(plotdat$variable,
                             levels = dict$new[match(names_means, dict$old)][alt.order],
                             ordered = FALSE)
  plotdat$Level <- factor(plotdat$Level, levels = base_Level, ordered = FALSE,
                          labels = Level)

  factor_plotdat <- .create_factor_plot_data(training_data, means, pmeans, model, predict_type)

  if (!all(is.na(factor_plotdat)))
    factor_plotdat$variable <- dict$new[match(factor_plotdat$variable, dict$old)]

  return(list(plotdat = plotdat, pmeans = pmeans, factor_plotdat = factor_plotdat))
}
