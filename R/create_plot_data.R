# Copyright 2019 Robert Carnell

#' Internal Method to create Plot Data in tornado plots
#'
#' @param model a glm object
#' @param modeldata the data that the model was fit with
#' @param type PercentChange, percentiles, or ranges
#' @param alpha the level of change
#' @param alt.order an alternate order for the plot
#' @param dict a dictionary to translate variables for the plot
#' @param xlabel a label for the x-axis
#'
#' @return the data to create the tornado plot
#'
#' @importFrom assertthat assert_that
#' @importFrom stats terms predict predict.lm predict.glm
#' @noRd
.create_plot_data <- function(model, modeldata, type="PercentChange", alpha=0.10,
                              alt.order=NA, dict=NA)
{
  if (FALSE)
  {
    # no factors
    model <- lm(mpg ~ cyl*wt*hp, data = mtcars)
    modeldata <- mtcars
    type <- "PercentChange"
    alpha <- 0.10
    alt.order <- NA
    dict <- NA
    # some factors
    modeldata <- mtcars
    modeldata$cyl <- as.factor(mtcars$cyl)
    modeldata$vs <- as.factor(mtcars$vs)
    model <- lm(mpg ~ cyl*wt*hp + vs, data = modeldata)
    type <- "PercentChange"
    alpha <- 0.10
    alt.order <- NA
    dict <- NA
    # one factors
    modeldata <- mtcars
    modeldata$cyl <- as.factor(mtcars$cyl)
    model <- lm(mpg ~ cyl*wt*hp + vs, data = modeldata)
    type <- "PercentChange"
    alpha <- 0.10
    alt.order <- NA
    dict <- NA
    # one factors
    modeldata <- mtcars
    modeldata$cyl <- as.factor(mtcars$cyl)
    model <- lm(mpg ~ cyl*wt*hp + vs, data = modeldata)
    type <- "ranges"
    alpha <- 0.10
    alt.order <- NA
    dict <- NA
  }
  assertthat::assert_that(is.data.frame(modeldata),
                          msg = "The data must be contained in a data.frame")
  assertthat::assert_that(type %in% c("PercentChange","percentiles","ranges"),
                          msg = "type must be PercentChagne, percentiles, or ranges")

  used_variables <- rownames(attr(stats::terms(model), "factors"))[-1]

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
  pmeans <- predict(model, newdata = means, type = "response")

  # predict on the range of possibilities
  dat <- .create_data_low_high(means, endpoints)
  pdat <- predict(model, dat, type = "response")

  if (is.na(alt.order))
  {
    bar_width <- abs(apply(matrix(c(pdat), nrow = 2, byrow = TRUE), 2, diff))
    alt.order <- order(bar_width, decreasing = FALSE)
  } else {
    assertthat::assert_that(length(alt.order) == lmeans)
  }

  plotdat <- data.frame(variable = rep(dict$Description.for.Presentation[match(names_means, dict$Orig.Node.Name)], times = 2),
                        value = pdat - pmeans,
                        Level = rep(base_Level, each = lmeans),
                        stringsAsFactors = FALSE)
  plotdat$variable <- factor(plotdat$variable,
                             levels = dict$Description.for.Presentation[match(names_means, dict$Orig.Node.Name)][alt.order],
                             ordered = FALSE)
  plotdat$Level <- factor(plotdat$Level, levels = base_Level, ordered = FALSE,
                          labels = Level)
  # if there are factors in the data, add a new plotting element to add points
  #   where the factor predictions are
  ind <- which(sapply(training_data, class) == "factor")
  if (length(ind) > 0)
  {
    factor_results <- vector("list", length = length(ind))
    factor_predictions <- vector("list", length = length(ind))
    for (i in seq_along(ind))
    {
      nlevs <- nlevels(training_data[,ind[i]])
      tempmeans <- NULL
      for (j in 1:nlevs)
      {
        tempmeans <- rbind(tempmeans, means)
      }
      tempmeans[,ind[i]] <- levels(training_data[,ind[i]])
      factor_predictions[[i]] <- predict(model, newdata = tempmeans)
      factor_results[[i]] <- data.frame(variable = rep(names(training_data)[ind[i]], nlevs),
                                        value = factor_predictions[[i]])
    }
    factor_plotdat <- do.call("rbind", factor_results)
  } else
  {
    factor_plotdat <- NA
  }
  return(list(plotdat = plotdat, pmeans = pmeans, factor_plotdat = factor_plotdat))
}
