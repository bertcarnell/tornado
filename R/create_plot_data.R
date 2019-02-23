# Copyright 2019 Robert Carnell

#' Internal Method to create Plot Data
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
  return(list(plotdat = plotdat, pmeans = pmeans))
}


#' Create training data column means
#'
#' @param training_data a data.frame
#'
#' @return a data.frame of means
#' @noRd
.create_means <- function(training_data)
{
  means <- data.frame(lapply(training_data, function(x)
  {
    if (is.numeric(x))
    {
      return(mean(x))
    } else if (is.factor(x))
    {
      tt <- table(x)
      ttmax <- names(tt[which.max(tt)])
      return(factor(ttmax, levels = levels(x)))
    }
  }))
  return(means)
}

#' Create variable endpoints
#'
#' @param training_data the data.frame with training data
#' @param means the data.frame with variable means
#' @param type the type of tornado plot
#' @param alpha the percentile or alpha level
#'
#' @importFrom stats quantile
#'
#' @return a list of the endpoints and levels
#' @noRd
.create_endpoints <- function(training_data, means, type, alpha)
{
  which_factor <- which(sapply(training_data, is.factor))
  lmeans <- length(means)

  if (type == "PercentChange" && length(which_factor) > 0)
  {
    warning("The PercentChange method will not show variation for factor variables")
  } else if (type == "percentiles" && length(which_factor) > 0)
  {
    warning("The percentiles method will not show variation for factor variables")
  }

  if (lmeans == length(which_factor))
  {
    stop("all variables are factors")
  }
  if (type == "percentiles" && alpha > 0 && alpha < 0.5)
  {
    if (length(which_factor) > 0)
    {
      endpoints <- apply(training_data[-which_factor], 2, stats::quantile, probs = c(alpha, 1 - alpha))
      names(endpoints) <- names(means[-which_factor])
      endpoints2 <- rbind(means[,which_factor], means[,which_factor])
      endpoints <- cbind(endpoints, endpoints2)
    } else
    {
      endpoints <- apply(training_data, 2, quantile, probs = c(alpha, 1 - alpha))
      names(endpoints) <- names(means)
    }
    Level <- c(paste0(round(alpha*100,0),"th"),
               paste0(round((1 - alpha)*100,0), "th"))
  } else if (type == "PercentChange" && alpha > 0)
  {
    if (length(means[-which_factor]) == 1)
    {
      endpoints <- data.frame(c(1 - alpha, 1 + alpha) * as.numeric(means[-which_factor]))
      names(endpoints) <- names(means[-which_factor])
      endpoints2 <- rbind(means[,which_factor], means[,which_factor])
      endpoints <- cbind(endpoints, endpoints2)
    } else if (length(which_factor) > 1)
    {
      endpoints <- cbind(c(1 - alpha, 1 + alpha)) %*% as.numeric(means[-which_factor])
      names(endpoints) <- names(means[-which_factor])
      endpoints2 <- rbind(means[,which_factor], means[,which_factor])
      endpoints <- cbind(endpoints, endpoints2)
    } else
    {
      endpoints <- cbind(c(1 - alpha, 1 + alpha)) %*% as.numeric(means)
      names(endpoints) <- names(means)
    }
    Level <- scales::percent(c(1 - alpha, 1 + alpha))
  } else if (type == "ranges")
  {
    if (length(which_factor) > 0)
    {
      stop("needs more work for factors")
    } else
    {
      endpoints <- apply(training_data, 2, range)
      names(endpoints) <- names(means)
    }
    Level <- c("Lower","Upper")
  } else
  {
    stop("command not recognized")
  }

  return(list(endpoints = endpoints, Level = Level))
}

#' create the high and low ends of the variable ranges
#'
#' @param means the variable means
#' @param endpoints the endspoints of the data
#'
#' @return a data.frame
#' @noRd
.create_data_low_high <- function(means, endpoints)
{
  lmeans <- length(means)
  datlow <- lapply(1:lmeans, function(x) return(means))
  datlow <- do.call(rbind, datlow)
  for (i in 1:lmeans)
  {
    datlow[i,i] <- endpoints[1,which(names(endpoints) == names(datlow)[i])]
  }
  dathigh <- lapply(1:lmeans, function(x) return(means))
  dathigh <- do.call(rbind, dathigh)
  for (i in 1:lmeans)
  {
    dathigh[i,i] <- endpoints[2,which(names(endpoints) == names(dathigh)[i])]
  }

  dat <- as.data.frame(rbind(datlow, dathigh))
  return(dat)
}
