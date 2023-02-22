# Copyright 2021 Robert Carnell

#' Create variable endpoints for tornado plots
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
  assertthat::assert_that(type %in% c("PercentChange", "percentiles", "ranges", "StdDev"),
                          msg = "Type must be one of PercentChange, percentiles, ranges, StdDev")

  # if (type == "PercentChange" && length(which_factor) > 0)
  # {
  #   warning("The PercentChange method will not show variation for factor variables")
  # } else if (type == "percentiles" && length(which_factor) > 0)
  # {
  #   warning("The percentiles method will not show variation for factor variables")
  # }

  ## All factors
  if (lmeans == length(which_factor))
  {
    endpoints <- as.data.frame(matrix(NA, nrow = 2, ncol = lmeans))
    names(endpoints) <- names(means)
    Level = NA
  } else if (type == "percentiles" && alpha > 0 && alpha < 0.5)
  {
    if (length(which_factor) > 0)
    {
      endpoints <- data.frame(
        apply(training_data[,-which_factor], 2, stats::quantile, probs = c(alpha, 1 - alpha))
      )
      names(endpoints) <- names(means)[-which_factor]
      endpoints2 <- data.frame(lapply(means[,which_factor], function(z) rep(z, 2)))
      names(endpoints2) <- names(means)[which_factor]
      endpoints <- cbind(endpoints, endpoints2)
    } else
    {
      endpoints <- data.frame(
        apply(training_data, 2, stats::quantile, probs = c(alpha, 1 - alpha))
      )
      names(endpoints) <- names(means)
    }
    Level <- c(paste0(round(alpha*100,0),"th"),
               paste0(round((1 - alpha)*100,0), "th"))
  } else if (type == "PercentChange" && alpha > 0)
  {
    if (length(which_factor) > 0)
    {
      endpoints <- data.frame(
        rbind((1 - alpha) * as.numeric(means[-which_factor]),
              (1 + alpha) * as.numeric(means[-which_factor])))
      names(endpoints) <- names(means)[-which_factor]
      endpoints2 <- data.frame(lapply(means[,which_factor], function(z) rep(z, 2)))
      names(endpoints2) <- names(means)[which_factor]
      endpoints <- cbind(endpoints, endpoints2)
    } else
    {
      endpoints <- data.frame(
        cbind(c(1 - alpha, 1 + alpha)) %*% as.numeric(means)
      )
      names(endpoints) <- names(means)
    }
    Level <- scales::percent(c(1 - alpha, 1 + alpha))
  } else if (type == "ranges")
  {
    if (length(which_factor) > 0)
    {
      endpoints <- as.data.frame(
        apply(training_data[,-which_factor], 2, range)
      )
      names(endpoints) <- names(means)[-which_factor]
      endpoints2 <- data.frame(lapply(means[,which_factor], function(z) rep(z, 2)))
      names(endpoints2) <- names(means)[which_factor]
      endpoints <- cbind(endpoints, endpoints2)
    } else
    {
      endpoints <- as.data.frame(apply(training_data, 2, range))
      names(endpoints) <- names(means)
    }
    Level <- c("Lower","Upper")
  } else if (type == "StdDev" && alpha > 0)
  {
    sdf <- function(z)
    {
      c(mean(z) - alpha*stats::sd(z), mean(z) + alpha*stats::sd(z))
    }
    if (length(which_factor) > 0)
    {
      endpoints <- as.data.frame(
        apply(training_data[,-which_factor], 2, sdf)
      )
      names(endpoints) <- names(means)[-which_factor]
      endpoints2 <- data.frame(lapply(means[,which_factor], function(z) rep(z, 2)))
      names(endpoints2) <- names(means)[which_factor]
      endpoints <- cbind(endpoints, endpoints2)
    } else
    {
      endpoints <- as.data.frame(apply(training_data, 2, sdf))
      names(endpoints) <- names(means)
    }
    Level <- c(paste0("mu - ", alpha, " sigma"), paste0("mu + ", alpha, " sigma"))
  } else
  {
    stop("command not recognized")
  }

  return(list(endpoints = endpoints, Level = Level))
}
