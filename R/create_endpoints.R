# Copyright 2021 Robert Carnell

.allowed_types <- c("PercentChange", "percentiles", "ranges", "StdDev")

#' Create variable endpoints for tornado plots
#'
#' @param training_data the data.frame with training data
#' @param means the data.frame with variable means
#' @param type the type of tornado plot
#' @param alpha the percentile or alpha level
#' @param wt model weights
#'
#' @importFrom stats quantile
#' @importFrom Hmisc wtd.quantile
#'
#' @return a list of the endpoints and levels
#' @noRd
.create_endpoints <- function(training_data, means, type, alpha, wt = NA)
{
  which_factor <- which(sapply(training_data, is.factor))
  lmeans <- length(means)
  if (length(type) != 1) {
    stop(paste0("type must be a singleton and must be one of ", paste(.allowed_types, collapse = ",")))
  }
  if (!(type %in% .allowed_types))
  {
    stop(paste0("type must be one of ", paste(.allowed_types, collapse = ",")))
  }

  ## All factors
  if (lmeans == length(which_factor))
  {
    endpoints <- as.data.frame(matrix(NA, nrow = 2, ncol = lmeans))
    names(endpoints) <- names(means)
    Level = NA
  ## percentiles
  } else if (type == "percentiles" && alpha > 0 && alpha < 0.5)
  {
    if (length(which_factor) > 0)
    {
      if (any(is.na(wt))) {
        endpoints <- data.frame(
          apply(training_data[,-which_factor], 2, stats::quantile, probs = c(alpha, 1 - alpha))
        )
      } else {
        endpoints <- data.frame(
          apply(training_data[,-which_factor], 2, Hmisc::wtd.quantile, weights = wt, probs = c(alpha, 1 - alpha))
        )
      }
      names(endpoints) <- names(means)[-which_factor]
      endpoints2 <- data.frame(lapply(means[,which_factor], function(z) rep(z, 2)))
      names(endpoints2) <- names(means)[which_factor]
      endpoints <- cbind(endpoints, endpoints2)
    } else
    {
      if (any(is.na(wt))) {
        endpoints <- data.frame(
          apply(training_data, 2, stats::quantile, probs = c(alpha, 1 - alpha))
        )
      } else {
        endpoints <- data.frame(
          apply(training_data, 2, Hmisc::wtd.quantile, weights = wt, probs = c(alpha, 1 - alpha))
        )
      }
      names(endpoints) <- names(means)
    }
    Level <- c(paste0(round(alpha*100,0),"th"),
               paste0(round((1 - alpha)*100,0), "th"))
  ## PercentChange
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
  ## ranges
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
  ## StdDev
  } else if (type == "StdDev" && alpha > 0)
  {
    sdf <- function(z, wt)
    {
      v <- ifelse(any(is.na(wt)), stats::sd(z), sqrt(Hmisc::wtd.var(z, wt)))
      c(mean(z) - alpha*v, mean(z) + alpha*v)
    }
    if (length(which_factor) > 0)
    {
      endpoints <- as.data.frame(
        apply(training_data[,-which_factor], 2, sdf, wt = wt)
      )
      names(endpoints) <- names(means)[-which_factor]
      endpoints2 <- data.frame(lapply(means[,which_factor], function(z) rep(z, 2)))
      names(endpoints2) <- names(means)[which_factor]
      endpoints <- cbind(endpoints, endpoints2)
    } else
    {
      endpoints <- as.data.frame(apply(training_data, 2, sdf, wt = wt))
      names(endpoints) <- names(means)
    }
    # grid graphics will not the multi-byte character encodings correctly
    #   on the examples when the pdf is created for R CMD check
    #Level <- c(paste0("\U003BC - ", alpha, "\u03C3"),
    #           paste0("\U003BC + ", alpha, "\u03C3"))
    Level <- c(paste0("mean - ", alpha, "*std"),
               paste0("mean + ", alpha, "*std"))
  } else
  {
    stop("type not recognized")
  }

  return(list(endpoints = endpoints, Level = Level))
}
