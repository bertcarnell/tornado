# Copyright 2021 Robert Carnell

#' Create training data column means or weighted column means for tornado plots
#'
#' @param training_data a data.frame
#' @param wt model weights
#'
#' @importFrom stats weighted.mean
#'
#' @return a data.frame of means
#' @noRd
.create_means <- function(training_data, wt = NA)
{
  if (any(is.na(training_data))) {
    stop("NA values not permitted in training_data in .create_means")
  }
  means <- data.frame(lapply(training_data, function(x)
  {
    if (is.numeric(x))
    {
      if (any(is.na(wt))) {
        return(mean(x))
      } else {
        return(stats::weighted.mean(x, wt))
      }
      return(mean(x))
    } else if (is.factor(x))
    {
      # pick the most frequent class
      if (any(is.na(wt))) {
        tt <- table(x)
        ttmax <- names(tt[which.max(tt)])
        return(factor(ttmax, levels = levels(x)))
      } else {
        tt <- by(wt, x, sum)
        ttmax <- names(tt[which.max(tt)])
        return(factor(ttmax, levels = levels(x)))
      }
    }
  }))
  return(means)
}
