# Copyright 2021 Robert Carnell

#' Create training data column means for tornado plots
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
      # pick the most frequent class
      tt <- table(x)
      ttmax <- names(tt[which.max(tt)])
      return(factor(ttmax, levels = levels(x)))
    }
  }))
  return(means)
}
