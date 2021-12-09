# Copyright 2021 Robert Carnell

#' Create factor plot data
#'
#' @param training_data the training data data.frame
#' @param means the means of the training data
#' @param pmeans the predictions at the means of the data
#' @param model the statistical model object
#' @param prediction_type the type of prediction used in predict.method.  Usually 'response' or 'raw'
#'
#' @return a data.frame
#'
#' @noRd
.create_factor_plot_data <- function(training_data, means, pmeans, model, prediction_type)
{
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
      character_levels <- levels(training_data[,ind[i]])
      tempmeans[,ind[i]] <- factor(character_levels, levels = character_levels)
      factor_predictions[[i]] <- predict(model, newdata = tempmeans, type = prediction_type)
      factor_results[[i]] <- data.frame(variable = rep(names(training_data)[ind[i]], nlevs),
                                        value = factor_predictions[[i]] - c(pmeans))
    }
    factor_plotdat <- as.data.frame(do.call("rbind", factor_results))
  } else
  {
    factor_plotdat <- NA
  }
  return(factor_plotdat)
}
