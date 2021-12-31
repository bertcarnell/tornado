# Copyright 2021 Robert Carnell

#' Internal Method to create or modify a dictionary
#'
#' @param dict NA or a data.frame
#' @param used_variables a list of variables used in a model
#'
#' @return the dictionary data.frame
#'
#' @importFrom assertthat assert_that
#' @noRd
.create_dict <- function(dict, used_variables)
{
  if (length(dict) == 1 && is.na(dict))
  {
    dict <- data.frame(old = used_variables,
                       new = used_variables)
    if (any(grepl("[xX]_[0-9][0-9]_[0-9][0-9]_[0-9][0-9]_", dict$old)))
    {
      dict$new <- gsub("[xX]_[0-9][0-9]_[0-9][0-9]_[0-9][0-9]_", "", dict$new)
    }
  } else
  {
    assertthat::assert_that(all(names(dict) == c("old", "new")),
                            msg = "The variable name translation dictionary must be a list or data.frame with components old and new")
    assertthat::assert_that(all(used_variables %in% dict$old),
                            msg = "All the variables used in the model must be in dict$old")
  }
  return(dict)
}
