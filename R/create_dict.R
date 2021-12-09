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
  return(dict)
}
