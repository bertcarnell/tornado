# Copyright 2021 Robert Carnell

#' create the high and low ends of the variable ranges
#'
#' @param means the variable means
#' @param endpoints the endpoints of the data
#'
#' @return a data.frame
#' @noRd
.create_data_low_high <- function(means, endpoints)
{
  if (!is.data.frame(endpoints))
    endpoints <- as.data.frame(endpoints)
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
