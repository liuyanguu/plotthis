# Some help func. --------------------------------------------------------------

#' dilute dataset.
#'
#' help function
#'
#' @param data0 dataset
#' @param dilute_by default to 10, 1/10
#' @export helper.dilute.data
#'
helper.dilute.data <- function(data0, dilute_by = 10){
  set.seed(1234)
  data0  <- data0[sample(nrow(data0), nrow(data0)/dilute_by)]
  return(data0)
}

#' split aodid to numeric lat lon.
#'
#' helper function
#'
#' @param data0 dataset
#' @export helper.aodid.to.lon.lat
#' @return dataset with 2 more columns
helper.aodid.to.lon.lat <- function(data0){
  setDT(data0)
  data0[, c("lon", "lat") := tstrsplit(aodid, "_")]
  data0[, `:=`(lon = as.numeric(lon), lat = as.numeric(lat))]
  data1 = copy(data0)
  data1
}
