# Help func. --------------------------------------------------------------

#'Greedily pack objects with sizes given by `object.sizes`
#'into `n.bins` bins. Return a vector of the bin indices.
#'by Kodi Arfer.
#'@export helper.pack.bins
helper.pack.bins = function(object.sizes, n.bins){

  d = data.table(size = as.integer(object.sizes), bin = NA_integer_)
  d[, oi := .I]
  # Shuffle the input so ties on object size are broken randomly.
  d = d[sample.int(nrow(d))]
  bin.sizes = rep(0L, n.bins)
  while (anyNA(d$bin))
  {the.oi = d[is.na(bin)][which.max(size), oi]
  bsi = which.min(bin.sizes)
  d[oi == the.oi, bin := bsi]
  bin.sizes[bsi] = bin.sizes[bsi] + d[oi == the.oi, size]}
  d[order(oi), bin]
}

#' dilute dataset.
#'
#' @export helper.dilute.data
#'
helper.dilute.data <- function(data0, dilute_by = 10){
  set.seed(1234)
  data0  <- data0[sample(nrow(data0), nrow(data0)/dilute_by)]
  return(data0)
}

#' split aodid to numeric lat lon
#' @export helper.aodid.to.lon.lat
#' @return dataset with 2 more columns
helper.aodid.to.lon.lat <- function(data0){
  setDT(data0)
  data0[, c("lon", "lat") := tstrsplit(aodid, "_")]
  data0[, `:=`(lon = as.numeric(lon), lat = as.numeric(lat))]
  data1 = copy(data0)
  data1
}
