# Scatterplot -------------------------------------------------------------

#' simple scatter plot, adding marginal histogram by default.
#' @import ggplot2
#'
#' @param data dataset
#' @param dilute a number or logical, dafault to TRUE, will plot \code{nrow(data_long)/dilute} data. For example, if dilute = 5 will plot 1/5 of the data.
#' @param size0 point size, default to 1 of nobs<1000, 0.4 if nobs>1000.
#'
#' @examples
#' scatter.plot.simple(data = datam1_terra, x = "col_water_cm", y = "Column_WV")
#'
#' @export scatter.plot.simple
#'
scatter.plot.simple <-  function(data, x, y, size0 = 0.1, alpha0 = 0.3,
                                 dilute = FALSE,
                                 add_hist = TRUE){
  set.seed(1234)
  if (is.null(dilute)) dilute = FALSE
  if (dilute!=0){
    dilute <- ceiling(min(nrow(data0)/10, abs(as.numeric(dilute)))) # not allowed to dilute to fewer than 10 obs/feature
    set.seed(1234)
    data0 <- data0[sample(nrow(data0), min(nrow(data0)/dilute, 1500))] # dilute
  }
  if (is.null(size0)) size0 <- if(nrow(data0)<1000L) 1 else 0.4

  plot0 <- ggplot(data = data, aes(x = data[[x]], y = data[[y]]))+
    geom_point(size = size0, alpha = alpha0) +
    # geom_density_2d(aes(fill = ..level..), geom = "polygon") +
    labs(x = x, y = y) +
    theme_bw()
  # add histogram by default
  if(add_hist){
    plot0 <- ggExtra::ggMarginal(plot0, type = "histogram", bins = 50, size = 10, color="white")
  }
  return(plot0)
}

#' make customized scatter plot with diagonal line and R2 printed.
#' @importFrom ggpubr stat_cor
#' @import data.table
#' @importFrom BBmisc capitalizeStrings
#' @export scatter.plot.diagonal
#' @examples
#' scatter.plot.diagonal(data = datam1_terra, x = "col_water_cm", y = "Column_WV", add_abline = T)
#'
#'
scatter.plot.diagonal <- function(data, x, y, dilute = F, add_abline = F, add_hist = T){
  # with diagonal line and R2
  if (x == 'dayint'){
    setDT(data)[, dayint:= as.Date(data[,dayint], format = "%Y-%m-%d",origin = "1970-01-01")]
  }
  # core plot
  plot1 <-  ggplot(data = data, aes(x = data[[x]], y = data[[y]]))+
    geom_point(size = 0.1, alpha = 0.3) +
    theme_bw() +
    geom_smooth(method = 'lm') +
    labs(x = BBmisc::capitalizeStrings(x), y = BBmisc::capitalizeStrings(y)) +
    ggpubr::stat_cor(method = "pearson")

  if(add_abline){
    plot1 <- plot1 + geom_abline(intercept =0 , slope = 1, color = "grey")

  }
  cat("R2 is", summary(lm(data[[y]]~data[[x]], na.action = na.omit))$r.squared,"\n")
  # customize labels
  if(add_hist){
    plot1 <- ggExtra::ggMarginal(plot1, type = "histogram", bins = 50, size = 10, color="white")
  }
  return(plot1)
}


# Maps --------------------------------------------------------------------

# us map using ggmap
#' @import ggmap
#' @export ggmap.plot.us
#'
ggmap.plot.us <- function(data0){
  # data0 contains the points to plot, plot on whole us mainland
  map_wholeUS <- readRDS("~/Jdrive/PM/Just_Lab/projects/ECHO_PM/data/intermediate/map_us_all.RData")
  ggmap::ggmap(map_wholeUS) +
    geom_point(data = data0, aes(x = lon, y = lat),size = 1, color= "red") + # MOD
    # geom_label(data = SiteName_aodid, aes(label = Site_Name), size = 2, alpha = 0.7) +
    theme(legend.position='none')
}


#' us map with fill = size0.
#'

ggmap.plot.us.size <- function(data0 = aeronetdt, size0 = "Ndays", sat0 = sat, NEMIA = F){
  # optional to print NEMIA alone
  NE_list <- c("maine", "new hampshire", "vermont",
               "massachusetts", "rhode island", "connecticut",
               "new york", "new jersey", "pennsylvania",
               "delaware", "maryland", "west virginia", "virginia")

  # borrowed from nemia_aeronet_stn.R
  g1 = ggplot(data = if (NEMIA) map_data("state", region = NE_list) else map_data("state"),
              aes(x = long, y = lat, group = group)) +
    geom_polygon(fill = 'NA', color = "grey") +
    coord_map("albers", parameters = list(at0 = 45.5, lat1 = 29.5), expand = c(0, 0)) +
    geom_point(aes_string("lon", "lat", "group" = 1, size = size0),
               alpha = 0.3, data = data0)+
    # geom_point(aes(lon, lat, group = 1, size = stn_count),
    #            alpha = 0.3, data = data0)+
    scale_size_continuous(name = paste0("Number of \nDays (",Hmisc::capitalize(sat0),")"),
                          breaks = c((1:3)*500), range = c(1, 10)) +
    ylab("Latitude") + xlab("Longitude") +
    theme_minimal() +
    theme(legend.position = c(0.1, 0.75))
  if (!NEMIA){
    g1 <- g1 +
      xlim(c(-130, -60)) +
      ylim(c(25, 50))
  }
  g1
}

#' plot.nemia.map. plot NEMIA region (NY state) using ggmap (google map).
#' @export ggmap.plot.nemia
#'
ggmap.plot.nemia <- function(data0){
  ggmap(map_US_NEMIA) +
    geom_point(data = data0, aes(x = lon, y = lat),size = 0.05) + # MOD
    # geom_label(data = SiteName_aodid, aes(label = Site_Name), size = 2, alpha = 0.7) +
    theme(legend.position='none')
}


#' plot only the NYC map
#' @import data.table
#' @import sf
#' @export sf.plot.nyc
#'
#' @examples sf.plot.nyc(datam1_terra)
#'
sf.plot.nyc <- function(data, lon = 'lon', lat = 'lat', zoom_in_option = 2, fill = NULL){
  # only need lat lon (WGS84) from data, will plot all the points
  data <- as.data.table(data)
  setnames(data, c(lon, lat), c("lon", "lat"))

  bd1 <- as.data.frame(cbind(
    lon = c(-74.4, -73.5),
    lat = c(41.5, 40.5)
  ))

  # zoom in
  bd2 <- as.data.frame(cbind(
    lon = c(-74.25, -73.7),
    lat = c(40.98, 40.4)
  ))
  # zoom out
  bd3 <- as.data.frame(cbind(
    lon = c(-75, -73),
    lat = c(42, 40)
  ))
  bd <- as.matrix(cbind(
    ny1 = c((bd1)[1, 1], (bd1)[2, 1], (bd1)[2, 2], (bd1)[1, 2]),
    ny2 = c((bd2)[1, 1], (bd2)[2, 1], (bd2)[2, 2], (bd2)[1, 2]),
    ny3 = c((bd3)[1, 1], (bd3)[2, 1], (bd3)[2, 2], (bd3)[1, 2])
  ))

  fi = zoom_in_option
  data_nyc <- data[lon>bd[1,fi] & lon<bd[2,fi] & lat>bd[3,fi] & lat< (bd[4,fi]), ]
  nycmap <- ggplot() +
    # NEMIA land background
    geom_sf(data = NEMIA_sf_poly, fill = "gray90", color="black", inherit.aes = F) +
    # map extent
    xlim(bd[1,fi], bd[2,fi]) +
    ylim(bd[3,fi], bd[4,fi]) +
    # optional: overlay NEMIA outline on top of cells
    geom_sf(data = NEMIA_sf_poly, fill = NA, color="black", inherit.aes = F) +
    # # scale use ggsn
    # ggsn::scalebar(dist = 6, transform = FALSE, dist_unit = 'km',
    #         border.size = 0.4, st.size = 4.5, # boarder width and font size
    #         anchor = c(x = 1761000, y = 2224000),
    #         box.fill = c('black','white'),
    #         x.min = bd[1,fi], x.max = bd[2,fi], y.min= bd[3,fi], y.max =  bd[4,fi]) +
    coord_sf(expand = F) +
    theme_bw()
  if(is.null(fill)){
    nycmap <-  nycmap + geom_point(data = data_nyc, aes(x = lon, y = lat))
  } else {
    nycmap <-
      nycmap + geom_point (data = data_nyc, aes_string(x = "lon", y = "lat", color = fill))+
      scale_color_viridis_c(option = "plasma", direction = -1)

  }
  return(nycmap)
}
