#' @title Nicely formatted polar ggplot
#' @description Produce a nicely formatted maps of the Arctic or Antarctic in
#' polar projection. Also works for longitudinal segements, e.g. just Greenland.
#' Adapted from Mikey Harper's plot https://stackoverflow.com/a/49084793
#' @param pole which pole N or S?
#' @param max.lat maximum latitude
#' @param min.lat minimum latitude
#' @param max.lon maximum longitude
#' @param min.lon minimum longitude
#' @param longitude.spacing interval between longitude axis tick marks
#' @param land.fill.colour colour to shade the land
#' @param country.outline.colour colour for political boundaries (default
#' "Black"); set to the same as \code{land.fill.colour} to hide them.
#' @param n.lat.labels approximate number of latitude tickmarks
#' @param nearest.x.degrees round latitude tickmarks to how many degrees?
#' @param rotate if plotting a segement of < 360 degrees longitude, rotate plot
#' so that north is up (or south is down)
#' @param data.layer optional ggplot2 layer of data onto which the polar map
#' shall be plotted. Defaults to \code{NULL} which only plots the map.
#' @import ggplot2 maptools rgeos
#' @importFrom raster crop extent
#' @author Andrew Dolman <andrew.dolman@awi.de>
#' @export
#' @examples
#' ggpolar(pole = "N", max.lat = 90, min.lat = 55, n.lat.labels = 4)
#' ggpolar(pole = "S", max.lat = -60, min.lat = -90)
#'
#' ggpolar(pole = "N", max.lat = 90, min.lat = 55,
#'                max.lon = 0, min.lon = -80,
#'                longitude.spacing = 15, n.lat.labels = 5) +
#'   geom_point(aes(x = -35, y = 75, colour = "sd")) +
#'   geom_point(aes(x = -35, y = 70, colour = "sf"))
#'
#'
#' ggpolar(pole = "N", max.lat = 90, min.lat = 55,
#'                max.lon = 0, min.lon = -80,
#'                longitude.spacing = 30,
#'                rotate = TRUE)
#'
#' ggpolar(pole = "S", max.lat = -55, min.lat = -90,
#'         max.lon = 80, min.lon = -20,
#'         longitude.spacing = 30,
#'         rotate = TRUE)
#'
#'
#' ggpolar(pole = "N", max.lat = 90, min.lat = 55,
#'                max.lon = 170, min.lon = -180,
#'                longitude.spacing = 33)
#'
#' # set country.outline.colour the same as land.fill.colour to hide political
#' # boundaries
#' ggpolar(pole = "N", max.lat = 90, min.lat = 65, n.lat.labels = 4,
#'  country.outline.colour = "Grey")
#'
#' \dontrun{
#' ggpolar(pole = "W", max.lat = -55, min.lat = -90)
#' ggpolar(pole = "S", max.lat = 90, min.lat = 55)
#' ggpolar(pole = "S", max.lat = -90, min.lat = -55)
#' }
ggpolar <- function(pole = c("N", "S"),
                    max.lat, min.lat,
                    max.lon = 180, min.lon = -180,
                    longitude.spacing = 60,
                    land.fill.colour = "Grey",
                    country.outline.colour = "Black",
                    n.lat.labels = 4, nearest.x.degrees = 5, rotate = FALSE,
                    size.outer = 1,
                    data.layer = NULL) {

  pole <- match.arg(pole, choices = c("N", "S"))

  if (pole == "N" & max.lat < 0) stop("If pole == N, max.lat should be positive")
  if (pole == "S" & min.lat > 0) stop("If pole == S, min.lat should be negative")

  stopifnot(max.lat > min.lat)
  stopifnot(max.lon <= 180 & min.lon >= -180 & max.lat <= 90 & min.lat >= -90)

  is.segment <- (min.lon == -180 & max.lon == 180) == FALSE

  mean.lon <- mean(c(min.lon, max.lon))

  # if not a segment
  if ((min.lon == -180 & max.lon == 180)){
    mean.lon <- 180
  }

  if (rotate) {rotate.to <- mean.lon} else {rotate.to <- 0}

  lat.range <- abs(max.lat - min.lat)
  d.lat.ax.vals <- nearest.x.degrees * round((max.lat - min.lat)/n.lat.labels/nearest.x.degrees)

  # Hemishphere specific values
  if (pole == "N") {
    lat.ax.vals <- seq(min.lat + d.lat.ax.vals, max.lat,
                       by = d.lat.ax.vals)
    outer.lat.ax.val <- min.lat
    long.lab.pos.1 <- outer.lat.ax.val - (lat.range / 20)
    long.lab.pos.2 <- outer.lat.ax.val - (lat.range / 7)

    long.line.strt <- max.lat
    long.line.end <- long.lab.pos.1

  } else if (pole == "S") {
    lat.ax.vals <- seq(max.lat - d.lat.ax.vals, min.lat,
                       by = -d.lat.ax.vals)
    outer.lat.ax.val <- max.lat
    long.lab.pos.1 <- outer.lat.ax.val + (lat.range / 20)
    long.lab.pos.2 <- outer.lat.ax.val + (lat.range / 7)

    long.line.strt <- long.lab.pos.1
    long.line.end <- min.lat
  }

  lat.ax.vals <- lat.ax.vals[abs(lat.ax.vals) != 90]
  lat.ax.labs <- paste0(ifelse(lat.ax.vals < 0, -lat.ax.vals, lat.ax.vals),
                        "°", ifelse(lat.ax.vals < 0, " S", " N"))

  # Defines the x axes required
  long.ax.vals <- seq(min.lon, max.lon, by = longitude.spacing)

  if (is.segment == FALSE){
    long.ax.vals <- long.ax.vals[long.ax.vals != -180]
  }

  long.ax.labs <- paste0(abs(long.ax.vals), "°", ifelse(long.ax.vals <=
                                                          0, " W", " E"))
  long.ax.labs[long.ax.vals == 0] <- "0°"
  long.ax.labs[long.ax.vals == 180] <- "180° W"

  lat.lines <- expand.grid(long = min.lon:max.lon, lat = lat.ax.vals)

  # Get map outline and crop
  data("wrld_simpl", package = "maptools")
  map.outline <- raster::crop(wrld_simpl,
                              raster::extent(min.lon, max.lon, min.lat, max.lat),
                              snap = "in")

  if (!length(data.layer)) {
    p <- ggplot()
  } else {
    p <- data.layer
  }

  p <- p +

    # Plot map outline and project to polar coordinates
    geom_polygon(data = map.outline, aes(x = long, y = lat, group = group),
                 fill = land.fill.colour, colour = country.outline.colour) +

    coord_map("ortho", orientation = c(ifelse(pole == "N", 90, -90),
                                       rotate.to,
                                       0),
              #ylim = sort(c(max.lat*0.8, min.lat*0.8)),
              xlim = c(min.lon, max.lon)
              ) +

    # Remove axes and labels
    scale_x_continuous("", breaks = NULL) +
    scale_y_continuous("", breaks = NULL) +

    # Add axes and labels


    # Longitude axis lines and labels
    geom_segment(aes(y = long.line.strt, yend = long.line.end,
                     x = long.ax.vals, xend = long.ax.vals), linetype = "dashed",
                 colour = "black", size = 0.25) +
    geom_text(aes(x = long.ax.vals, y = long.lab.pos.2, label = long.ax.labs)) +

    # Outer latitude axis
    geom_line(aes(y = outer.lat.ax.val, x = min.lon : max.lon), size = size.outer,
              colour = "black") +

    # Lat axis lines
    geom_line(data = lat.lines, aes(y = lat, x = long, group = lat),
              size = 0.25, linetype = "dashed", colour = "black") +
    # Lat axis labels
    geom_label(aes(x = mean.lon, y = lat.ax.vals, label = lat.ax.labs),
               #hjust = -0.2,
               alpha = 0.75, label.size = 0) +

    # Change theme to remove panel backgound
    theme(panel.background = element_blank())

  # If segment add lines to edge
  if (is.segment){
    p <- p + geom_segment(aes(y = long.line.strt, yend = long.line.end,
                              x = c(min.lon, max.lon), xend = c(min.lon, max.lon)),
                          colour = "black", size = size.outer)
  }

  p
}




# # ## DUMMY Data
# library(ggplot2)
# lat <- seq(87.5, -87.5, -2.5)
# lon <- seq(2.5, 360, 2.5)
# dat <- matrix(rnorm(length(lat) * length(lon)),
#               ncol = length(lat), nrow = length(lon))
#
# colnames(dat) <- lat
# rownames(dat) <- lon
#
# dat.vec <- reshape2::melt(dat)
# colnames(dat.vec)[1:2] <- c('lon','lat')
#
# dat.vec.sub <- subset(dat.vec, lat >= 55)
# #dat.vec.sub <- dat.vec.sub[sample(nrow(dat.vec.sub), 100) ,]
#
# p + geom_tile(data = dat.vec.sub, aes(x = lon, y = lat, z = value, fill = value), alpha = 0.5)






