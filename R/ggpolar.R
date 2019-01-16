#' @title Nicely formatted polar ggplot
#' @description Produce a nicely formatted map of the Arctic or Antarctic in
#' polar projection. Adapted from Mikey Harper's plot
#' https://stackoverflow.com/a/49084793
#' @param pole which pole N or S?
#' @param max.lat maximum latitude
#' @param min.lat minimum latitude
#' @param longitude.spacing interval between longitude axis tick marks
#' @param land.fill.colour colour to shade the land
#' @param n.lat.labels approximate number of latitude tickmarks
#' @param nearest.x.degrees round latitude tickmarks to how many degrees?
#' @import ggplot2 maptools
#' @importFrom raster crop extent
#' @author Andrew Dolman <andrew.dolman@awi.de>
#' @export
#' @examples
#' ggpolar(pole = "N", max.lat = 90, min.lat = 55, n.lat.labels = 4)
#' ggpolar(pole = "S", max.lat = -60, min.lat = -90)
#' \dontrun{
#' ggpolar(pole = "W", max.lat = -55, min.lat = -90)
#' ggpolar(pole = "S", max.lat = 90, min.lat = 55)
#' ggpolar(pole = "S", max.lat = -90, min.lat = -55)
#' }

ggpolar <- function(pole = c("N", "S"), max.lat, min.lat,
                    longitude.spacing = 60,
                    land.fill.colour = "Grey",
                    n.lat.labels = 4, nearest.x.degrees = 5) {

  pole <- match.arg(pole, choices = c("N", "S"))

  if (pole == "N" & max.lat < 0) stop("If pole == N, max.lat should be positive")
  if (pole == "S" & min.lat > 0) stop("If pole == S, min.lat should be negative")

  stopifnot(max.lat > min.lat)

  lat.ends <- c(min.lat, max.lat)

  d.lat.ax.vals <- nearest.x.degrees * round((max.lat - min.lat)/n.lat.labels/nearest.x.degrees)

  # Hemishphere specific values
  if (pole == "N") {
    lat.ax.vals <- seq(min.lat + d.lat.ax.vals, max.lat,
                       by = d.lat.ax.vals)
    outer.lat.ax.val <- min.lat
    long.lab.pos.1 <- outer.lat.ax.val - d.lat.ax.vals/3
    long.lab.pos.2 <- outer.lat.ax.val - d.lat.ax.vals/2

    long.line.strt <- max.lat
    long.line.end <- long.lab.pos.1

  } else if (pole == "S") {
    lat.ax.vals <- seq(max.lat - d.lat.ax.vals, min.lat,
                       by = -d.lat.ax.vals)
    outer.lat.ax.val <- max.lat
    long.lab.pos.1 <- outer.lat.ax.val + d.lat.ax.vals/3
    long.lab.pos.2 <- outer.lat.ax.val + d.lat.ax.vals/2

    long.line.strt <- long.lab.pos.1
    long.line.end <- min.lat
  }

  lat.ax.labs <- paste0(lat.ax.vals, "°", ifelse(lat.ax.vals <
                                                   0, "S", "N"))

  # Defines the x axes required
  long.ax.vals <- seq(-180 + longitude.spacing, 180, by = longitude.spacing)
  long.ax.labs <- paste0(abs(long.ax.vals), "°", ifelse(long.ax.vals <=
                                                          0, "W", "E"))
  long.ax.labs[long.ax.vals == 0] <- "0°"
  long.ax.labs[long.ax.vals == 180] <- "180°W"

  lat.lines <- expand.grid(long = -180:180, lat = lat.ax.vals)

  # Get map outline and crop
  data("wrld_simpl", package = "maptools")
  map.outline <- raster::crop(wrld_simpl,
                              raster::extent(-180, 180, min.lat, max.lat),
                              snap = "in")

  p <- ggplot() +

    # Plot map outline and project to polar coordinates
    geom_polygon(data = map.outline, aes(x = long, y = lat, group = group),
                 fill = land.fill.colour, colour = "black", alpha = 0.8) +

    coord_map("ortho", orientation = c(ifelse(pole == "N", 90, -90), 0, 0)) +

    # Remove axes and labels
    scale_x_continuous("", breaks = NULL) +
    scale_y_continuous("", breaks = NULL) +

    # Add axes and labels

    # Longitude axis lines and labels
    geom_segment(aes(y = long.line.strt, yend = long.line.end,
                     x = long.ax.vals, xend = long.ax.vals), linetype = "dashed",
                 colour = "black", size = 0.25) +
    geom_text(aes(x = long.ax.vals, y = long.lab.pos.2, label = long.ax.labs)) +

    # Outer longitude axis
    geom_line(aes(y = outer.lat.ax.val, x = -180:180), size = 1,
              colour = "black") +

    # Lat axis lines
    geom_line(data = lat.lines, aes(y = lat, x = long, group = lat),
              size = 0.25, linetype = "dashed", colour = "black") +
    # Lat axis labels
    geom_label(aes(x = 180, y = lat.ax.vals, label = lat.ax.labs),
               hjust = -0.2, alpha = 0.5) +

    # Change theme to remove panel backgound
    theme(panel.background = element_blank())

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

