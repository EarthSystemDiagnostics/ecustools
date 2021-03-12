#' Plot a ggplot map in orthographic projection 
#'
#' @param centre.lat Centre map on this latitude
#' @param centre.lon Centre map on this longitude
#' @param lat.ax.vals Optionally specify values for latitude labels
#' @param long.ax.vals Optionally specify values for longitude labels 
#' @param lat.ax.labs.pos Specify the line of longitude on which the latitude
#'  labels will be printed
#' @param ocean.fill Specify the colour of the ocean
#' @param land.fill Specify the colour of the land
#' @param border.colour Specify the colour of the land borders
#' @param lon.lat.colour Colour of the longitude and latitude lines
#' @param lon.lat.linetype Linetype of the longitude and latitude lines
#' @param bury.lon.lat.lines Hide longitude and latitude lines under land
#' @param plot.visible Return a rectangular projection showing the area that 
#' will be visible in the orthographic plot
#' 
#' @details Adapted from code here:
#' https://gist.github.com/fzenoni/ef23faf6d1ada5e4a91c9ef23b0ba2c1
#' @return ggplot object
#' @export
#'
#' @examples
#'# Centre on 90 degrees East, 45 degrees north
#'p <- ggortho(centre.lat = 45, centre.lon = 90,
#'             lat.ax.vals = seq(-90, 90, 30))
#'p
#'
#'# Hide lon lat lines on land
#'p <- ggortho(centre.lat = 45, centre.lon = 90,
#'             lat.ax.vals = seq(-90, 90, 30),
#'             bury.lon.lat.lines = TRUE)
#'p
#'
#'
#'
#'# Plot Antarctica and add a point
#'p <- ggortho(centre.lat = -90, centre.lon = 0,
#'             lat.ax.vals = seq(-90, 90, 30),
#'             lat.ax.labs.pos = 60)
#'
#'test.dat <-
#'  data.frame(lat = c(-80), lon = c(100))
#'
#'test.dat.sf <- sf::st_as_sf(test.dat, coords = c("lon", "lat")) %>%
#'  sf::st_set_crs(4326)
#'
#'p + ggplot2::geom_sf(data = test.dat.sf, colour = "Red")
#'
#'
#'# change the colour of the land
#'
#'p <- ggortho(centre.lat = -30, centre.lon = -120,
#'             lat.ax.vals = seq(-90, 90, 30),
#'             #lat.ax.labs.pos = 60,
#'             plot.visible = FALSE,
#'             land.fill = "Darkgreen")
#'
#'test.dat <-
#'  data.frame(lat = c(-35:-36), lon = c(-120:-121))
#'
#'test.dat.sf <- sf::st_as_sf(test.dat, coords = c("lon", "lat")) %>%
#'  sf::st_set_crs(4326)
#'
#'p + ggplot2::geom_sf(data = test.dat.sf, colour = "Red")
ggortho <- function(centre.lat, centre.lon,
                    lat.ax.vals = NULL, long.ax.vals = NULL,
                    lat.ax.labs.pos = NULL,
                    ocean.fill = "aliceblue",
                    land.fill = "grey92",
                    border.colour = "grey55",
                    lon.lat.colour = "grey55",
                    lon.lat.linetype = 4,
                    bury.lon.lat.lines = FALSE,
                    plot.visible = FALSE){

  suppressMessages({
    # Read the data
    mini_world <- rnaturalearth::ne_countries(scale = 'small', returnclass = 'sf')

    # Define the orthographic projection ----
    # Choose lat_0 with -90 <= lat_0 <= 90 and lon_0 with -180 <= lon_0 <= 180

    ortho <- paste0('+proj=ortho +lat_0=', centre.lat, ' +lon_0=', centre.lon, ' +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs')

    # Define the polygon that will help you finding the "blade"
    # to split what lies within and without your projection
    circle <- sf::st_point(x = c(0,0)) %>% sf::st_buffer(dist = 6371000) %>% sf::st_sfc(crs = ortho)

    # Project this polygon in lat-lon
    circle_longlat <- circle %>% sf::st_transform(crs = 4326)

    # circle_longlat cannot be used as it is
    # You must decompose it into a string with ordered longitudes
    # Then complete the polygon definition to cover the hemisphere
    if(centre.lat != 0) {
      circle_longlat <- sf::st_boundary(circle_longlat)

      circle_coords <- sf::st_coordinates(circle_longlat)[, c(1,2)]
      circle_coords <- circle_coords[order(circle_coords[, 1]),]
      circle_coords <- circle_coords[!duplicated(circle_coords),]

      # Rebuild line
      circle_longlat <- sf::st_linestring(circle_coords) %>% sf::st_sfc(crs = 4326)

      if(centre.lat > 0) {
        rectangle <- list(rbind(circle_coords,
                                c(X = 180, circle_coords[nrow(circle_coords), 'Y']),
                                c(X = 180, Y = 90),
                                c(X = -180, Y = 90),
                                c(X = -180, circle_coords[1, 'Y']),
                                circle_coords[1, c('X','Y')])) %>%
          sf::st_polygon() %>% sf::st_sfc(crs = 4326)
      } else {
        rectangle <- list(rbind(circle_coords,
                                c(X = 180, circle_coords[nrow(circle_coords), 'Y']),
                                c(X = 180, Y = -90),
                                c(X = -180, Y = -90),
                                c(X = -180, circle_coords[1, 'Y']),
                                circle_coords[1, c('X','Y')])) %>%
          sf::st_polygon() %>% sf::st_sfc(crs = 4326)
      }

      circle_longlat <- sf::st_union(sf::st_make_valid(circle_longlat), sf::st_make_valid(rectangle))
    }

    if (plot.visible)  {
      # Visible hemisphere in red ----
      pv <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = mini_world) +
        ggplot2::geom_sf(data = circle_longlat, color = 'red', fill = 'red', alpha = 0.3) +
        ggplot2::theme_bw()
      
      return(pv)
    }

    # A small negative buffer is necessary to avoid polygons still disappearing in a few pathological cases
    # I should not change the shapes too much
    visible <- sf::st_intersection(sf::st_make_valid(mini_world), sf::st_buffer(circle_longlat, -0.09)) %>%
      sf::st_transform(crs = ortho)

    # DISCLAIMER: This section is the outcome of trial-and-error and I don't claim it is the best approach
    # Resulting polygons are often broken and they need to be fixed
    # Get reason why they're broken
    broken_reason <- sf::st_is_valid(visible, reason = TRUE)

    # First fix NA's by decomposing them
    # Remove them from visible for now
    na_visible <- visible[is.na(broken_reason),]
    visible <- visible[!is.na(broken_reason),]

    # Open and close polygons
    na_visible <- sf::st_cast(na_visible, 'MULTILINESTRING') %>%
      sf::st_cast('LINESTRING', do_split=TRUE)
    na_visible <- na_visible %>% dplyr::mutate(npts = mapview::npts(geometry, by_feature = TRUE))
    # Exclude polygons with less than 4 points
    na_visible <- na_visible %>%
      dplyr::filter(npts >=4) %>%
      dplyr::select(-npts) %>%
      sf::st_cast('POLYGON')

    # Fix other broken polygons
    broken <- which(!sf::st_is_valid(visible))
    for(land in broken) {
      result = tryCatch({
        # visible[land,] <- st_buffer(visible[land,], 0) # Sometimes useful sometimes not
        visible[land,] <- sf::st_make_valid(visible[land,]) %>%
          sf::st_collection_extract()
      }, error = function(e) {
        visible[land,] <<- sf::st_buffer(visible[land,], 0)
      })
    }

    # Bind together the two tables
    visible <- rbind(visible, na_visible)

    ## My changes from here:
    ## To fix graticule, don't plot it - add your own lat lon lines.


    if (is.null(long.ax.vals)){
      long.ax.vals <- seq(-180, 180, 30)
    }


    lon.lines <- expand.grid(x = long.ax.vals, y = seq(-90, 90, 1)) %>%
      dplyr::mutate(id = as.character(x)) %>%
      dplyr::group_by(id) %>%
      sf::st_as_sf(., coords = c("x","y")) %>%
      sf::st_set_crs(4326) %>%
      dplyr::summarise() %>%
      sf::st_cast("LINESTRING")

    visible.lon.lines <- sf::st_intersection(sf::st_make_valid(lon.lines), sf::st_buffer(circle_longlat, -0.09)) %>%
      sf::st_transform(crs = ortho)


    visible.lon.lines.deg <- sf::st_intersection(sf::st_make_valid(lon.lines), sf::st_buffer(circle_longlat, -0.09))

    # Get endpoints of the long.lines to use to position long labels
    if (centre.lat >= 0){
      long.ax.coords <- t(sapply(1:(length(long.ax.vals)-1), function(i)
        attributes(visible.lon.lines.deg$geometry[i])$bbox[c("xmin", "ymin")]))
      colnames(long.ax.coords) <- c("x", "y")
    }else if(centre.lat < 0){
      long.ax.coords <- t(sapply(1:(length(long.ax.vals)-1), function(i)
        attributes(visible.lon.lines.deg$geometry[i])$bbox[c("xmin", "ymax")]))
      colnames(long.ax.coords) <- c("x", "y")
    }


    FormatLongLabs <- function(long){

      long <- round(long)

      long.ax.labs <- paste0(abs(long), "°",
                             ifelse(long <= 0, " W", " E"))

      long.ax.labs[long == 0] <- "0°"
      long.ax.labs[long == 180] <- "180° W"
      return(long.ax.labs)
    }


    long.ax <- long.ax.coords %>%
      data.frame(.) %>%
      dplyr::filter(stats::complete.cases(x)) %>%
      dplyr::mutate(labs = FormatLongLabs(x)) %>%
      sf::st_as_sf(., coords = c("x","y")) %>%
      sf::st_set_crs(4326)


    if (is.null(lat.ax.vals)){
      lat.ax.vals <- seq(-90, 90, 15)
    }

    lat.ax.labs <- paste0(ifelse(lat.ax.vals < 0, -lat.ax.vals, lat.ax.vals),
                          "°", ifelse(lat.ax.vals < 0, " S", " N"))

    if (is.null(lat.ax.labs.pos)){
      lat.ax.labs.pos <- centre.lon - abs(diff(long.ax.vals)[1]/2)
    }



    lat.ax <- expand.grid(x = lat.ax.labs.pos, y = lat.ax.vals) %>%
      sf::st_as_sf(., coords = c("x","y")) %>%
      sf::st_set_crs(4326)


    lat.lines <- expand.grid(x = -180:180, y = lat.ax.vals) %>%
      dplyr::mutate(id = as.character(y)) %>%
      dplyr::group_by(id) %>%
      sf::st_as_sf(., coords = c("x","y")) %>%
      sf::st_set_crs(4326) %>%
      dplyr::summarise() %>%
      sf::st_cast("LINESTRING")

    visible.lat.lines <- sf::st_intersection(sf::st_make_valid(lat.lines),
                                         sf::st_buffer(circle_longlat, -0.09)) %>%
      sf::st_transform(crs = ortho)



    # Project outer circle in lat-lon
    circ_longlat <- circle %>% sf::st_transform(crs = 4326)


  })


  # Final plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = circle,
            fill = ocean.fill) +
    ggplot2::geom_sf(data=sf::st_collection_extract(visible),
            fill = land.fill, colour = border.colour) +
    #coord_sf(crs = ortho) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.background = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank()) +
    ggplot2::geom_sf(data = circ_longlat, fill = NA) +
    ggplot2::geom_sf(data = visible.lat.lines, colour = lon.lat.colour, linetype = lon.lat.linetype) +
    ggplot2::geom_sf(data = visible.lon.lines, colour = lon.lat.colour, linetype = lon.lat.linetype) +
    ggplot2::geom_sf_label(data = lat.ax, ggplot2::aes(label = lat.ax.labs),
                  label.size = 0, alpha = 0.75, label.r = grid::unit(0.5, "lines")) +
    ggplot2::geom_sf_label(data = long.ax, ggplot2::aes(label = labs),
                  label.size = 0, alpha = 0.75, label.r = grid::unit(0.5, "lines"))

  # plot(p)

  if (bury.lon.lat.lines) p$layers <- p$layers[c(1,4,5,2,3,6,7)]

  return(p)
}



