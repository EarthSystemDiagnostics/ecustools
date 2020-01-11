# Adapted from here:
# https://gist.github.com/fzenoni/ef23faf6d1ada5e4a91c9ef23b0ba2c1

# Download earth data first
# https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_land.zip

library(sf)
library(lwgeom)
library(dplyr)
library(ggplot2)
library(mapview)
library(rnaturalearth)

# Read the data
#mini_world <- read_sf('/Users/andrewdolman/Dropbox/Work/AWI/Data/ne_110m_land/ne_110m_land.shp')

mini_world <- ne_countries(scale = 'small', returnclass = 'sf')

# Define the orthographic projection ----
# Choose lat_0 with -90 <= lat_0 <= 90 and lon_0 with -180 <= lon_0 <= 180
lat <- 60
lon <- 0
ortho <- paste0('+proj=ortho +lat_0=', lat, ' +lon_0=', lon, ' +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs')

# Define the polygon that will help you finding the "blade"
# to split what lies within and without your projection
circle <- st_point(x = c(0,0)) %>% st_buffer(dist = 6371000) %>% st_sfc(crs = ortho)

# Project this polygon in lat-lon
circle_longlat <- circle %>% st_transform(crs = 4326)

# circle_longlat cannot be used as it is
# You must decompose it into a string with ordered longitudes
# Then complete the polygon definition to cover the hemisphere
if(lat != 0) {
  circle_longlat <- st_boundary(circle_longlat)

  circle_coords <- st_coordinates(circle_longlat)[, c(1,2)]
  circle_coords <- circle_coords[order(circle_coords[, 1]),]
  circle_coords <- circle_coords[!duplicated(circle_coords),]

  # Rebuild line
  circle_longlat <- st_linestring(circle_coords) %>% st_sfc(crs = 4326)

  if(lat > 0) {
    rectangle <- list(rbind(circle_coords,
                            c(X = 180, circle_coords[nrow(circle_coords), 'Y']),
                            c(X = 180, Y = 90),
                            c(X = -180, Y = 90),
                            c(X = -180, circle_coords[1, 'Y']),
                            circle_coords[1, c('X','Y')])) %>%
      st_polygon() %>% st_sfc(crs = 4326)
  } else {
    rectangle <- list(rbind(circle_coords,
                            c(X = 180, circle_coords[nrow(circle_coords), 'Y']),
                            c(X = 180, Y = -90),
                            c(X = -180, Y = -90),
                            c(X = -180, circle_coords[1, 'Y']),
                            circle_coords[1, c('X','Y')])) %>%
      st_polygon() %>% st_sfc(crs = 4326)
  }

  circle_longlat <- st_union(st_make_valid(circle_longlat), st_make_valid(rectangle))
}

# Visible hemisphere in red ----
ggplot() +
  geom_sf(data = mini_world) +
  geom_sf(data = circle_longlat, color = 'red', fill = 'red', alpha = 0.3)

# A small negative buffer is necessary to avoid polygons still disappearing in a few pathological cases
# I should not change the shapes too much
visible <- st_intersection(st_make_valid(mini_world), st_buffer(circle_longlat, -0.09)) %>%
  st_transform(crs = ortho)

# DISCLAIMER: This section is the outcome of trial-and-error and I don't claim it is the best approach
# Resulting polygons are often broken and they need to be fixed
# Get reason why they're broken
broken_reason <- st_is_valid(visible, reason = TRUE)

# First fix NA's by decomposing them
# Remove them from visible for now
na_visible <- visible[is.na(broken_reason),]
visible <- visible[!is.na(broken_reason),]

# Open and close polygons
na_visible <- st_cast(na_visible, 'MULTILINESTRING') %>%
  st_cast('LINESTRING', do_split=TRUE)
na_visible <- na_visible %>% mutate(npts = npts(geometry, by_feature = TRUE))
# Exclude polygons with less than 4 points
na_visible <- na_visible %>%
  filter(npts >=4) %>%
  select(-npts) %>%
  st_cast('POLYGON')

# Fix other broken polygons
broken <- which(!st_is_valid(visible))
for(land in broken) {
  result = tryCatch({
    # visible[land,] <- st_buffer(visible[land,], 0) # Sometimes useful sometimes not
    visible[land,] <- st_make_valid(visible[land,]) %>%
      st_collection_extract()
  }, error = function(e) {
    visible[land,] <<- st_buffer(visible[land,], 0)
  })
}

# Bind together the two tables
visible <- rbind(visible, na_visible)


## My changes from here:
## To fix graticule, don't plot it - add your own lat lon lines.

lat.lines <- expand.grid(x = -180:180, y = seq(-90, 90, 15)) %>%
  mutate(id = as.character(y)) %>%
  group_by(id) %>%
  sf::st_as_sf(., coords = c("x","y")) %>%
  sf::st_set_crs(4326) %>%
  summarise() %>%
  st_cast("LINESTRING")

visible.lat.lines <- st_intersection(st_make_valid(lat.lines), st_buffer(circle_longlat, -0.09)) %>%
  st_transform(crs = ortho)


lon.lines <- expand.grid(x = seq(-179.9, 180, 30), y = seq(-90, 90, 1)) %>%
  mutate(id = as.character(x)) %>%
  group_by(id) %>%
  sf::st_as_sf(., coords = c("x","y")) %>%
  sf::st_set_crs(4326) %>%
  summarise() %>%
  st_cast("LINESTRING")

visible.lon.lines <- st_intersection(st_make_valid(lon.lines), st_buffer(circle_longlat, -0.09)) %>%
  st_transform(crs = ortho)


# Project this polygon in lat-lon
circ_longlat <- circle %>% st_transform(crs = 4326)


lat.ax.vals <- seq(-90, 90, 15)
lat.ax.labs <- paste0(ifelse(lat.ax.vals < 0, -lat.ax.vals, lat.ax.vals),
                      "°", ifelse(lat.ax.vals < 0, " S", " N"))
lat.ax.labs.pos <- -30

lat.ax <- expand.grid(x = lat.ax.labs.pos, y = lat.ax.vals) %>% 
  sf::st_as_sf(., coords = c("x","y")) %>%
  sf::st_set_crs(4326)

long.ax.vals <- seq(-180, 180, 30)

# circle_longlat.labs <- st_point(x = c(0,0)) %>% 
#   st_buffer(dist = 6372000) %>% 
#   st_sfc(crs = ortho) %>% 
#   st_transform(crs = 4326)

visible.lon.lines.deg <- st_intersection(st_make_valid(lon.lines), st_buffer(circle_longlat, -0.09))

long.ax.coords <- t(sapply(1:(length(long.ax.vals)-1), function(i) attributes(visible.lon.lines.deg$geometry[i])$bbox[c("xmin", "ymin")]))
colnames(long.ax.coords) <- c("x", "y")


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
  filter(complete.cases(x)) %>% 
  mutate(labs = FormatLongLabs(x)) %>% 
  sf::st_as_sf(., coords = c("x","y")) %>%
  sf::st_set_crs(4326)


# Final plot
p <- ggplot() +
  geom_sf(data = circle,
          fill = NA) +
  geom_sf(data = visible.lat.lines, colour = "Grey") +
  geom_sf(data = visible.lon.lines, colour = "Grey") +
  geom_sf(data=st_collection_extract(visible))+#, fill = "Darkgrey", colour = NA) +
  coord_sf(crs = ortho) +
  theme(panel.grid.major = element_blank(), panel.background = element_blank(), 
        axis.title = element_blank()) +
  geom_sf(data = circ_longlat, fill = NA)


p <- p + geom_sf_label(data = lat.ax, aes(label = lat.ax.labs), label.size = 0, alpha = 0.75, label.r = unit(0.5, "lines"))
p <- p + geom_sf_label(data = long.ax, aes(label = labs), label.size = 0, alpha = 0.75, label.r = unit(0.5, "lines"))
p
