# load required libraries
library(sp)
library(maps)
library(rgeos)

# set colours for map grid
grid.col.light = rgb(0.5,0.5,0.5,0.8)
grid.col.dark = rgb(0.5,0.5,0.5)

# coordinate systems
#polar = CRS("+init=epsg:3995")
polar = CRS("+init=epsg:3031")
longlat = CRS("+init=epsg:4326")

# download the blue marble data if it doesn't
# exist
if (!file.exists("blue_marble.tif")) {
  download.file("http://neo.sci.gsfc.nasa.gov/servlet/RenderData?si=526312&cs=rgb&format=TIFF&width=5400&height=2700","blue_marble.tif")
}

# read in the raster map and
# set the extent, crop to extent and reproject to polar
lat_max = -60
lat_min = -90
r = raster::brick("blue_marble.tif")
e = raster::extent(c(-180,180,lat_min,lat_max))
r_crop = raster::crop(r,e)

# traps NA values and sets them to 1
r_crop[is.na(r_crop)] = 1
r_polar = raster::projectRaster(r_crop, crs = polar, method = "bilinear")

# some values are not valid after transformation
# (rgb range = 1 - 255) set these back to 1
# as they seem to be the black areas
r_polar[r_polar < 1 ] = 1

# define the graticule / grid lines by first specifying
# the larger bounding box in which to place them, and
# feeding this into the sp() gridlines function
# finally the grid lines are transformed to
# the EPSG 3995 projection
pts=SpatialPoints(rbind(c(-180,lat_min),c(0,lat_min),c(180,lat_max),c(180,lat_max)), CRS("+init=epsg:4326"))
gl = gridlines(pts, easts = seq(-180, 180, 30), norths = seq(-50,-85,-10), ndiscr = 100)
gl.polar = spTransform(gl, polar)

# I also create a single line which I use to mark the
# edge of the image (which is rather unclean due to pixelation)
# this line sits at 55 degrees North similar to where I trimmed
# the image
pts=SpatialPoints(rbind(c(-180,lat_min),c(0,lat_min),c(180,80),c(180,80)), CRS("+init=epsg:4326"))
my_line = SpatialLines(list(Lines(Line(cbind(seq(-180,180,0.5),rep(lat_min,721))), ID="outer")), CRS("+init=epsg:4326"))

# crop a map object (make the x component a bit larger not to exclude)
# some of the eastern islands (the centroid defines the bounding box)
# and will artificially cut of these islands
m = maps2sp(c(-180,200),c(lat_min,lat_max),clip = TRUE)

#----- below this point is the plotting routine
# set margins to let the figure "breath" and accommodate labels
par(mar=rep(1,4))

# plot the grid, to initiate the area
# plotRGB() overrides margin settings in default plotting mode
plot(spTransform(gl, polar), lwd=2, lty=2,col="white")

# plot the blue marble raster data
raster::plotRGB(r_polar, add = TRUE)

# plot grid lines / graticule
lines(spTransform(gl, polar), add = TRUE, lwd=2, lty=2,col=grid.col.light)

# plot outer margin of the greater circle
lines(spTransform(ll, polar), lwd = 3, lty = 1, col=grid.col.dark)

# plot continent outlines, for clarity
plot(spTransform(m, polar), lwd = 1, lty = 1, col = "transparent", border=grid.col.dark, add = TRUE)

# plot longitude labels
l = labels(gl.polar, crs.longlat, side = 1)
l$pos = NULL
text(l, cex = 1, adj = c( 0.5, 2 ),  col = "black")

# plot latitude labels
l = labels(gl.polar, crs.longlat, side = 2)
l$srt = 0
l$pos = NULL
text(l, cex = 1, adj = c(1.2, -1), col = "white")

# After all this you can plot your own site locations etc
# but don't forget to tranform the data from lat / long
# into the arctic polar stereographic projection using
# spTransform()





######

# Clean antarctic lines with base graphics
# from https://edzer.github.io/sp/

library(sp)
library(maptools)
library(rgdal)

maps2sp = function(xlim, ylim, l.out = 100, clip = TRUE) {
  stopifnot(require(maps))
  m = map(xlim = xlim, ylim = ylim, plot = FALSE, fill = TRUE)
  p = rbind(cbind(xlim[1], seq(ylim[1],ylim[2],length.out = l.out)),
            cbind(seq(xlim[1],xlim[2],length.out = l.out),ylim[2]),
            cbind(xlim[2],seq(ylim[2],ylim[1],length.out = l.out)),
            cbind(seq(xlim[2],xlim[1],length.out = l.out),ylim[1]))
  LL = CRS("+init=epsg:4326")
  bb = SpatialPolygons(list(Polygons(list(Polygon(list(p))),"bb")), proj4string = LL)
  IDs <- sapply(strsplit(m$names, ":"), function(x) x[1])
  stopifnot(require(maptools))
  m <- map2SpatialPolygons(m, IDs=IDs, proj4string = LL)
  if (!clip)
    m
  else {
    stopifnot(require(rgeos))
    gIntersection(m, bb) # cut map slice in WGS84
  }
}

crs.longlat = CRS("+init=epsg:4326")

par(mar = c(0, 0, 1, 0))

m = maps2sp(xlim = c(-180,180), ylim = c(-90,-70), clip = FALSE)
gl = gridlines(m, easts = seq(-180,180,20))
polar = CRS("+init=epsg:3031")
gl.polar = spTransform(gl, polar)
plot(as(gl.polar, "Spatial"), expandBB = c(.05, 0, .05, 0))
plot(gl.polar, add = TRUE)
plot(spTransform(m, polar), add = TRUE, col = grey(0.8, 0.8))
l = labels(gl.polar, crs.longlat, side = 3)
# pos is too simple here, use adj:
l$pos = NULL
text(l, adj = c(0.5, -0.3), cex = .8)
l = labels(gl.polar, crs.longlat, side = 2)
l$srt = 0 # otherwise they are upside-down
text(l, cex = .8)
title("grid line labels on polar projection, epsg 3031")


### With a raster background
  # would need to desaturate blues

# from http://www.khufkens.com/2017/01/18/r-polar-plots/

# coordinate systems
# polar = CRS("+init=epsg:3995")
polar = CRS("+init=epsg:3031")
longlat = CRS("+init=epsg:4326")

# download the blue marble data if it doesn't
# exist
if (!file.exists("blue_marble.tif")) {
  download.file("http://neo.sci.gsfc.nasa.gov/servlet/RenderData?si=526312&cs=rgb&format=TIFF&width=5400&height=2700","blue_marble.tif")
}

# read in the raster map and
# set the extent, crop to extent and reproject to polar
lat_max = -64
lat_min = -90
r = raster::brick("blue_marble.tif")
e = raster::extent(c(-180,180,lat_min,lat_max))
r_crop = raster::crop(r,e)

# traps NA values and sets them to 1
r_crop[is.na(r_crop)] = 1
r_polar = raster::projectRaster(r_crop, crs = polar, method = "bilinear")

# some values are not valid after transformation
# (rgb range = 1 - 255) set these back to 1
# as they seem to be the black areas
r_polar[r_polar < 1 ] = 1


# Plot as before but with raster
par(mar = c(0, 0, 1, 0))

m = maps2sp(xlim = c(-180,180), ylim = c(-90,-70), clip = FALSE)
gl = gridlines(m, easts = seq(-180,180,20))
gl.polar = spTransform(gl, polar)
plot(as(gl.polar, "Spatial"), expandBB = c(.05, 0, .05, 0))
raster::plotRGB(r_polar, add = TRUE)
plot(gl.polar, add = TRUE, col = "Red")
#plot(spTransform(m, polar), add = TRUE, col = grey(0.8, 0.8))
l = labels(gl.polar, crs.longlat, side = 3)
# pos is too simple here, use adj:
l$pos = NULL
text(l, adj = c(0.5, -0.3), cex = .8)
l = labels(gl.polar, crs.longlat, side = 2)
l$srt = 0 # otherwise they are upside-down
text(l, cex = .8, col = "Red")
title("grid line labels on polar projection, epsg 3031")


