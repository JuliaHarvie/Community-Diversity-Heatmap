library(sf)
library(sp)
library(raster)
library(rgdal)
#how to make a polygon
n <- 10
xx <- c(0:n, n:0)
yy <- c(c(0, cumsum(stats::rnorm(n))), rev(c(0, cumsum(stats::rnorm(n)))))
plot  (xx, yy, type = "n", xlab = "Time", ylab = "Distance")
#plots on graph, doesnt make anything 
polygon <- polygon(xx, yy, col = "gray", border = "red")
class(polygon)

#make polygon usign sp package
#polygons will assign an object of class spatialpolygons to a dataframe
grd <- GridTopology(c(1,1), c(1,1), c(4,4))
polys_from_grid <- as.SpatialPolygons.GridTopology(grd)
#converting a grd into spatial polygon seems to give me
#level 1 (a spatial polygon) just connect dataframe
#can us to build outline and change as needed

level_1 <- polys_from_grid
level_2 <- level_1@polygons # this is a list
level_2[[1]] #displays "level 4" data
level_2[[1]]@area #now can edit level 4 data
level_1@polygons[[1]]@area # this also works, dont actually need to renamem

## how to export
grd <- GridTopology(c(1,1), c(1,1), c(4,4))
polys_from_grid <- as.SpatialPolygons.GridTopology(grd)
level_1 <- polys_from_grid
Basic <- as(level_1, "SpatialPolygons")
coordinates <- coordinates(Basic)
x <- coordinates[,1]
y <- coordinates[,2]
z <- x+y
Basic2 <- SpatialPolygonsDataFrame(Basic,data=data.frame(x=x, y=y, z=z))
Shape <- st_as_sf(Basic2, coords=c("X","Y"))
st_write(Shape, "Basic5.shp")


four <- GridTopology(c(2,2), c(2,2), c(2,2))
poly_four <- as(four, "SpatialPolygons")
coordinates <- coordinates(poly_four)
x <- coordinates[,1]
y <- coordinates[,2]
z <- c(4,5,6,NA)
shape_four <- SpatialPolygonsDataFrame(poly_four,data=data.frame(x=x, y=y, z=z))
Shape <- st_as_sf(shape_four, coords=c("X","Y"))
st_write(Shape, "NAs.shp")

## editing poly_four
## want to move plot order 4 and 1 over, which is [3], [4]
poly_four@polygons[[1]]@area
edit <- poly_four
# does change 3
edit@polygons[[3]]@coords <- c(4,2)

edit@polygons[[3]]@Polygons
extract <- edit@polygons[[3]]                  
class(extract)

### 10 by 10  
ten <- GridTopology(c(1,1), c(1,1), c(10,10))
poly_ten <- as(ten, "SpatialPolygons")
coordinates <- coordinates(poly_ten)
x <- coordinates[,1]
y <- coordinates[,2]
z <- c(rep(7,times=100))
shape_ten <- SpatialPolygonsDataFrame(poly_ten,data=data.frame(x=x, y=y, z=z))
Shape <- st_as_sf(shape_ten, coords=c("X","Y"))
st_write(Shape, "TEN_NA.shp")

### Odd shape 
ten <- GridTopology(c(1,1), c(1,1), c(4,4))
poly_ten <- as(ten, "SpatialPolygons")
coordinates <- coordinates(poly_ten)
x <- coordinates[,1] +10
y <- coordinates[,2] +10
z <- c(rep(13,times=16))
shape_ten <- SpatialPolygonsDataFrame(poly_ten,data=data.frame(x=x, y=y, z=z))
Shape <- st_as_sf(shape_ten, coords=c("X","Y"))
st_write(Shape, "plus10.shp")

cbind(x,y)
x_edit <- c(1,2,3,1,2,3,1,2,3,2,3,2,3,2,3,4)
y_edit <- c(6,6,6,5,5,5,4,4,4,3,3,2,2,1,1,1)


### attachign coordinates
four <- GridTopology(c(1,1), c(1,1), c(4,4))
poly_four <- as(four, "SpatialPolygons")
coordinates <- coordinates(poly_four)
x <- coordinates[,1]
y <- coordinates[,2]
z <- c(rep(7,times=16))
lat <- x+10
long <- y+10
shape_four <- SpatialPolygonsDataFrame(poly_four,data=data.frame(x=x, y=y, z=z))
Shape_crs <- st_as_sf(shape_four, coords=c("x","y"), crs= crs(r))
st_write(Shape_crs, "lat_long2.shp")

good <- readOGR(file.choose())
readOGR()
good[1]
good[2]
city <- readOGR(file.choose())
city[[1]]
class(city)
class(shape_four)
head(city)


r <- raster()
crs(r)
crs(r) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"
crs(r)
sf::st_write()
