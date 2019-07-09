## how to transform data into shape file

library(sf)
library(sp)

x <- cbind(c(30,30,40,40,30),c(30,40,40,30,30))
x1 <- cbind(c(40,50,50,40,40),c(40,40,50,50,40))
x2 <- cbind(c(40,40,50,50,40),c(30,40,40,30,30))

#each value is assigned to a polygon
#use Polygon function to outline one cube of matrix an put in in the Polygons function
#(ID numbers dont seem to matter)
poly1 <- Polygons(list(Polygon(x)),"ID1")
poly2 <- Polygons(list(Polygon(x1)), "ID2")
poly3 <- Polygons(list(Polygon(x2)), "ID3")

#All of these Polygons functions are combind in a list
D <- list(poly1,poly2,poly3)

#Convert to Spatial Polygons
C <- SpatialPolygons(D)

#Get the coordinates from C and convert them into x and y coordinate vectors
coordinates <- coordinates(C)
xc <- coordinates[,1]
yc <- coordinates[,2]

#make a vector with the values, the order the polygons are in the list is the order their values
#should be inputted into the vector
L <- c(4,14,15)

#Turn all this into a data frame
B <- SpatialPolygonsDataFrame(C, data=data.frame(x=xc,y=yc,L=L))

#conver it to a sf object, this is where you include the coordinate reference system
A <- st_as_sf(B,coords = c("x","y"), crs = 4326, agr = "constant")

#export as shape file
st_write(A,"may5.shp")


#### reproducing
x <-cbind(c(10,10,20,20,10),c(40,60,60,40,40))
poly1 <- Polygons(list(Polygon(x)),"ID1")
D <- list(poly1)
C <- SpatialPolygons(D)
coordinates <- coordinates(C)
xc <- coordinates[,1]
yc <- coordinates[,2]
L <- c(4)
B <- SpatialPolygonsDataFrame(C, data=data.frame(x=xc,y=yc,L=L))
A <- st_as_sf(B,coords = c("x","y"), crs = 4326, agr = "constant")
st_write(A,"may5_small.shp")

x <-cbind(c(-80.1828,-80.1828,-80.1756,-80.1756,-80.1828),c(43.5132,43.5204,43.5204,43.5132,43.5132))
poly1 <- Polygons(list(Polygon(x)),"ID1")
D <- list(poly1)
C <- SpatialPolygons(D)
coordinates <- coordinates(C)
xc <- coordinates[,1]
yc <- coordinates[,2]
L <- c(4)
B <- SpatialPolygonsDataFrame(C, data=data.frame(x=xc,y=yc,L=L))
A <- st_as_sf(B,coords = c("x","y"), crs = 4326, agr = "constant")
st_write(A,"may5_F.shp")
