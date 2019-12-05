X <- NA
Y <- NA
V <- NA
for (y in 1:DY){
  for(x in 1:DX){
    if(is.na(complete[[1]][y,x])){
      NULL
    } else {
      X <- c(X,x)
      Y <- c(Y,y)
      V <- c(V,complete[[1]][y,x])
    }
  }
}


show(matrix(1:24,4,6))
complete[[1]][1:5,1:55]

complete[[1]][DY,DX]


shape <- data.frame(X=X,Y=Y,V=V)
write.csv(shape,"shape.csv")


install.packages("raster")
install.packages("sf")
library(raster)
library(sf)
library(sp)
library(rgdal)
install.packages("sp")

shape <- read.csv(file.choose())
head(shape)
class(shape)
shape2 <- st_as_sf(shape, coords=c("X","Y"))
class(shape2)
head(shape2)
st_crs(shape2)
show(shape2)
tail(shape2)
st_write(shape2, "shape2.shp")

library(GISTools)
library(rgdal)
data <- data(tornados)
summary(torn)
head(torn)
head(us_states)
summary(us_states)
nrow(torn)
head(torn[,1:4])
tail(torn[,1:4])

summary(us_states)
show(us_states[1,1:4])
library(sp)
coords <- (cbind(c(1,1,4,4,1),c(1,4,4,1,1)))       
show(coords)
ploy <- Polygon(coords, hole=as.logical(NA))
show(ploy)

Sr1 = Polygon(cbind(c(2,4,4,1,2),c(2,3,5,4,2)))
Sr2 = Polygon(cbind(c(5,4,2,5),c(2,3,2,2)))
Sr3 = Polygon(cbind(c(4,4,5,10,4),c(5,3,2,5,5)))
Sr4 = Polygon(cbind(c(5,6,6,5,5),c(4,4,3,3,4)), hole = TRUE)

Srs1 = Polygons(list(Sr1), "s1")
Srs2 = Polygons(list(Sr2), "s2")
Srs3 = Polygons(list(Sr3, Sr4), "s3/4")
SpP = SpatialPolygons(list(Srs1,Srs2,Srs3), 1:3)
plot(SpP, col = 1:3, pbg="white")

grd <- GridTopology(c(1,1), c(1,1), c(10,10))
polys <- as(grd, "SpatialPolygons")
plot(polys)
text(coordinates(polys), labels=row.names(polys))
centroids <- coordinates(polys)
x <- centroids[,1]
y <- centroids[,2]
z <- 1.4 + 0.1*x + 0.2*y + 0.002*x*x
ex_1.7 <- SpatialPolygonsDataFrame(polys,data=data.frame(x=x, y=y, z=z))
brks <- quantile(z, seq(0,1,1/7))
cols <- grey((length(brks):2)/length(brks))
dens <- (2:length(brks))*3
plot(ex_1.7, col=cols[findInterval(z, brks, all.inside=TRUE)])
plot(ex_1.7, density=dens[findInterval(z, brks, all.inside=TRUE)])

poly1 <- Polygon(cbind(c(1,1,4,4,1),c(1,4,4,1,1)))
poly2 <- Polygon(cbind(c(4,4,8,8,4),c(4,8,8,4,4)))
Joint <- Polygons(list(poly1,poly2), "s1")

write.table(ex_1.7, "dataframe.txt")
example <- ex_1.7
class(example)

st_write(example, "example.shp")

shape <- read.csv(file.choose())
head(shape)
class(shape)
shape2 <- st_as_sf(shape, coords=c("X","Y"))
class(shape2)
head(shape2)
st_crs(shape2)
show(shape2)
tail(shape2)
st_write(example3, "example3.shp")
example3 <- st_as_sf(example, coords=c("X","Y"))
