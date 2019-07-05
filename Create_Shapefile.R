# create a point with coordinates
pt1 = st_point(c(0,1)) 
pt2 = st_point(c(1,1))
#attches them together into single object 
st_sfc(pt1, pt2)
#create dataframe 
d = data.frame(a = 1:2)
# adds column to dataframe that are the coordinate points
d$geom = st_sfc(pt1, pt2)
# switch dataframe to sf
df = st_as_sf(d)
# if you want wkt
d$geom = c("POINT(0 0)", "POINT(0 1)")
df = st_as_sf(d, wkt = "geom")
d$geom2 = st_sfc(pt1, pt2)
st_as_sf(d)

#meuse is a dataframe
data(meuse, package = "sp")
class(meuse)
head(meuse)
meuse_sf = st_as_sf(meuse, coords = c("x", "y"), crs = 28992, agr = "constant")
head(meuse_sf)
meuse_sf[1:3,]
summary(meuse_sf)

testdataframe <- data.frame("Twos" = c(2,4,6,8,10,12,14,16), "Threes" = c(3,6,9,12,15,18,21,24),
                            "Fives" = c(5,10,15,20,25,30,35,40))   
testdataframe_sf = st_as_sf(testdataframe, coords = c("Twos", "Fives"), crs = 4326, agr = "constant")
# THIS WORKS THIS IS A PROJECTION
st_write(testdataframe_sf,"testdata.shp")

shape_four <- SpatialPolygonsDataFrame(poly_four,data=data.frame(x=x, y=y, z=z))
Shape_crs <- st_as_sf(shape_four, coords=c("x","y"), crs= crs(r))


showP4(showWKT("+init=epsg:28992"))
showEPSG("+proj=longlat +ellps=WGS84")
showP4(showWKT("+init=epsg:4326"))





x = rbind(c(-1,-1), c(1,-1), c(1,1), c(-1,1), c(-1,-1))
x = rbind(c(30,30), c(40,30), c(40,40), c(30,40), c(30,30))
x1 = x + 10
x2 = rbind(c(40,30), c(40,40), c(50,40), c(50,30), c(40,30))
x3 = 0.1 * x + 15
y = x + 3
y1 = x1 + 3
y3 = x3 + 3
m = matrix(c(3, 0), 5, 2, byrow = TRUE)
z = x + m
z1 = x1 + m
z2 = x2 + m
z3 = x3 + m
p1 = Polygons(list( Polygon(x[5:1,]), Polygon(x2), Polygon(x3),
                    Polygon(y[5:1,]), Polygon(y1), Polygon(x1), Polygon(y3)), "ID1")
p2 = Polygons(list( Polygon(z[5:1,]), Polygon(z2), Polygon(z3), Polygon(z1)),
              "ID2")

poly1 <- Polygons(list(Polygon(x[5:1,])),"ID1")
poly2 <- Polygons(list(Polygon(x1)), "ID2")
poly3 <- Polygons(list(Polygon(x2)), "ID3")


D <- list(poly1,poly2,poly3)
C <- SpatialPolygons(D)
B <- SpatialPolygonsDataFrame(C, data=data.frame(x=xc,y=yc,L=L))
A <- st_as_sf(B,coords = c("x","y"), crs = 4326, agr = "constant")
st_write(A,"A.shp")

plot(c(1,100),c(1,100), type="n")
polygon(coordinates(poly1)[,1],coordinates(poly1)[,2])
polygon(coordinates(poly2)[,1],coordinates(poly2)[,2])
polygon(coordinates(poly3)[,1],coordinates(poly3)[,2])

coordinates <- coordinates(C)
xc <- coordinates[,1]
yc <- coordinates[,2]
L <- c(4,14,15)
