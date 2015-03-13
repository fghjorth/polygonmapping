wd<-"~/GitHub/polygonmapping/"
setwd(wd)

#read in lat long test data
latlongs<-read.table("latlongdata.txt",sep=";",header=T)

#data frame of lat long pairs
require(dplyr)
coords <- strsplit(as.character(latlongs$neighborhood[1]),split=";")[[1]] %>%  
  strsplit(split=",") %>%
  unlist()
coords <- matrix(coords,ncol=2,nrow=length(coords)/2,byrow=T) %>%
  as.data.frame()
names(coords)<-c("lat","lon")

#create lon/lat numeric coordinates for ggmap
coords<-coords[,c(2,1)]
coords$lon<-as.numeric(as.character(coords$lon))
coords$lat<-as.numeric(as.character(coords$lat))

#get map of copenhagen
require(ggmap)
cphmap <- get_googlemap("copenhagen",zoom=13)

#plot polygon
ggmap(cphmap) +
  geom_point(aes(x=lon,y=lat),data=coords,color="magenta") +
  geom_polygon(aes(x=lon,y=lat),data=coords,alpha=.2,fill="magenta")

ggsave("polygonmap.pdf")

#export coordinates to shapefile
require(maptools)
require(rgdal)
require(sp)

#set projection
coords.sp<-coords
coordinates(coords.sp) <- ~lon+lat
proj4string(coords.sp) <- CRS("+proj=longlat +datum=WGS84")

#convert to spatialpointsdataframe
coords.spdf <- SpatialPointsDataFrame(coords.sp, data.frame(id=1:nrow(coords)))

#write out to shapefile
writePointsShape(coords.spdf,"coords.shp")

#read back in from shapefile
shpcoords <- readOGR(".","coords")
proj4string(shpcoords) <- CRS("+proj=longlat +datum=WGS84")
shpcoords <- spTransform(shpcoords, CRS("+proj=longlat +datum=WGS84"))

#read points in from data
shpcoordsdf<-as(shpcoords,"data.frame")

#plot polygon 
ggmap(cphmap) +
  geom_point(aes(x=lon,y=lat),data=shpcoordsdf,color="magenta") +
  geom_polygon(aes(x=lon,y=lat),data=shpcoordsdf,alpha=.2,fill="magenta")

ggsave("polygonmap_fromshapefile.pdf")