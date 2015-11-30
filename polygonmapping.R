wd<-"~/GitHub/polygonmapping/"
setwd(wd)

#read in lat long test data
latlongs<-read.table("latlongdata.txt",sep=";",header=T)

#long form data frame of lat long pairs
require(dplyr)

#get only the nhood from østerbro
latlongs<-subset(latlongs,idno==2110)

for (i in 1:length(latlongs$idno)){
  coords <- strsplit(as.character(latlongs$neighborhood[i]),split=";")[[1]] %>%  
    strsplit(split=",") %>%
    unlist()
  coords <- matrix(coords,ncol=2,nrow=length(coords)/2,byrow=T) %>%
    as.data.frame()
  coords$id<-latlongs$idno[i]
  if (i==1){
    coordsdf1<-coords
  }
  if (i>1){
    coords<-rbind(coordsdf1,coords)
  }
}

names(coords)[1:2]<-c("lat","lon")

#create lon/lat numeric coordinates for ggmap
coords<-coords[,c(2,1,3)]
coords$lon<-as.numeric(as.character(coords$lon))
coords$lat<-as.numeric(as.character(coords$lat))

#get map of copenhagen
require(ggmap)
cphmap <- get_googlemap("parken, copenhagen",zoom=15,scale=2)

#plot polygons
ggmap(cphmap) +
  geom_point(aes(x=lon,y=lat,group=id),data=coords,color="magenta") +
  geom_polygon(aes(x=lon,y=lat,group=id),data=coords,alpha=.2,fill="magenta")

ggsave("polygonmap.pdf")
ggsave("polygonmap.png")

#export coordinates to shapefile
require(maptools)
require(rgdal)
require(sp)

#repeat first point at the end of each polygon - shapefiles need this
for (i in 1:length(unique(coords$id))){
  coordsi<-subset(coords,id==unique(coords$id)[i])
  coordsi<-rbind(coordsi,head(coordsi,1))
  if (i==1){
    coords.sp<-coordsi
  }
  if (i>1){
    coords.sp<-rbind(coords.sp,coordsi)
  }
}

#set projection
coordinates(coords.sp) <- ~lon+lat
proj4string(coords.sp) <- CRS("+proj=longlat +datum=WGS84")

#points to list of N polygons
for (i in 1:length(unique(coords.sp$id))){
  coords.spi<-subset(coords.sp,id==unique(coords.sp$id)[i])
  if (i==1){
    coords.polygon<-Polygons(list(Polygon(coords.spi@coords[,1:2],hole=F)),ID=paste(unique(coords.sp$id)[i]))
  }
  if (i>1){
    coords.polygon<-c(coords.polygon,Polygons(list(Polygon(coords.spi@coords[,1:2],hole=F)),ID=paste(unique(coords.sp$id)[i])))
  }
}

#gather list of polygons in a spatial polygons data frame
spdf<-data.frame(f=rep(99.9,length(coords.polygon)))
row.names(spdf)<-unique(coords.sp$id)

coords.spdf <- coords.polygon %>%
  list() %>%
  SpatialPolygons() %>% 
  SpatialPolygonsDataFrame(data=spdf)

#get area of polygon(s)
require(geosphere)
areaPolygon(coords.spdf)

#write out to shapefile
writePolyShape(coords.spdf,"coords.shp")

#read back in from shapefile
shpcoords <- readOGR(".","coords")
proj4string(shpcoords) <- CRS("+proj=longlat +datum=WGS84")
shpcoords <- spTransform(shpcoords, CRS("+proj=longlat +datum=WGS84"))

#assign the right ID's to the shapefile
row.names(shpcoords)<-as.character(shpcoords@data$SP_ID)

#convert shapefile to plotable data
shpcoordsdf <- fortify(shpcoords)

#plot polygons 
ggmap(cphmap) +
  geom_point(aes(x=long,y=lat,group=id),data=shpcoordsdf,color="magenta") +
  geom_polygon(aes(x=long,y=lat,group=id),data=shpcoordsdf,alpha=.2,fill="magenta")

ggsave("polygonmap_fromshapefile.pdf")
