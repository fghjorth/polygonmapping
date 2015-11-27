# pilot test form: http://goo.gl/forms/cZGpbhtT6e

# #get google sheets package
# require(devtools)
# devtools::install_github("jennybc/googlesheets")
require(googlesheets)

#read in sheet w. pilot data
# URL https://docs.google.com/spreadsheets/d/1oYnAee7cJuYe6qjVP94FmzLi5pYuNJ6ghD1ncbHKJeM/pubhtml?gid=515798020&single=true
sheetkey<-"1oYnAee7cJuYe6qjVP94FmzLi5pYuNJ6ghD1ncbHKJeM"

??gs_

require(dplyr)
pd <- gs_key(sheetkey) %>%
  gs_read_csv()


### customized: get rows of interest
pd<-pd[18:26,]

#get data frame of coordinates
options(digits=15)

responsenumber<-2

pilotpointslist<-as.list(rep(NA,nrow(pd)))

require(stringr)

for (i in 1:length(pilotpointslist)){
  pilotpointsi <- as.character(pd[i,2]) %>%
    (function(x){gsub("[^0-9.:]+","",x)}) %>%
    strsplit(split=":") %>%
    unlist()
  
  pilotpointsi <- if(pilotpointsi[1]==""){
    pilotpointsi[-1]
  } else pilotpointsi
  
  pilotpointsi<-pilotpointsi %>%
    str_trim() %>%
    as.numeric() %>%
    matrix(ncol=2,nrow=length(pilotpointsi)/2,byrow=T) %>%
    as.data.frame()
  
  names(pilotpointsi)<-c("lat","lon")
  head(pilotpointsi)
  pilotpointsi$id<-i
  
  if(i==1){
    allpilotpoints<-pilotpointsi
  } else allpilotpoints<-rbind(allpilotpoints,pilotpointsi)
  
}

#load map
#get map of copenhagen
require(ggmap)
require(ggplot2)

centerlonlat<-geocode("copenhagen, denmark")

centerlonlat[1]

bbox<-c(left=centerlonlat[1]-.08,
        bottom=centerlonlat[2]-.04,
        right=centerlonlat[1]+.04,
        top=centerlonlat[2]+.05) %>%
  as.numeric()

dkmap<-get_map(location=bbox,scale=2)

ggmap(dkmap)

#plot polygons
setwd("~/GitHub/polygonmapping")
ggmap(dkmap) +
  geom_point(aes(x=lon,y=lat,group=id),data=allpilotpoints,color="magenta") +
  geom_polygon(aes(x=lon,y=lat,group=id),data=allpilotpoints,alpha=.2,fill="magenta")

ggsave("pilotmap1.png")

?ggmap

## SHAPEFILE IMPORT/EXPORT

#export coordinates to shapefile
require(maptools)
require(rgdal)
require(sp)

#repeat first point at the end of each polygon - shapefiles need this
for (i in 1:length(unique(allpilotpoints$id))){
  coordsi<-subset(allpilotpoints,id==unique(allpilotpoints$id)[i])
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
#NOTE: this is modified due to column selection error in original
for (i in 1:length(unique(coords.sp$id))){
  coords.spi<-subset(coords.sp,id==unique(coords.sp$id)[i])
  if (i==1){
    coords.polygon<-Polygons(list(Polygon(coords.spi[,1],hole=F)),ID=paste(unique(coords.sp$id)[i]))
  }
  if (i>1){
    coords.polygon<-c(coords.polygon,Polygons(list(Polygon(coords.spi[,1],hole=F)),ID=paste(unique(coords.sp$id)[i])))
  }
}

#gather list of polygons in a spatial polygons data frame
spdf<-data.frame(f=rep(99.9,length(coords.polygon)))
row.names(spdf)<-unique(coords.sp$id)

#NOTE: added list() argument in line 2
coords.spdf <- coords.polygon %>%
#  list() %>%
  SpatialPolygons() %>% 
  SpatialPolygonsDataFrame(data=spdf)

#write out to shapefile
writePolyShape(coords.spdf,"pilotcoords.shp")

#read back in from shapefile
shpcoords <- readOGR(".","pilotcoords")
proj4string(shpcoords) <- CRS("+proj=longlat +datum=WGS84")
shpcoords <- spTransform(shpcoords, CRS("+proj=longlat +datum=WGS84"))

#assign the right ID's to the shapefile
row.names(shpcoords)<-as.character(shpcoords@data$SP_ID)

#convert shapefile to plotable data
shpcoordsdf <- fortify(shpcoords)

#plot polygons 
ggmap(dkmap) +
  geom_point(aes(x=long,y=lat,group=id),data=shpcoordsdf,color="magenta") +
  geom_polygon(aes(x=long,y=lat,group=id),data=shpcoordsdf,alpha=.2,fill="magenta")

ggsave("pilotmap1shp.png")
