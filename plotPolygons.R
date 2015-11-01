plotPolygons<-function(var,maparea,zoomlevel=7){
  if (var=="nhood"){
    coordsvar<-td$q2
  }
  if (var=="goodarea"){
    coordsvar<-td$q4
  }
  if (var=="badarea"){
    coordsvar<-td$q5
  }
  if (var=="exposure"){
    coordsvar<-td$q6
  }
  
  for (i in 1:nrow(td)){
    if (coordsvar[i]!=""){
      coordsi<-coordsvar[i] %>%
        gsub("#",";",.) %>%
        strsplit(.,";") %>%
        extract2(1) %>%
        as.numeric() %>%
        matrix(.,ncol=2,byrow=T) %>%
        as.data.frame()
      coordsi$id<-i
      if (i==1){
        coords<-coordsi
      }
      if (i>1){
        coords<-rbind(coords,coordsi)
      }
    }
  }
  names(coords)[1:2]<-c("lat","lon")
  
  #get map of denmark
  dkmap <- get_map(maparea,zoom=zoomlevel,scale=2,maptype="toner-lite",source="stamen")
  
  #plot polygons
  ggmap(dkmap) +
    #  geom_point(aes(x=lon,y=lat,group=id),data=coords,color="magenta") +
    geom_polygon(aes(x=lon,y=lat,group=id),data=coords,alpha=.2,fill="magenta")
}
