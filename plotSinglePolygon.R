plotSinglePolygon<-function(i){
  get_map("Denmark",zoom=7) %>%
    ggmap(.) + 
    geom_polygon(data=subset(coords,id==i),aes(x=lon,y=lat),group=id,fill="magenta",alpha=.5)
}
