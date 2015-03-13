# Plotting and converting polygon data in R
This repo containts R code for reading in sets of lat/long pairs, plotting as polygons, and exporting polygons to shapefile format.

The input is a string of lat/long pairs for each respondent as such:

```
lat1,long1;lat2,long2;...;latN,longN
```

The testfile `latlongdata.txt` contains coordinate strings for 2 polygons. Plotted on a map, they look like this:

![](polygonmap.png?raw=true)
