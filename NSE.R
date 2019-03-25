# NSE.R
# This code contains a function for Nash-sutcliffe efficiency (or coefficient) calculation
# and a function to check a 3x3 grid around a location for catchment area matching
# Danielle S Grogan
# 2016-02-18

# packages:
library(rgdal)
library(chron)

########################################################################################
### FUNCTIONS ###
########################################################################################
# Nash-sutcliffe efficiency
NSE<-function(obs, model){
  # obs = observed discharge, time series
  # model = modeled discharge, time series 
  ns = 1-( sum((obs - model)^2) / sum(((obs - mean(obs))^2)) )
}

########################################################################################
# match catchment area: 
# returns the grid cell ID with best match for catchment area within a 3x3 grid
# 3x3 large enough??

match.CA<-function(Lat, Lon, CA, up.area, cell.sz){
  # Lat = obs Q gage latitude, decimal degree 
  # Lon = obs Q gage longitude, decimal degree
  # CA = catchment area of obs Q gage (km2)
  # up.area = upstream area of model river network (CHECK: km2), raster
  
  # search 3x3 grid around matching lat/lon for good catchment area
  lat.n = Lat[1]+cell.sz
  lat.s = Lat[1]-cell.sz
  lon.e = Lon[1]+cell.sz
  lon.w = Lon[1]-cell.sz
  
  a = c(lat.n, lon.w)
  b = c(lat.n, Lon[1])
  c = c(lat.n, lon.e)
  d = c(Lat[1], lon.w)
  e = c(Lat[1], Lon[1])
  f = c(Lat[1], lon.e)
  g = c(lat.s, lon.w)
  h = c(lat.s, Lon[1])
  i = c(lat.s, lon.e)
  
  xy<-rbind(a,b,c,d,e,f,g,h,i)
  xy<-as.data.frame(xy)
  colnames(xy)<-c("lats","lons")   # identify as coordinates
  coordinates(xy)<- ~ lons + lats  # make the data frame a SpatialPixelsDataFrame (spatial object)
  
  area<-extract(up.area, xy)
  match<-min(abs(CA - area), na.rm=T)
  match.index<-min(which(abs(CA - area) == match))
  match.cell = xy[match.index]
}

########################################################################################