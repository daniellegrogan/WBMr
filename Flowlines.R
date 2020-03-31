# 2016-10-25
# make a vector shapefile showing flow direction lines, based on a flowdirection raster
library(raster)
library(rasterVis)
library(rgdal)

###########################################################################################
flowlines<-function(flowdir, basinID=NA, uparea=NA, region=NA, out.loc, out.name){
  # flowdir is a raster of flow directions, RGIS/ArcInfo Decimal system
  # basinID is a raster of basinIDs.  Optional
  # uparea is a raster of upstream areas.  Optional
  # region = extent object to define a subregion. Optional
  # out.loc = character, directory for output
  # out.name = character, file name for spatial line output. NO FILE EXTENSION
  
  if(is.na(region)==F){
  flowdir<-crop(flowdir, region)
  basinID<-crop(basinID, region)
  uparea<-crop(uparea, region)
  }
  
  res = coordinates(flowdir)[2] - coordinates(flowdir)[1] # spatial resolution
  
  dir.0  <-(flowdir == 0)
  dir.1  <-(flowdir == 1)
  dir.2  <-(flowdir == 2)
  dir.4  <-(flowdir == 4)
  dir.8  <-(flowdir == 8)
  dir.16 <-(flowdir == 16)
  dir.32 <-(flowdir == 32)
  dir.64 <-(flowdir == 64)
  dir.128<-(flowdir == 128)
  
  long.plus1  = (dir.128 | dir.1  | dir.2)
  long.minus1 = (dir.32  | dir.16 | dir.8)
  long.same   = (dir.64  | dir.0  | dir.4)
  
  lat.plus1   = (dir.32  | dir.64 | dir.128)
  lat.minus1  = (dir.8   | dir.4  | dir.2  )
  lat.same    = (dir.16  | dir.0  | dir.1  )
  
  # make a raster set of beginning coordinates
  begin.coord <- data.frame(coordinates(flowdir))
  begin.coord.r<-raster(nrows=nrow(flowdir), ncols=ncol(flowdir), res = res(flowdir)[1])
  begin.coord.r<-crop(begin.coord.r, flowdir)
  if(is.na(region)==F){
  begin.coord.r<-crop(begin.coord.r, region)
  }
  
  begin.coord.long<-setValues(begin.coord.r, begin.coord$x)
  begin.coord.lat <-setValues(begin.coord.r, begin.coord$y)
  
  
  end.coord.long = ((begin.coord.long + res)*(long.plus1)) + 
    ((begin.coord.long - res)*(long.minus1)) + 
    ((begin.coord.long)      *(long.same))
  
  end.coord.lat = ((begin.coord.lat + res)*(lat.plus1)) + 
    ((begin.coord.lat - res)*(lat.minus1)) + 
    ((begin.coord.lat)      *(lat.same))
  
  end.coord<-data.frame(matrix(nr=nrow(begin.coord), nc=ncol(begin.coord)))
  names(end.coord)=names(begin.coord)
  end.coord[,1]<-getValues(end.coord.long)
  end.coord[,2]<-getValues(end.coord.lat)
  
  flowdir<-getValues(flowdir)
  if(is.null(nrow(basinID))==F){
    basin.id<-getValues(basinID)
  }
  if(is.null(nrow(uparea))==F){
    up.area<-getValues(uparea)
  }
  
  # ignore cells with no flow direction
  coords<-cbind(begin.coord, end.coord, basin.id, flowdir, up.area)
  coords<-subset(coords, is.na(coords[,3])==0)
  
  begin = c(coords[,1], coords[,2])
  end   = c(coords[,3], coords[,4])
  
  # there should be a faster way to create this Lines object: to do - use apply here instead of loop
  l <- vector("list", nrow(coords))
  for (i in 1:nrow(coords)) {
    begin = c(coords[i,1], coords[i,2])
    end   = c(coords[i,3], coords[i,4])
    
    l[[i]] <- Lines(list(Line(rbind(begin, end))), as.character(i))
    text<-paste(i, "/", length(l))
    print(text)
  }
  
  FlowLines = SpatialLines(l)
  df = data.frame(row.names=sapply(slot(FlowLines, "lines"), function(x) slot(x, "ID")))
  df$flowdir = coords[,6]
  if(is.null(nrow(basinID))==F){
    df$basinID = coords[,5]
  }
  if(is.null(nrow(uparea))==F){
    df$uparea = coords[,7]
  }
  
  Sldf = SpatialLinesDataFrame(FlowLines, data = df)
  writeOGR(Sldf, dsn = out.loc, layer=out.name, driver = "ESRI Shapefile", overwrite_layer=T)
}
###########################################################################################

