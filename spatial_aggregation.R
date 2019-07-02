# Spatial aggregation of WBM output (or other raster data in units: mm/day)
# Danielle S Grogan
# 2015-03-03 

# INPUT DATA MUST BE IN MM

# spatial_aggregation function:
  # spatial aggregation of raster data by shapefile.  Can calculate mean or sum.

# global_sum function:
  # spatial aggregation of a raster - outputs sum or area-weighted mean of all grid cell values

# packages:
library(raster)
library(rgdal)  # IMPORTANT: THIS CODE ONLY WORKS WITH THE MOST RECENT VERSION OF RGDAL PACKAGE 
# update.packages("rgdal")  # checks for most recent version of rgdal

###################################################################################################
spatial_aggregation<-function(raster.data, shapefile, s = 1, cell.area = 1, weight = T, poly.out = T){
  # raster.data = raster (or brick?) of data in mm
  # shapefile   = shapefile with polygons
  # s: set to 1 for sum over area, 0 for mean (mm) over area
  # cell.area   = raster of cell areas, used if sum=1.  If no input raster is provided, cell.area is calculted from raster.data
  #             NOTE: R function cell area results in 0.2% smaller cell area at equator than WBM-based 30-minute cell size
  # weight: if T, the mean value is the area-weighted mean of cells within a polygon
  # poly.out: if T, the output is a shapefile that includes the new, sum or mean value.  If F, output is just a list of values.
  
  if(s == 1){ # output sum (km3)
    if(length(cell.area)==1){
      #print("cell.area calculated from raster.data")
      cell.area<-raster::area(raster.data) # unit: km2.  
    }
    mm_to_km = 10^-6
    # raster.data[is.na(raster.data)]<-c(0)   # causes problems with bricks
    data.km3<-overlay(raster.data, cell.area, fun=function(x,y){return(mm_to_km * x * y)}) 
    data.out<-raster::extract(data.km3, shapefile, fun=sum, na.rm=T, sp=poly.out)
  }else{ # output mean (mm)
    data.out<-extract(raster.data, shapefile, fun=mean, weights = weight, na.rm=T, sp=poly.out)
  }
  data.out
}

###################################################################################################
global.sum<-function(raster.data, km3.out=1, cell.area = 1){
  # km3.out: if 1, the output is in km3.  If 0, output is mm (average over area)
  # cell.area   = raster of cell areas, used if km3.out=1.  If no input raster is provided, cell.area is calculted from raster.data
  #             NOTE: Native R function to calculate cell area results in 0.2% smaller cell area at equator than WBM-based 30-minute cell size
  # outputs SUM (no average)
  
    if(length(cell.area)==1){
      #print("cell.area calculated from raster.data")
      cell.area<-raster::area(raster.data) # unit: km2.  
    }
    
    mm_to_km = 10^-6
    km3.yr.grid<-overlay(raster.data, cell.area, fun=function(x,y){x*y*mm_to_km})
    if(km3.out == 1){ # output km3/yr sum
      data.out = sum(as.matrix(km3.yr.grid), na.rm=T)
      
    }else{ # output as mm/yr mean
      cell.weights = cell.area/max(as.matrix(cell.area))
      data.out = sum(as.matrix(raster.data*cell.weights), na.rm=T)  # area-weights the mm value of each grid cell
    }

  data.out
}
###################################################################################################
global.sum.stack<-function(brick.data, km3.out=1, cell.area = 1){
  # km3.out: if 1, the output is in km3.  If 0, output is mm (average over area)
  # cell.area   = raster of cell areas, used if km3.out=1.  If no input raster is provided, cell.area is calculted from brick.data
  #             NOTE: Native R function to calculate cell area results in 0.2% smaller cell area at equator than WBM-based 30-minute cell size
  # outputs SUM (no average)
  
  if(length(cell.area)==1){
    #print("cell.area calculated from raster.data")
    cell.area<-area(brick.data) # unit: km2.  
  }
  
  mm_to_km = 10^-6
  km3.yr.grid<-overlay(brick.data, cell.area, fun=function(x,y){x*y*mm_to_km})
  data.out<-mat.or.vec(nr=nlayers(data),nc=1)
  if(km3.out == 1){ # output km3/yr sum
    for(i in 1:nlayers(data)){
      data.out[i]<-sum(as.matrix(subset(km3.yr.grid, i)), na.rm=T)
    }
  }else{ # output as mm/yr mean
    cell.weights = cell.area/max(as.matrix(cell.area))
    for(i in 1:nlayers(data)){
      data.out[i]<-mean(as.matrix(subset((brick.data*cell.weights), i)), na.rm=T)
    }
  }
  
  data.out
}
###################################################################################################
spatial.agg.ts = function(path, var, shape, s=1, cell.area = 1, weight = T, scl=1, ts='d', poly.out = F){
  # path  = character string; path to wbm output files (e.g., /net/nfs/yukon/.../WBM_run/daily/)
  # var   = character string; variable name in wbm files
  # shape = shapefile over which to aggregate
  # s: set to 1 for sum over area, 0 for mean (mm) over area
  # cell.area   = raster of cell areas, used if sum=1.  If no input raster is provided, cell.area is calculted from raster.data
  #             NOTE: R function cell area results in 0.2% smaller cell area at equator than WBM-based 30-minute cell size
  # weight: if T, the mean value is the area-weighted mean of cells within a polygon
  # scl = 0 or 1; set to 1 to scale from daily average values (e.g., daily average runoff over a year) so sum over time series (e.g, total runoff over a year)
  # ts    = character string, one of:
  #   'd'  for daily files
  #   'm'  for monthly files
  #   'mc' for monthly climatology files
  #   'y'  for yearly files
  #   'yc' for yearly climatology files
  # poly.out: if T, the output is a shapefile that includes the new, sum or mean value.  If F, output is just a list of values.

  if(ts == 'd' | ts == 'm' | ts == 'mc'){
    data1 = do.call(stack,
                    lapply(list.files(path,
                                      full.names=T),
                           brick, varname = var))
    
  }else if(ts == 'y' | ts == 'yc' ){
    data1 = do.call(brick,
                    lapply(list.files(path,
                                      full.names=T),
                           raster, varname = var))

  }
  
  if(scl == 1){
    if(ts == 'd'){
      x = 1 
    }
    else if(ts == 'm' | ts == 'mc'){
      x = 30   # scale x30 b/c monthly wbm output is in daily ave. this scale makes monthly sum
    }else if(ts == 'y' | ts == 'yc'){
      x = 365  # scale x365 b/c yearly wbm output is in daily ave. this scale makes yearly sum
    }
  }else{
    x = 1
  }
  
  data.shape = spatial_aggregation(x*data1, shape, s, cell.area, weight, poly.out)
}

###################################################################################################
