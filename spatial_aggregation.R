# Spatial aggregation of WBM output (or other raster data in units: mm/day)
# Danielle S Grogan

#### UPDATED TO USE TERRA PACKAGE 2024-01-16

# INPUT DATA MUST BE IN MM

# spatial_aggregation function:
  # spatial aggregation of raster data by shapefile.  Can calculate mean or sum.

# global_sum function:
  # spatial aggregation of a raster - outputs sum or area-weighted mean of all grid cell values

# packages:
library(sp)
library(terra)

###################################################################################################
spatial_aggregation<-function(raster.data, shapefile, s = 1){
  # raster.data = raster (or brick?) of data in mm
  # shapefile   = shapefile with polygons
  # s: set to 1 for sum over area, 0 for mean (mm) over area

  if(s == 1){ # output sum (km3)
    cell.area = terra::cellSize(raster.data, unit = 'km') # unit: km2.  
    mm_to_km = 10^-6
    data.km3 = raster.data * cell.area * mm_to_km
    data.out = terra::extract(data.km3, shapefile, fun=sum, na.rm=T)
    
  }else{ # output mean (mm)
    data.out = terra::extract(raster.data, shapefile, fun=mean, na.rm=T)
  }
  data.out
}

###################################################################################################
global.sum<-function(raster.data, km3.out=1){
  # km3.out: if 1, the output is in km3.  If 0, output is mm (average over area)
  # outputs SUM (no average)
  
    cell.area = terra::cellSize(raster.data, unit = 'km') # unit: km2.  
    mm_to_km  = 10^-6
    km3.grid = raster.data * cell.area * mm_to_km
 
    if(km3.out == 1){ # output km3/yr sum
      data.out = terra::global(km3.grid, fun = sum, na.rm = TRUE)   
      
    }else{ # output as mm/yr mean
      cell.weights = cell.area/as.numeric((global(cell.area, fun = max, na.rm = TRUE)))
      data.out = terra::global(raster.data*cell.weights, fun = sum, na.rm = TRUE)   
    }

  data.out
}

###################################################################################################
# EOF
