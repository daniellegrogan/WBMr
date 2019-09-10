# spatial_agg_wrapper()
# calls spatial aggregation functions, and re-names output to reflect dates in wbm input files
# also checks that input unit is in mm

# Danielle S Grogan
# last updated: 2019-09-10

source("~/git_repos/WBMr/spatial_aggregation.R")

spatial_agg_wrapper = function(wbm.var,               # raster or brick of wbm output
                               unit,                  # character string; unit of output (e.g., "mm/day")
                               global = 1,            # if 1, then a global spatial aggregation is done. If 0, then no global spatial aggregation
                               spatial.mask = NA){    # optional; shapefile or ascii grid with IDs, used for spatial aggregations
  
  if(grepl("mm", unit)){   # check unit; if mm, then use standard functions from spatial_aggregation.R
    
    # Global aggregation
    if(global == 1){
      glob.sum = global.sum(wbm.var, km3.out=1)
      
      # name columns with dates
      names(glob.sum) = sub("X", "", names(wbm.var))
      if(sum(grepl(".15", names(glob.sum))) == length(glob.sum)){   # change monthly naming convention from YYYY.MM.15 to YYYY.MM.00
        names(glob.sum) = sub(".15", ".00", names(glob.sum))
      }
      out = glob.sum
    }
    
    # Polygon aggregation
    if(class(spatial.mask)[1] == "SpatialPolygonsDataFrame"){
      spatial.sum = spatial_aggregation(wbm.var, spatial.mask)
      
      # name columns by with dates
      sum.cols = which(grepl("layer.", colnames(spatial.sum@data)))            # identify which columns have the calcuated sums
      colnames(spatial.sum@data)[sum.cols] = sub("X", "", names(wbm.var))      # use layer names from wbm data for the columsn with sums
      if(sum(grepl(".15", colnames(spatial.sum@data)[sum.cols])) == length(sum.cols)){ 
        colnames(spatial.sum@data)[sum.cols] = sub(".15", ".00", names(wbm.var)) # change monthly naming convention from YYYY.MM.15 to YYYY.MM.00
      }
      out = spatial.sum
    }
  }else{
    out = print("input units not mm")  # need to write options for input units other than mm
  }
  return(out)
  
}