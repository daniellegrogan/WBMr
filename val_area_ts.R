# val_area_ts() 
# Quantify the area in a brick or raster that has a given value 
# Applies optional area mask
# Area output options: Length^2 units or # of grid cells
# Returns a time series (list)
# 
# Danielle S Grogan
# last updated 2019-03-21

library(raster)

######################################################################################################################
val_area_ts = function(brick.data,        # raster brick; data to sum na.area
                       val.count,         # integer, float, or NA; sum grid cells with this value. Must be in same projection and resolution as brick.data
                       area.mask = NA,    # raster of 0 and 1; if provided, only count grid cells within this area
                       area.calc  = F     # T or F; if T an area in the source units of brick.data is returned (e.g., km2 for geographic projections). 
                                          #         If F then a number of grid cells is returned
)
{
  
  if(is.na(val.count)){
    brick.data[is.na(brick.data)] = -9999
    val.count = -9999
  }
  
  brick.mask = mask(brick.data, area.mask)
  brick.val  = (brick.mask == val.count)
  
  if(area.calc == T){
    cell.area = raster::area(landmask)
    brick.val = (brick.val * cell.area)
  }
  
  # sum areas in each layer
  sum.vals = cellStats(brick.val, stat='sum')
  
  return(sum.vals)
}

######################################################################################################################
