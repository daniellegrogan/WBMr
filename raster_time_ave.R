# raster_time_ave.R
# Function to time average or sum layers of a raster brick or stack
# Function is useful for making climatologies or decadal averages of WBM output (or other raster data)

# Danielle S Grogan
# Last updated 2019-07-23


raster_time_ave = function(brk.data,        # a raster brick or raster stack
                           time.step,       # integer; time step over which to aggregate, e.g., 10 (will aggregate 10 layers)
                           start.lyr = 1,   # integer; the brick or stack layer at which to begin aggregation
                           s = 1,           # 1 or 0; 1 will sum, 0 will average
                           out.dir = NA,    # character string; dirctory to which to write output. If NA, then no file is written
                           out.nm,          # character string; file name for output
                           r = 1){          # 1 or 0; 1 will return result as brick, 0 will not
  
  # if start.lyr != 1, remove unused layers
  if(start.lyr !=1){
    brk.data = subset(brk.data, start.lyr:nlayers(brk.data))
  }
  
  # calculate number of resulting time-average layers
  n.out = nlayers(brk.data)/time.step
  
  # if the number of layers in brk.data is not a multiple of the time step, trim end layers
  if(floor(n.out) != n.out){
    print("not all layers used")
    brk.data = subset(brk.data, start.lyr:(time.step * floor(n.out)))
  }
  
  # make indices for aggregating
  ind = sapply(seq(1:n.out), FUN = function(x) rep(x, time.step))
  
  # aggregate
  if(is.na(out.dir)){
    if(s == 1){ # sum
      brk.agg = stackApply(brk.data, indices = ind, fun = sum)
    }else{ # average
      brk.agg = stackApply(brk.data, indices = ind, fun = mean)
    }
  }else{
    if(s == 1){ # sum
      brk.agg = stackApply(brk.data, indices = ind, fun = sum,
                           filename = file.path(out.dir, out.nm))
    }else{ # average
      brk.agg = stackApply(brk.data, indices = ind, fun = mean,
                           filename = file.path(out.dir, out.nm))
    }
  }
  
  if(r == 1){
    return(brk.agg)
  }

}       