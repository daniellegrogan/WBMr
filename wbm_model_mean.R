# wbm_model_mean.R
# Function to read WBM results from a list of file paths, then average the raster bricks
# This function assumes that the set of wbm results in each file path contains the same number of layers
# Function is useful for making multi-model means for WBM run by an ensemble of GCMs

# Danielle S Grogan
# updated by Becker Gibson 11/20/2024

# source wbm_load function
source("https://raw.githubusercontent.com/daniellegrogan/WBMr/refs/heads/master/wbm_load.R")

wbm_model_mean <- function(
    file.path.list,  # list of file paths (character strings) from which to read WBM files
    yrs,             # vector of years, or NA
    var,             # character string: name for variable to average
    out.dir = NA,    # character string: directory to which to write output. if NA, no file written
    out.nm = NA,     # character string; name for new file - include file extension - .CDF suggested
    ret = 1          # 1 or 0; if 1, function returns the raster result.  If 0, no return value
){        
  rast.list <- do.call(c, lapply(file.path.list, FUN = \(x) wbm_load(x, varname = var, years = yrs)))
  
  ids <- rep(seq(1:(terra::nlyr(rast.list)/length(file.path.list))), length(file.path.list))
  
  if (is.na(out.dir)){
    rast.list.mmm <- terra::tapp(
      rast.list, 
      index = ids, 
      fun = mean,
      na.rm = T)
  } else {
    rast.list.mmm <- terra::tapp(
      rast.list, 
      index = ids, 
      fun = mean,
      overwrite = T,
      na.rm = T)
    terra::writeRaster(rast.list.mmm, filename = file.path(out.dir, out.nm), overwrite = T)
  }
  
  if(ret == 1){
    return(rast.list.mmm)
  }
}