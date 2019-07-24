# wbm_model_mean.R
# Function to read WBM results from a list of file paths, then average the raster bricks
# This function assumes that the set of wbm results in each file path contains the same number of layers
# Function is useful for making multi-model means for WBM run by an ensemble of GCMs

# Danielle S Grogan
# Last updated 2019-07-23

wbm_model_mean = function(file.path.list,   # list of file paths (character strings) from which to read WBM files
                          yrs,             # vector of years, or NA
                          out.dir,         # character string: directory to which to write output. if NA, no file written
                          out.nm,          # character string; name for new file
                          ret = 1          # 1 or 0; if 1, function returns the raster or brick result.  If 0, no return value
                          ){
  
  brk = do.call(stack,
                lapply(file.path.list,
                       FUN = function(x) wbm_load(x, varname = varname, years = yrs)))
  
  ids = rep(seq(1:(nlayers(brk)/length(gcm.list))), length(gcm.list))
  if(is.na(out.dir)){
    brk.mmm = stackApply(brk, 
                         indices = ids, 
                         fun = mean)
  }else{
    brk.mmm = stackApply(brk, 
                         indices = ids, 
                         fun = mean,
                         filename = file.path(out.dir, out.nm),
                         overwrite = T)
  }
  
  if(ret == 1){
    return(brk.mmm)
  }

}


