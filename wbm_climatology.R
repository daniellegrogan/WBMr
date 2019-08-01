# wbm_climatology.R
# Function to read WBM results from a directory, then average across files

# Danielle S Grogan
# Last updated 2019-08-01

library(RCurl)  # enables sourcing R code from github
library(raster)
library(rgeos)
library(rgdal)

### Source functions from other github repos:
# wbm_load()
wbm_load.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/wbm_load.R", ssl.verifypeer=F)
eval(parse(text=wbm_load.script))

wbm_climatology = function(path,            # character string; path to wbm files
                           varname,         # character string; variable name
                           yrs,             # vector of years, or NA
                           out.dir,         # character string: directory to which to write output. if NA, no file written
                           out.nm,          # character string; name for new file
                           ret = 1){        # 1 or 0; if 1, function returns the raster or brick result.  If 0, no return value
  
  brk = wbm_load(path, varname, yrs)
  ids = rep(seq(1:(nlayers(brk)/length(yrs))), length(yrs))
  
  if(is.na(out.dir)){
    brk.ave = stackApply(brk, 
                         indices = ids, 
                         fun = mean)
  }else{
    brk.ave = stackApply(brk, 
                         indices = ids, 
                         fun = mean,
                         filename = file.path(out.dir, out.nm),
                         overwrite = T)
  }
  
  if(ret == 1){
    return(brk.mmm)
  }
  
}


