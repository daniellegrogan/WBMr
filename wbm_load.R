# wbm_load.R

# R for WBM
# Load WBM data as brick
# Danielle S Grogan

# 2024-01-16 UPDATED TO USE TERRA PACKAGE   

library(terra)

wbm_load = function(path, varname, years = NA){
  # path     = character string; file path to model output
  # varname  = character string; variable name to load 
  # years    = vector or NA; 
  #            vector: sequence of years of model data to read if you don't want to read ALL files in file path

  
    if(sum(is.na(years)) == 1){
      file.list = list.files(path = path, full.names = T)
    }else{
      file.list.full = list.files(path = path, full.names = T)
      file.list = file.list.full[unlist(sapply(years, FUN = function(x) grep(pattern=paste("wbm_",x, sep=""), file.list.full)))]
    }

  wbm.out = terra::rast(file.list, lyrs = varname)
  
  return(wbm.out)
}
