# wbm_load.R

# R for WBM
# Load WBM data as brick
# Danielle S Grogan
# 2019-03-27

library(raster)

wbm_load = function(path, varname, years = NA){
  # path     = character string; file path to model output
  # varname  = character string; variable name to load 
  # years    = vector or NA; 
  #            vector: sequence of years of model data to read if you don't want to read ALL files in file path

  if(is.na(years) == T){
    file.list = list.files(path = path, full.names = T)
  }else{
    file.list.full = list.files(path = path, full.names = T)
    file.list = file.list.full[sapply(years, FUN = function(x) grep(pattern=x, file.list.full))]
  }
  wbm.brk = do.call(stack,
                    lapply(file.list, 
                           brick, 
                           varname = varname)
  )
  return(wbm.brk)
}
