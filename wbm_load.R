# wbm_load.R

# R for WBM
# Load WBM data as brick
# Danielle S Grogan
# 2019-03-27

library(raster)

wbm_load = function(path, varname, years = NA, monthly.files = 0){
  # path     = character string; file path to model output
  # varname  = character string; variable name to load 
  # years    = vector or NA; 
  #            vector: sequence of years of model data to read if you don't want to read ALL files in file path
  # monthly.files = binary: 1 or 0 indicating the structure of WBM output files for monthly time series
              # 0 := files are 1 file per year, 12 layers (temporal aggregate of WBM daily output)
              # 1 := files are 1 file per month, with file structure montyly/YYYY/YYYY-MM.nc
  
  if(grepl("monthly", c(raster.path)) & monthly.files == 1){
    if(sum(is.na(years)) == 1){ # if years are not specified, use all years 
      dir.list = dir(raster.path, full.names = T)      # in this file structure, there are directories for each year. List the directories
      file.list = unlist(lapply(dir.list, FUN=list.files, full.names = T)) # list files
    }else{
      dir.list.full = dir(raster.path, full.names = T) # list all directories
      dir.list.yrs = subset(dir.list.full,      # subset to those with names that match the "years" specified
                            as.numeric(
                              substr(dir.list.full, 
                                     start = nchar(dir.list.full)-3, 
                                     stop = nchar(dir.list.full))) 
                            %in% years)
      file.list = unlist(lapply(dir.list.yrs, FUN=list.files, full.names = T)) # list files
    }
  }else{
    if(is.na(years) == T){
      file.list = list.files(path = path, full.names = T)
    }else{
      file.list.full = list.files(path = path, full.names = T)
      file.list = file.list.full[sapply(years, FUN = function(x) grep(pattern=paste("wbm_",x, sep=""), file.list.full))]
    }
  }

  wbm.brk = do.call(stack,
                    lapply(file.list, 
                           raster::brick, 
                           varname = varname)
  )
  return(wbm.brk)
}
