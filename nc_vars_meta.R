# nc_vars_meta()
# Given a file path to a netcdf file, outputs a table with metadata including:
# variable names, dimensions, units, longname, and missingval

library(ncdump)
library(dplyr)

nc_vars_meta = function(nc.file.path){ # full path, including .nc file
  con <- NetCDF(nc.file.path)
  var.index = which(names(con)=="variable")
  out = con[[var.index]]
  return(out)
}