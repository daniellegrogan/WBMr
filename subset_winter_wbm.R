# Extract layers from wbm yearly files, crossing years
# typically needed for analysis of winter climate, 
# e.g., extract layers for November through May from annual time series files

# Danielle S Grogan
# Project: NSF-MSB Vernal Windows
# last updated 2019-10-04

library(raster)
library(lubridate)
source("~/git_repos/WBMr/wbm_load.R")

subset_winter_wbm = function(path,      # path to wbm files 
                             year       = 2016,
                             varname    = 'runoff',
                             start.date = "11-01", # MM-DD format
                             end.date   = "05-31"  # MM-DD format
){
  
  
  # load data for two years
  wbm.data = wbm_load(path = path, 
                      varname = varname,
                      years = seq(year-1, year))
  
  # make a date sequence
  date.seq = ymd(seq(from = ymd(paste(year-1, start.date, sep="-")), 
                     to = ymd(paste(year, end.date, sep="-")),
                     by = "day"))
  
  # read layer names
  layer_names = names(wbm.data)
  
  # format layer names as dates
  layer_dates = sub("X", "", layer_names)
  layer_dates = sub("\\.", "-", layer_dates)
  layer_dates = sub("\\.", "-", layer_dates)
  
  # idenfity layers to keep
  layers_keep = as.Date(layer_dates) %in% as.Date(date.seq)
 
  # subset raster data
  data.winter = subset(wbm.data,  which(layers_keep == T))
  
  # return raster stack of winter data
  return(data.winter)
  
} # end
