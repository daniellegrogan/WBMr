# Extract layers from multiple time series bricks 
# typically needed for analysis of winter climate, 
# e.g., extract layers for November through May from annual time series files
# NB: character string formatting on line 35 is specifically for LOCA climate data file names. 
#     Edit for other climate data inputs

# Danielle S Grogan
# Project: NSF-MSB Vernal Windows
# last updated 2019-06-12

library(raster)

subset_winter = function(path      = "/net/nfs/merrimack/raid/Northeast_US_Downscaling_cmip5",  # path to climate data files 
                         year      = 2016,
                         varname   = 'tasmean',
                         start.doy = 305, # julian date format. 305 = Nov 1  (non-leap years)
                         end.doy   = 151, # julian date format. 151 = May 31 (non-leap years)
                         model     = "IPSL-CM5A-MR",
                         scenario  = "rcp85"){
  
  # list all netCDF files in the path
  files = list.files(path = path,
                     pattern = "*\\.nc",
                     full.names = FALSE)
  

  # loop over the two years needed
  # depending on the offset
  for (i in c(year - 1, year)){
    
    # download or read data
    i.year = paste(i, "0101", sep="")  # format string for LOCA data format
    filename = files[which(grepl(i.year,files) &
                             grepl(varname,files) &
                             grepl(scenario,files) &
                             grepl(toupper(model),toupper(files))
    )]
    
    # if the file exist use the local file
    if (length(filename) != 0){
      data = raster::brick(file.path(path, filename))
      #return(r)
    }else{
      stop("Required files not available: Check your path variable!")
    }
    
    # combine data in one big stack
    if (year != i){
      data.prev.yr = data
    } else {
      # combine data with previous year's
      data.out = raster::stack(data.prev.yr, data)
    }
  }
  
  # grab layer names
  layer_names <- names(data.out)
  
  # extract the yday and year strings
  # depends on how things are subset
  # and pasted back together
  if (grepl("layer", layer_names[1])){
    layer_values = do.call("rbind",strsplit(layer_names, "\\."))
    yday = as.numeric(layer_values[,2])
    years = as.numeric(layer_values[,3])
  } else {
    dates = as.Date(layer_names,"X%Y.%m.%d")
    yday = as.numeric(format(dates,"%j"))
    years = as.numeric(format(dates,"%Y")) - year + 2
  }
  
  # calculate if the previous year was a leap year
  # to account for this offset
  leap_year = ifelse((year-1%%4==0 & year-1%%100!=0) | year-1%%400==0,
                     TRUE,
                     FALSE)
  
  # select layers to subset using this year and yday data
  # account for leap years included in the NEX data  ### LOCA also accounts for leap years
  if(leap_year){
    layers = which((years == 1 & yday >= (start.doy+1)) |
                     (years == 2 & yday <= (end.doy+1 )))
  } else {
    layers = which((years == 1 & yday >= start.doy) |
                     (years == 2 & yday <= end.doy))
  }
  
  # subset raster data
  data.winter = subset(data.out, layers)
 
  # return raster stack of winter data
  return(data.winter)
  
} # end
