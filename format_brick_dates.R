# format_brick_dates()
# function to convert the layer names of a raster brick into dates.

library(raster)
library(rgdal)

# infile is a raster brick
# output: character strings of dates 

format_brick_dates = function(infile){
  # format layer names as dates, use to subset input file
  date.str = (sub("X", "", names(infile)))
  date.str = (sub(".00.00.00", "", date.str))
  date.str = (sub(".01.00.00", "", date.str))
  date.layers = as.Date(date.str, format = "%Y.%m.%d")
  date.layers
}
