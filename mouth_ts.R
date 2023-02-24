# mouth_ts.R
# function to extract wbm output data at the mouth of a river basin
# requires input files associated with the river network: 
#   basin ID file  
#   upstream area file

# Danielle S Grogan
# 2019-06-25

library(raster)
library(RCurl)  # enables sourcing R code from github

# wbm_load()
wbm_load.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/wbm_load.R", ssl.verifypeer=F)
eval(parse(text=wbm_load.script))

############################################################################################################
id_mouth = function(basin.ID, ID, up.area){
  basin.up<-((basin.ID==ID)*up.area)
  basin.mouth = (basin.up == max(as.matrix(basin.up),na.rm=T))
  crs(basin.mouth) <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"
  r.pts <- rasterToPoints(basin.mouth, spatial=TRUE)
  r.pts@data <- data.frame(r.pts@data, long=coordinates(r.pts)[,1],
                           lat=coordinates(r.pts)[,2])
  row = which(r.pts@data$layer==1)
  coords = cbind(r.pts@data$long[row],r.pts@data$lat[row]) 
  basin.m = SpatialPoints(coords)
  crs(basin.m) <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"
  basin.m
}

############################################################################################################
mouth_ts = function(ID,            # ID of the basin for which you want data from the mouth
                    basin.ID,      # basin ID file (ascii grid of basin IDs)
                    up.area,       # upstream area file (ascii grid)
                    path,          # path to wbm output files
                    varname,       # variable name in wbm output file to extract
                    yrs){           # years of wbm output to extract

                             
    
  
  wbm.data = wbm_load(path, varname, yrs)                 # load wbm data
  pt = id_mouth(basin.ID, ID, up.area)                    # identify basin mouth point
  data.mouth = extract(wbm.data, pt)                      # extract wbm data from basin mouth point
  data.mouth                 
}

############################################################################################################
