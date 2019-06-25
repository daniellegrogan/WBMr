# mouth_ts.R
# function to extract wbm output data at the mouth of a river basin
# requires input files associated with the river network: 
#   basin ID file  
#   upstream area file

# Danielle S Grogan
# 2019-06-25

library(raster)
############################################################################################################
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
                           raster::brick, 
                           varname = varname)
  )
  return(wbm.brk)
}
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
mouth_ts = function(basin.ID,      # basin ID file (ascii grid of basin IDs)
                    ID,            # ID of the basin for which you want data from the mouth
                    up.area,       # upstream area file (ascii grid)
                    path,          # path to wbm output files
                    varname,       # variable name in wbm output file to extract
                    yrs){          # years of wbm output to extract
    
  
  wbm.data = wbm_load(path, varname, yrs)  # load wbm data
  pt = id_mouth(basin.ID, ID, up.area)     # identify basin mouth point
  data.mouth = extract(wbm.data, pt)       # extract wbm data from basin mouth point
  data.mouth                 
}

############################################################################################################
