# mouth_ts.R
# function to extract wbm output data at the mouth of a river basin
# requires input files associated with the river network: 
#   basin ID file  
#   upstream area file

# Danielle S Grogan

library(terra)
library(RCurl)  # enables sourcing R code from github
library(lubridate)

# wbm_load()
wbm_load.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/wbm_load.R", ssl.verifypeer=F)
eval(parse(text=wbm_load.script))

############################################################################################################
id_mouth = function(basin.ID, # gridded file with basin ID values
                    ID,       # ID of basin for which you want to find the mouth point
                    up.area)  # gridded file of upstream area. Must match network of basin.ID
  { 
  basin.up = ((basin.ID==ID)*up.area)
  m        = where.max(basin.up, values=TRUE)
  mxy      = vect(xyFromCell(object = basin.up, cell = m[2]))
  crs(mxy) = crs(up.area)
  mxy   # this returns a SpatVector object with point location of the basin mouth
}

############################################################################################################
mouth_ts = function(ID,            # ID of the basin for which you want data from the mouth
                    basin.ID,      # basin ID file (ascii grid of basin IDs)
                    up.area,       # upstream area file (ascii grid)
                    path,          # path to wbm output files
                    varname,       # variable name in wbm output file to extract
                    yrs){          # years of wbm output to extract

  wbm.data = wbm_load(path, varname, yrs)                 # load wbm data
  mxy = id_mouth(basin.ID, ID, up.area)                   # identify basin mouth point
  data.mouth = extract(wbm.data, mxy)                      # extract wbm data from basin mouth point
  data.mouth[,2:length(data.mouth)]                 
}

############################################################################################################
### Output table of yearly values
mouth_ts_basins_annual = function(basin.ID.list,     # list of basin ID values. Typically numeric, but could be character string. Must match basin_ID_grid file
                                  basin.ID.grid,     # grid acsii file giving the ID value for each grid cell
                                  up.area,           # upstream area file (ascii grid)
                                  path,              # character string: path to wbm output (directory with .nc files in it)
                                  varname,           # character string: variable to extract from basin mouth points
                                  yrs,               # years of wbm output to extract
                                  out.nm = NA        # optional name to save output
){
  out.table = data.frame(matrix(nr=length(basin.ID.list), nc=length(yrs)))
  rownames(out.table) = basin.ID.list
  colnames(out.table) = yrs
  for(i in 1:length(basin.ID.list)){
    out.table[i, ] = mouth_ts(basin.ID.list[i],   # ID of the basin for which you want data from the mouth
                              basin.ID.grid,      # basin ID file (ascii grid of basin IDs)
                              up.area,            # upstream area file (ascii grid)
                              path,               # path to wbm output
                              varname,            # variable name in wbm output file to extract
                              yrs)                # years of wbm output to extract
    
  }
  if(!is.na(out.nm)){
    write.csv(out.table, out.nm)
    print(paste(out.nm, "written to file"))
  }
  return(out.table)
}

############################################################################################################
### Monthly values
mouth_ts_basins_monthly = function(basin.ID.list,     # list of basin ID values. Typically numeric, but could be character string. Must match basin_ID_grid file
                                   basin.ID.grid,     # grid acsii file giving the ID value for each grid cell
                                   up.area,           # upstream area file (ascii grid)
                                   path,              # character string: path to wbm output (directory with .nc files in it)
                                   varname,           # character string: variable to extract from basin mouth points
                                   yrs,               # years of wbm output to extract
                                   out.nm = NA        # optional name to save output
){
  out.table = data.frame(matrix(nr=length(basin.ID.list), nc=length(yrs)*12))
  rownames(out.table) = basin.ID.list
  colnames(out.table) = seq(from = ymd(paste(yrs[1], "01", "01", sep="-")), to=ymd(paste(yrs[length(yrs)], "12", "01", sep="-")), by="month")
  for(i in 1:length(basin.ID.list)){
    out.table[i, ] = mouth_ts(basin.ID.list[i],   # ID of the basin for which you want data from the mouth
                              basin.ID.grid,      # basin ID file (ascii grid of basin IDs)
                              up.area,            # upstream area file (ascii grid)
                              path,               # path to wbm output
                              varname,            # variable name in wbm output file to extract
                              yrs)                # years of wbm output to extract
    
  }
  if(!is.na(out.nm)){
    write.csv(out.table, out.nm)
    print(paste(out.nm, "written to file"))
  }
  return(out.table)
}