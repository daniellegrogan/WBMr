# mouth_ts_basins()
# wrapper on mouth_ts()

### R Libraries
library(RCurl)  # enables sourcing R code from github
library(raster)
rasterOptions(tmpdir = "/net/usr/spool/")   # set alternative /tmp directory
library(rgdal)
library(rgeos)

# wbm_load()
wbm_load.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/wbm_load.R", ssl.verifypeer=F)
eval(parse(text=wbm_load.script))

# mouth_ts()
mouth_ts.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/mouth_ts.R", ssl.verifypeer=F)
eval(parse(text=mouth_ts.script))

mouth_ts_basins = function(basin.ID.list,     # list of basin ID values. Typically numeric, but could be character string. Must match basin_ID_grid file
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
