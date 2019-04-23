# Model validation by linear regression.  
# Report r2 values, slope and intercept of linear regression.
# Optional: Save results as spatial points data frame.  
# Optional: Scatterplot

# Danielle S Grogan
# Last update: 2019-03-29

# notes: Output file types could be more generalized; e.g., allow choice of png, pdf, jpeg, etc for plot output

library(raster)
library(lubridate)
library(rgdal)
library(rgeos)

###################################################################################################
mean_diff = function(x,y){
  # x         = vector; SHOULD BE OBSERVATIONAL DATA 
  # y         = vector; SHOULD BE MODEL DATA 
   out = mean(x - y)
}

###################################################################################################
linear_reg = function(x,
                      y,
                      plot.nm  = NA,
                      date.x,
                      date.y,
                      plot.dir = NA
                      )
  {
  # x         = vector; SHOULD BE OBSERVATIONAL DATA FOR PURPOSES OF PLOTTING
  # y         = vector; SHOULD BE MODEL DATA FOR PURPOSES OF PLOTTING
  # date.x    = vector; dates in format YYYY-MM-DD associated with x
  # date.y    = vector; dates in format YYYY-MM-DD associated with y
  # plot.dir  = character string; file path to write plots. IF "NA" (default), no plot is made
  # plot.nm   = character string; file name, also used for plot title. Must be something other than NA if plot.dir != NA.
  
  # convert dates to julian.Date format - avoids character vs numeric issue in data frame merging
  date.x.j = as.numeric(julian.Date(ymd(date.x), origin = ymd("1800-01-01")))
  date.y.j = as.numeric(julian.Date(ymd(date.y), origin = ymd("1800-01-01")))
  
  dfx = as.data.frame(cbind(date.x.j, x))
  dfy = as.data.frame(cbind(date.y.j, y))
  colnames(dfx)[1] =  colnames(dfy)[1] = "DATE"
  
  # Merge based on dates
  df.merge = merge(dfx, dfy, by = "DATE")
  df.merge[df.merge < 0] = NA
  df.merge.comp = na.omit(df.merge)
  
  # Get linear regression results
  if(nrow(df.merge.comp) > 10 &  # check there are at least 10 data points in the merged data frame
     sum(is.na(df.merge.comp[,2])) < nrow(df.merge.comp) &    # check that neither column is entirely NAs
     sum(is.na(df.merge.comp[,3])) < nrow(df.merge.comp) &
     sum(df.merge.comp[,2] == 0)   < nrow(df.merge.comp))     # check the obs data isn't all zeroes
    {
    S = summary(lm(formula = df.merge.comp[,3] ~ df.merge.comp[,2]))  
    intercept = S$coefficients[[1]]
    slope = S$coefficients[[2]]
    r2 = S$r.squared
    p = coef(S)[[2,4]]
    out = c(intercept, slope, r2, p)

    # if a plot directory is given, make a plot
    if(is.na(plot.dir) == F){
      
      # check that the linear fit can be plotted
      if(is.finite(slope) == T &   
         is.finite(intercept) == T){
        
        png(file.path(plot.dir, paste(plot.nm, ".png", sep="")))
        plot(df.merge.comp[,3] ~ df.merge.comp[,2],
             xlab = "Observed SWE (mm)", ylab = "Modeled SWE (mm)",
             main = plot.nm)
        mtext(paste("R2 =", round(r2,2)))
        abline(a=0, b=1, col='grey', lty=2)
        abline(lm(df.merge[,3] ~ df.merge[,2]), col='red')
      }
      dev.off()
    }
  }else{
    out = c(NA, NA, NA, NA)
  }
  
  return(out)
}

###################################################################################################
linear_val = function(mod.data, 
                     obs.points, 
                     obs.data, 
                     out.name = NA, 
                     out.sp   = F,
                     plot.dir = NA)
  {
  # mod.data   = raster brick of gridded model data
  # obs.points = spatial points object
  # obs.data   = vector; observational data associated with points. Must have same order as obs.points 
  #              First column must be a column of dates named "DATE" 
  # out.name   = character string; file path for saving results. 
  #              End in ".csv" for out.sp = F
  #              No file extension for out.sp = T. Last part of file path will be used for shapefile names
  # out.sp     = F (default) outputs a csv file. Make out.name end in ".csv"
  #              T outputs a spatial points data frame. Make out.name with no file extension. 
  #                New directory with out.name will be made to contain shapefile files.
  # plot.dir   = character string; file path to write plots. IF "NA" (default), no plot is made
  
  # Extract data at points, save as data frame
  mod.pts = as.data.frame(t(raster::extract(mod.data, obs.points)))
  
  # Format dates
  DATE = ymd(sub("X", "", rownames(mod.pts)))
  mod.pts = as.data.frame(cbind(DATE, mod.pts))
  obs.data$DATE = ymd(obs.data$DATE)  # ensure consistent date formatting
  
  # Compare each column of mod.pts to each column of obs.data
  # linear.reg = list, each list element containing (intercept, slope, r2) of linear regression
  #st.names = sub("X", "Station_", colnames(obs.data)[2:ncol(obs.data)])
  st.names = colnames(obs.data)[2:ncol(obs.data)]
  
  if(ncol(obs.data) > 2){
    linear.reg =  mapply(FUN     = linear_reg,
                         x        = obs.data[,2:ncol(obs.data)], 
                         y        = mod.pts[,2:ncol(mod.pts)], 
                         plot.nm  = st.names,
                         MoreArgs = list(
                           date.x = obs.data$DATE,
                           date.y = mod.pts$DATE,
                           plot.dir = plot.dir
                         )
    )
  }else{
    linear.reg = linear_reg(x        = obs.data[,2], 
                            y        = mod.pts[,2],
                            plot.nm  = st.names,
                            date.x = obs.data$DATE,
                            date.y = mod.pts$DATE,
                            plot.dir = plot.dir)
  }

  
  # check if "DATE" column was used:
  # remove.col =  which(grepl("DATE", colnames(linear.reg)))
  # linear.reg = subset(linear.reg, select = -c(remove.col))
  
  lr.array = as.data.frame(linear.reg)
  rownames(lr.array) = c("Intercept", "Slope", "r2", "p_value")
  colnames(lr.array) = st.names
  
  if(is.na(out.name) == F){
    if(out.sp == F){
      # write results as .csv file
      write.table(lr.array, out.name, sep=",")
      
    }else{
      dir.create(out.name) # create directory for shape files
      out_sp = obs.points
      out_sp@data = as.data.frame((t(lr.array)))
      writeOGR(out_sp, 
               dsn   = out.name, 
               layer = strsplit(out.name, "/")[[1]][length(strsplit(out.name, "/")[[1]]) ], 
               driver="ESRI Shapefile",
               overwrite = T)
    }

  }
}
###################################################################################################
map_r2 = function(r2.points, 
                  sp.map,
                  plot.nm){
  # r2.points = spatial points data frame; contains r2 values to map
  # sp.map    = spatial polygons data frame; map file for coastlines, state outlines, etc
  # plot.nm   = character string; path (with .png file extension) for plot 
  
  # reproject r2.points if not in same map projections as sp.map
  if(compareCRS(r2.points, sp.map) == F){
    r2.points = spTransform(r2.points, crs(sp.map))
  }
  
  # set color scale based on r2 values. breaks: red to orange for r2 < 0.5. green to dark green for r2 > 0.5
  rbPal <- colorRampPalette(c('red','orange', 'yellow', 'chartreuse3', 'chartreuse4'))
  r2.points$Col <- rbPal(10)[as.numeric(cut(r2.points$r2,breaks = 10))]
  
  png(plot.nm)
  plot(sp.map, 
       xlim = c(extent(r2.points)[1], extent(r2.points)[2]),
       ylim = c(extent(r2.points)[3], extent(r2.points)[4]))
  
  plot(r2.points, 
       add=T, 
       pch=19, 
       cex=0.5, 
       col=r2.points$Col, 
       legend=T)
  
  legend("bottomright",
         title="R2",
         legend=seq(0, 0.9, by=0.1),
         col = rbPal(10),
         pch=19, 
         cex=0.7, 
         y.intersp=1, 
         bty='n')
  dev.off()
  
  return(plot.nm)
}
###################################################################################################


# # EXAMPLE: WBM simulated Snow Water Equivalent (SWE) vs GHCND observational data
# source("/net/home/eos/dgrogan/git_repos/WBMr/wbm_load.R")
# 
# mod.data = wbm_load(path    = "/net/nfs/merrimack/raid/data/WBM_USNE/livneh/daily",
#                     varname = "snowPack",
#                     years   = seq(from = 1950, to = 2013))
# obs.points = readOGR("SWE_data/SWE_data_formatted/GHCND/GHCND_stations", "GHCND_stations")
# crs(obs.points) = "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 "
# obs.data   = read.delim("SWE_data/SWE_data_formatted/GHCND/GHCND_SWE.csv", header=T, sep=",")
# # out.name  = "/net/nfs/yukon/raid5/projects/Vernal_Windows/validation/test/test_val.csv"
# out.name   = "/net/nfs/yukon/raid5/projects/Vernal_Windows/validation/SWE_Validation/GHCN_SWE"
# out.sp     = T
# plot.dir   = "/net/nfs/yukon/raid5/projects/Vernal_Windows/validation/SWE_Validation/Figures"
# 
# linear_val(mod.data,
#            obs.points,
#            obs.data,
#            out.name,
#            out.sp,
#            plot.dir)
# 
# 
# # map r2 values
# states    = readOGR("/net/nfs/squam/raid/userdata/dgrogan/data/map_data/cb_2015_us_state_500k/", "cb_2015_us_state_500k")
# provinces = readOGR("/net/nfs/squam/raid/userdata/dgrogan/data/map_data/Canada/", "Canada")
# provinces = spTransform(provinces, CRSobj = crs(states))
# us.canada = raster::union(states, provinces)
# r2.points = readOGR("/net/nfs/yukon/raid5/projects/Vernal_Windows/validation/SWE_Validation/GHCN_SWE", "GHCN_SWE")
# 
# # subset to r2 points that have significant p-values
# r2.sub = subset(r2.points, r2.points$p_value < 0.01)
# 
# map_r2(r2.points,
#        sp.map = us.canada,
#        plot.nm = "/net/nfs/yukon/raid5/projects/Vernal_Windows/validation/SWE_Validation/Figures/GHCN_map.png")

