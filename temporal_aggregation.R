# 2014-03-02
# temporal aggregation of gridded WBM output

# packages:
library(raster)
library(rgdal)

temp.agg<-function(file.path, file.nm.pre, cumulative=1, agg.from, agg.to, varname, unit, 
                   out.dir, out.name, start.year, end.year, over.write=FALSE){
  
  # file.path   = path to data that is to be aggregated
  # file.nm.pre = prefix of file name, data to be aggregated 
  # cumulative: if 1, cumulative aggregation.  if 0, average.
  # agg.from  : options: daily, monthly (time step of data to be aggregated)
  # agg.to    : options: monthly, monthly_clim, yearly, yearly_clim (output time step)
  # varname    = variable name in file to aggregate
  # unit       = unit of data to be aggregated
  # out.dir    = directory to write output
  # our.name   = prefix of file names to write to output
  # start.year = start year of aggregation
  # end.year   = end year of aggregation
  # over.write : if 1, overwrite existing file of same name.  If 0, do not overwrite (code will stop if it encounters existing file)
  
  #########################################################################
  ### daily to monthly aggregation
  #########################################################################
  if(agg.from == 'daily' & agg.to == 'monthly'){
    
    for(y in start.year:end.year){
      year=y
      file.nm<-paste(file.path, file.nm.pre, year, ".nc", sep="")
      data<-brick(file.nm, varname=varname)
      
      # set up monthly aggregation indices (include extra day for leap year)
      if(nlayers(data) == 365){
        month.lengths<-c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)  
      }else if(nlayers(data) == 366){
        month.lengths<-c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
      }
      
      ind<-mat.or.vec(nr = sum(month.lengths), nc=1)
      for(m in 1:12){
        month.start.day = (1 + sum(month.lengths[1:m]) - month.lengths[m])
        month.end.day   = (sum(month.lengths[1:m]))
        ind[month.start.day:month.end.day] = m
      }
      
      # cumulative aggregation
      if(cumulative==1){
        data.month<-stackApply(data, ind, sum, na.rm=FALSE)
        if(unit == 'mm/day'){
          varunit<-c('mm/month')
        }
        
        # average aggregation (average daily value over the month)
      }else{  
        data.month<-stackApply(data, ind, mean, na.rm=FALSE)
        if(unit == 'mm/day'){
          varunit<-c('mm/day')
        }
      }
      
      out.file<-paste(out.dir, out.name, varname, "_", year, ".nc", sep="")
      writeRaster(data.month, out.file, format="CDF", varname = varname, varunit = varunit, 
                  xname = 'latitude', yname = 'longitude', zname = 'time', zunit='months',
                  overwrite=over.write)
         
    print(year)
    } # close year loop    
  } # close if loop
  
  #########################################################################
  ### daily OR monthly to yearly aggregation
  #########################################################################
  if(agg.to == 'yearly'){
    for(y in start.year:end.year){
      year=y
      file.nm<-paste(file.path, file.nm.pre, year, "_.nc", sep="")
      data<-brick(file.nm, varname=varname)
      
      # cumulative aggregation
      if(cumulative==1){
        data.year<-calc(data, sum, na.rm=FALSE)
        if(unit == 'mm/day' | unit == 'mm/month'){
          varunit<-c('mm/year')
        }
        
        # average aggregation (average daily value over the month)
      }else{  
        data.year<-calc(data, mean, na.rm=FALSE)
        varunit = unit
      }
      
      out.file<-paste(out.dir, out.name, varname, "_", year, ".nc", sep="")
      writeRaster(data.year, out.file, format="CDF", varname = varname, varunit = varunit, 
                  xname = 'latitude', yname = 'longitude', zname = 'time', zunit='year',
                  overwrite=over.write)
      print(year) 
    } # close year loop
  } # close if loop
 
  #########################################################################
  ### monthly to monthly_clim aggregation
  #########################################################################
  if(agg.from == 'monthly' & agg.to == 'monthly_clim'){
    for(y in start.year:end.year){
      year=y
      if(y == start.year){
        file.nm<-paste(file.path, file.nm.pre, "_", varname, "_", year, ".nc", sep="")
        data<-brick(file.nm, varname=varname)
      } else if(y > start.year){
        file.nm<-paste(file.path, file.nm.pre, "_", varname, "_", year, ".nc", sep="")
        data.new<-brick(file.nm, varname=varname)
        data<-stack(data, data.new)
      }
      print(year)
    }
    
    n.yrs = end.year - start.year + 1
    m.ind=seq(1:12)
    ind<-rep(m.ind, n.yrs)
    
    monthly.clim<-stackApply(data, ind, mean, na.rm=FALSE)
    varunit = unit
    
    out.file<-paste(out.dir, out.name, varname, "_mc.nc", sep="")
    writeRaster(monthly.clim, out.file, format="CDF", varname = varname, varunit = varunit, 
                xname = 'latitude', yname = 'longitude', zname = 'time', zunit='monthly_clim',
                overwrite=over.write)

  }

  #########################################################################
  ### yearly to yearly_clim aggregation
  #########################################################################
  if(agg.from == 'yearly' & agg.to == 'yearly_clim'){
    for(y in start.year:end.year){
      year=y
      if(y == start.year){
        file.nm<-paste(file.path, file.nm.pre, "_", varname, "_", year, ".nc", sep="")
        data<-brick(file.nm, varname=varname)
      } else if(y > start.year){
        file.nm<-paste(file.path, file.nm.pre,  "_", varname, "_", year, ".nc", sep="")
        data.new<-brick(file.nm, varname=varname)
        data<-stack(data, data.new)
      }
      print(year)
    }
    
    yearly.clim<-calc(data, mean, na.rm=FALSE)
    varunit = unit
    
    out.file<-paste(out.dir, out.name, varname, "_yc.nc", sep="")
    writeRaster(yearly.clim, out.file, format="CDF", varname = varname, varunit = varunit, 
                xname = 'latitude', yname = 'longitude', zname = 'time', zunit='yearly_clim',
                overwrite=over.write)
  }  
} # close function