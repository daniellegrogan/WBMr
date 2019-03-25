# Select USGS data from gages based on upstream area
# Danielle S Grogan
# 2018-10-22

library(dataRetrieval) # functions to access USGS online data
library(lubridate)     # date functions
library(rgeos)
library(rgdal)
library(raster)

#######################################################################################################
full_ts = function(date.list, min.yrs){
  # checks if there is a min.yrs+ year time series of continuous daily data
  # outputs F if there is not
  # outputs the dates of the longest (first if multiple equal) min.yrs+ year daily time series
  
  d.check = date.list[2:length(date.list)]
  date.list = date.list[1:length(date.list) - 1]
  diff.time = as.numeric(ymd(d.check)) - as.numeric(ymd(date.list))

  if(sum(diff.time > 1) > 0){
    gaps = which(diff.time > 1)
    length.gaps = diff(gaps)
    yrs.10 = (max(length.gaps) >= 365*min.yrs)
    
    if(yrs.10 == T){
      x = which((length.gaps) == max(length.gaps))
      max.gap = x[length(x)]
      start = gaps[max.gap]
      end = gaps[max.gap + 1]
      
      out.dates = date.list[start:end]
    }else{
      out.dates = F
    }
  }else{
    out.dates = date.list
  }
  
  out.dates
}

#######################################################################################################

get_USGS = function(huc.list, 
                    grid.area   = max(as.matrix(raster::area(raster("/net/nfs/merrimack/raid/data/WBM_USNE/livneh/climatology/wbm_discharge_yc.nc")))),
                    serv        = 'dv',
                    mod.crs     = crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"),
                    ex          = extent(-81, -66.8, 37.8, 48.1),
                    min.cells   = 1,
                    q.out.dir   = "/net/nfs/yukon/raid5/projects/Vernal_Windows/data/USGS/Q_daily/",
                    parCD       = "00060",
                    startDate   = "1950-01-01",
                    endDate     = "2013-12-31",
                    min.yrs     = 10,
                    shp.out.dir = "/net/nfs/yukon/raid5/projects/Vernal_Windows/data/USGS/station_shp"
                    
                    # grid.area   = float; an area in km2
                    # serv        = character string; input to whatNWISsites() identifying temporal resolution of USGS data. 'dv' := dalily values
                    # mod.crs     = crs object; projection of output from whatNWISsites(). 
                    # ex          = extent object; spatial extent over which to download USGS data
                    # min.cells   = integer; a minimum upstream area size for USGS gages, expressed as a number of WBM grid cells
                    # q.out.dir   = character string; output directory for csv files of USGS gage data
                    # parCD       = character string; input to whatNWISsites() identifying the time of data. "00060" := discharge
                    # startDate   = character string in Lubridate date format; earliest date of USGS data to download
                    # endDate     = character string in Lubridate date format; latest date of USGS data to download
                    # min.yrs     = integer; minimum years of continuous data from USGS. Gages with < min.yrs continuous data are not saved.
                    # shp.out.dir = character string; output directory for spatial point file of gage locations
                    
                    # Given a spatial extent object (ex), a date range (startDate and endDate), 
                    # a minimum number of years (min.yrs), and a minimum number of WBM cells (min.cells), this function will:
                    # 1. Write USGS gage data as csv files to q.out.dir.  Includes unit conversion to m3/s. One file per station.
                    # 2. Write USGS gage locations as a spatial point file to the shp.out.dir
){
  
  # Download metadata from USGS server by huc
  for(h in 1:length(huc.list)){
    huc.data = whatNWISsites(parameterCD=parCD, huc=huc.list[h], service=serv)
    coordinates(huc.data)<- ~ dec_long_va + dec_lat_va
    crs(huc.data) = mod.crs
    huc.data = crop(huc.data, ex)
    
    # combine spatial points from each huc into one SpatialPointsDataFrame
    if(h == 1){
      q.stations = huc.data
    }else{
      q.stations = rbind(q.stations, huc.data)
    }
  }
  
  # Remove stations with drainage area < min.cells wbm grid cells
  st.att = readNWISsite(q.stations$site_no)
  min.area = min.cells * grid.area # minimum drainage area = min.cells x wbm grid cell (unit = km2)
  
  # convert USGS drainage area to km2
  d.area.mi2 = st.att$drain_area_va
  d.area.km2 = d.area.mi2 * 2.58999
  st.att$d.ara.km2 = d.area.km2
  st.keep = subset(st.att, st.att$d.ara.km2 > min.area)
  
  q.stations = subset(q.stations, q.stations$site_no %in% st.keep$site_no)
  
  text = paste("# of USGS stations:", nrow(st.keep))
  print(text)
  
  # download and save discharge data from selected stations (q.stations)
  cfs_to_m3s = 0.0283168
  
  count = 0
  for(i in 1:nrow(q.stations)){
    discharge <- readNWISdv(q.stations$site_no[i],
                            parCD, 
                            startDate, 
                            endDate)
    
    if(nrow(discharge) > 365*min.yrs){ # only use sites that have the potential to have min.yrs years of continuous daily data
      new.ts = full_ts(discharge$Date, min.yrs) # only use sites with at least min.yrs years of continuous daily data
      discharge = subset(discharge, discharge$Date %in% new.ts)
      
      if(length(new.ts) > 1){
        count = count + 1
        text = paste("stations kept: ", count)
        print(text)
        
        q.stations$full_ts[i] = 1 # idenfiy which stations have a full time series in the point file
        discharge$q_m3s = discharge[,4] * cfs_to_m3s
        write.table(discharge, paste(q.out.dir, q.stations$site_no[i], ".csv", sep=""), row.names=F, sep=",")
      }else{
        q.stations$full_ts[i] = 0
      }
    }else{
      q.stations$full_ts[i] = 0
    }
    text = paste("downloading station", i, "out of", nrow(q.stations))
    print(text)
  }
  q.stations = subset(q.stations, q.stations$full_ts == 1) # subset point file to full time series
  
  # save point file
  writeOGR(q.stations, 
           dsn = shp.out.dir, 
           layer = "station_shp", 
           driver="ESRI Shapefile",
           overwrite_layer = T)
  
} # end get_USGS function