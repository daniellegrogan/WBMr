# yearly_to_yc()

# Input a time series data frame of yearly values, and output yearly climatology for a given set of years
# assumes column header in format: XYYYY.MM.DD (typical output of spatial aggregation)

# dataframe format; NB: this is NOT for rasters

################################################################################################################
yearly_to_yc = function(data.y, years, out.nm=NA){
  yr = as.numeric(substr(colnames(data.y), start=2, stop=5))
  
  if(is.na(yr[1])){
    yr = yr[2:length(yr)]
    agg.names = data.y[,1]
    data.y = data.y[,2:ncol(data.y)]
  }else{
    agg.names = NA
  }
  
  data.y.sub = subset(data.y, select=c(yr %in% years))
  data.yc = rowMeans(data.y.sub)
  data.sd = apply(data.y.sub, MARGIN=1, FUN=sd)
  
  if(is.na(agg.names[1]) == T){
    out = cbind(data.yc, data.sd)
    colnames(out) = c("Mean", "Stdev")
  }else{
    out = cbind(as.character(agg.names), data.yc, data.sd)
    colnames(out) = c("Basin", "Mean", "Stdev")
  }
  
  if(is.na(out.nm) == F){
    write.csv(out, out.nm, quote = F, row.names = F)
    print(out.nm)
  }
  out
}
################################################################################################################

