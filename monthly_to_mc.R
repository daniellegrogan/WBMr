# monthly_to_mc()

# Calculate monthly climatologies from a time series of monthly values (dataframe format). NB: this is NOT for rasters
# Monthly values taken from spatial aggregation
# Input data format:
  # One column per month
  # column name of format: XYYYY.MM.DD
# assumes that if the data frame has a first column with a non-date column name, this is a list of basins and will give the output a first column with column name "Basin"

################################################################################################################
monthly_to_mc = function(data.months, years, out.nm=NA){
 
   # subset to defined sequence of years 
   yr = as.numeric(substr(colnames(data.months), start=2, stop=5))
  
  if(is.na(yr[1])){
    yr = yr[2:length(yr)]
    agg.names = data.months[,1]
    data.months = data.months[,2:ncol(data.months)]
  }else{
    agg.names = NA
  }
  data.months.sub = subset(data.months, select=c(yr %in% years))
   
  
  # calculate monthly climatology on subset
  month.num = as.numeric(lapply(X = colnames(data.months.sub), FUN = function(x){unlist(strsplit(x, "\\."))[2]})) 

  month.mean  = data.frame(matrix(nr=nrow(data.months.sub), nc=12))
  month.stdev = data.frame(matrix(nr=nrow(data.months.sub), nc=12))
  
  for(m in 1:12){
    data.m = data.months.sub[, month.num==m]
    month.mean[,m]  = rowMeans(data.m)
    month.stdev[,m] = apply(data.m, c(1), sd)
  }
  
  month.names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  colnames(month.mean)  = paste(month.names, "mean",  sep="_")
  colnames(month.stdev) = paste(month.names, "stdev", sep="_")
  
  if(is.na(agg.names[1]) == T){
    out = as.data.frame(cbind(month.mean, month.stdev))
  }else{
    out = cbind(as.character(agg.names), month.mean, month.stdev)
    colnames(out)[1] = "Basin"
  }
  
  # write data to file if an out.nm is given
  if(is.na(out.nm) == F){
    write.csv(out, out.nm, quote = F, row.names = F)
    print(out.nm)
  }

  out
}
################################################################################################################
