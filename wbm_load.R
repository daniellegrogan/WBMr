function(path, varname, years = NA){
  # path     = character string; file path to model output
  # varname  = character string; variable name to load 
  # years    = vector or NA; 
  #            vector: sequence of years of model data to read if you don't want to read ALL files in file path

  
    if(sum(is.na(years)) == 1){
      file.list = list.files(path = path, full.names = T)
    }else{
      file.list.full = list.files(path = path, full.names = T)
      file.list = file.list.full[unlist(sapply(years, FUN = function(x) grep(pattern=paste("wbm_",x, sep=""), file.list.full)))]
    }

  wbm.all = terra::rast(file.list) # loads all variables
  
  # wbm.all will include "_sigma" vars.  Subset to only vars of interest:
  if(grepl("yearly", path)){
    wbm.out = wbm.all[[which(names(wbm.all) == varname)]]  # subsets to only variable of interest
  }else if(grepl("monthly", path)){
    wbm.out = wbm.all[[which(names(wbm.all) %in% paste(varname, seq(1,12), sep="_"))]]  # subsets to only variable of interest
  }else if(grepl("daily", path)){
    wbm.out = wbm.all[[which(names(wbm.all) %in% paste(varname, seq(1,365), sep="_"))]]  # subsets to only variable of interest
  }

  return(wbm.out)
}
