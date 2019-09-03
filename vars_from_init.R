# vars_from_init()
# Function to get list of WBM output variables from a WBM init file

vars_from_init = function(init.file.path){  # init.file.path = character string of full file path 
  
  # read wbm init file 
  ini = as.data.frame(readIniFile(init.file.path))
  
  var.row = which(ini$name == "Output_vars")                      # idenfity row with output variables
  vars = ini$value[var.row]                                       # read row of output variables
  v1 = strsplit(as.character(vars), "[, ]+")                      # split into multiple strings
  v2 = unlist(lapply(v1, FUN = function(x) sub("\\'", "", x)))    # remove leading and trailing single quotes
  v3 = subset(v2, grepl("([[:alpha:]])", v2))                     # remove empty or non-alpha numeric entries (e.g., "<")
  
  return(v3)
}
