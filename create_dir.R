# create_dir()
# check if output directory exists.  If not, create it

create_dir = function(out.path){
  f1 = file.path(strsplit(out.path, "/")[[1]][1], strsplit(out.path, "/")[[1]][2])
  if(!file.exists(f1)){
    dir.create(f1)
  }
  if(!file.exists(out.path)){
    dir.create(out.path)
  }
}

