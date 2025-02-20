# Init functions:
# These functions help to initialize the project without
# leaving artifacts in the workspace



#####################################################
#Run all scripts in a directory
run.script <- function(dir.name){
  #check whether directory exists
  if(dir.exists(dir.name)){
    if(!is.null(dir(dir.name,pattern = ".R"))){
      invisible(lapply(dir(dir.name,pattern = ".R",full.names = T),source))
    }
  } else {
    stop("Invalid directory name")
  }
}
#unit test
#run.script("functions")




#This function builds out the folder structure
folder.setup <- function(slink){
  folder.list <- c("build/code",
                   "build/cache",
                   "analysis/inputs",
                   "analysis/code",
                   "analysis/cache",
                   "analysis/outputs")
  
  lapply(folder.list,
      function(x){
        if(!dir.exists(x)){
          dir.create(x,recursive = T)
          message(str_c("The ",x," folder has been created."))
        } else {
          message(str_c("The ",x," folder already exists."))
        }
      })
  
  if(!dir.exists("build/inputs") & !missing(slink)) system(paste0("ln -s ",slink," build/inputs"))
  
  return(NULL)
}

#Short function to create a dir if it does not exist
dir_ifnot <- function(dir_name){
  if(!dir.exists(dir_name)) dir.create(dir_name,recursive = T)
}

#Function to download and cache census files or load them if cached
load_or_dl <- function(url,destdir){
  #download.file(url = url,destfile = paste0(destdir,"/",basename(url)))
  require(tools)
  target_file <- paste0(destdir,"/",file_path_sans_ext(basename(url)),".rds")
  
  if(!file.exists(target_file)) {
    temp <- read_delim(url,delim = "|")
    saveRDS(temp,file = target_file)
  } else {
    temp <- readRDS(target_file)
  }
  
  return(temp)
}

