#This script initializes the project and should be run at the beginning of each
#session

#########################
#Load init functions
source("functions/init_functions.R")

#Loading and installing packages
library(pacman)
p_load(char=c("tidyverse",      #shortcut to many useful packages (eg, dplyr, ggplot)
            "conflicted",     #resolves function conflict across packages
            "sf",             #for GIS
            "readxl"
))


#Setting package::function priority with conflicted package
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
#########################
#Loading project helper functions (all scripts within folder)
run.script("functions")


##########################################
##########################################
#This function sets up the directory structure with an options 
#to create a symbolic link to the inputs folder that may be shared across several users
comp = Sys.info()

if(comp["nodename"]=="Judes-MacBook-Pro-2.local"){
  folder.setup(slink="")#local setup
} else {
  folder.setup(slink="/RSTOR/bayham/projects/tc_nps/inputs")
}


#Subset of pois with verified locations and known visitor histories
park_subset <- read_excel("build/inputs/park_ref.xlsx") %>%
  select(park:dest_lat)

saveRDS(park_subset,"build/cache/park_subset.rds")

#dlgMessage("Do you need to pull the repo?")
