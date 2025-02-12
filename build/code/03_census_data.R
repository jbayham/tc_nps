#This script constructs a database of ACS datatables used in this analysis

library(pacman)
p_load(tidyverse,janitor,arrow,readxl)

conflicts_prefer(dplyr::filter)

source("project_init.R")

####################
#Get ACS data from FTP
tab_names <- c("b01001", #pop
               "b01002", #age
               "b06009", #education
               "b19013", #hh med income
               "b25010") #hh size



acs_geo_ref <- read_delim("https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/documentation/Geos20225YR.txt",
                          delim = "|")
tract_subset <- acs_geo_ref %>%
  filter(!is.na(TRACT)) %>%
  #select(FILEID:COMPONENT,STATE,COUNTY,TRACT,GEO_ID) %>%
  select(GEO_ID)


yr_list=c(2023)

for(yr in yr_list){
  
  #base url for 5yr
  ftp_base <- paste0("https://www2.census.gov/programs-surveys/acs/summary_file/",yr,"/table-based-SF/data/5YRData/acsdt5y",yr)
  
  #Population
  pop_dat <- read_delim(paste0(ftp_base,"-b01001.dat"),
                        delim = "|") %>%
    select(GEO_ID,total_pop = B01001_E001) %>% #select only total pop
    inner_join(tract_subset) 
  
  #Median Age
  age_dat <- read_delim(paste0(ftp_base,"-b01002.dat"),
                        delim = "|") %>%
    select(GEO_ID,med_age = B01002_E001) %>% #select only total pop
    inner_join(tract_subset) 
  
  #Education - % bachelors
  bach_dat <- read_delim(paste0(ftp_base,"-b06009.dat"),
                         delim = "|") %>%
    select(GEO_ID,B06009_E001,B06009_E005,B06009_E006) %>% #select only total pop
    inner_join(tract_subset) %>%
    mutate(bach_perc = (B06009_E005+B06009_E006)/B06009_E001) %>%
    select(GEO_ID,bach_perc)
  
  #Median household income
  med_hh_inc_dat <- read_delim(paste0(ftp_base,"-b19013.dat"),
                               delim = "|") %>%
    select(GEO_ID,med_inc = B19013_E001) %>% #select only total pop
    inner_join(tract_subset)
  
  #Household size
  hh_size_dat <- read_delim(paste0(ftp_base,"-b25010.dat"),
                            delim = "|") %>%
    select(GEO_ID,hh_size = B25010_E001) %>% #select only total pop
    inner_join(tract_subset) 
  
  
  tract_dat <- pop_dat %>%
    left_join(age_dat,by = join_by(GEO_ID)) %>%
    left_join(bach_dat,by = join_by(GEO_ID)) %>%
    left_join(med_hh_inc_dat,by = join_by(GEO_ID)) %>%
    left_join(hh_size_dat,by = join_by(GEO_ID)) %>%
    mutate(GEO_ID = str_remove(GEO_ID,"1400000US")) %>%
    rename(geoid=GEO_ID)
  
  #################################
  saveRDS(tract_dat,paste0("build/cache/census_data_",yr,".rds"))
}




################################################

if(!dir.exists("build/cache/census_geo")) dir.create("build/cache/census_geo")
if(!dir.exists("build/cache/census_geo/temp")) dir.create("build/cache/census_geo/temp")
st_list <- unique(str_extract(tract_subset$GEO_ID,"(?<=US)\\d{2}"))

st=st_list[1]
yr=2023

tract_geo <- 
  map(st_list,
      function(st){
        fname=paste0("build/cache/census_geo/tl_",yr,"_",st,"_tract")
        
        if(!file.exists(paste0(fname,".zip"))){
          download.file(url = paste0("https://www2.census.gov/geo/tiger/TIGER",yr,"/TRACT/tl_",yr,"_",st,"_tract.zip"),
                        destfile = paste0(fname,".zip"))
        }
        
        if(!file.exists(paste0(fname,".shp"))){
          unzip(zipfile = paste0(fname,".zip"),exdir = "build/cache/census_geo/")
        }
        
        tig_temp <- st_read(paste0(fname,".shp")) %>%
          clean_names() %>%
          select(geoid,aland,awater) %>%
          st_cast('MULTIPOLYGON') #converts all to multipolygon because some are polygon and others are multi
        
        return(tig_temp)
        
      }) %>%
  data.table::rbindlist() %>% 
  st_as_sf()

saveRDS(tract_geo,paste0("build/cache/census_geo_",yr,".rds"))

tract_geo %>%
  st_centroid(of_largest_polygon = TRUE) %>%
  sfc_as_cols(.,names = c("longitude","latitude")) %>%
  st_drop_geometry() %>%
  saveRDS(paste0("build/cache/census_geo_points_",yr,".rds"))
