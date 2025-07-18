#This script constructs a database of ACS datatables used in this analysis

library(pacman)
p_load(tidyverse,conflicted,janitor,arrow,readxl)

conflicts_prefer(dplyr::filter)

source("project_init.R")

####################
#Get ACS data from FTP

dir_ifnot("build/cache/census")




#Getting geo reference file used to subset tract data
acs_geo_ref <- load_or_dl(url="https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/documentation/Geos20225YR.txt",
                          destdir = "build/cache/census")
tract_subset <- acs_geo_ref %>%
  filter(!is.na(TRACT)) %>%
  select(GEO_ID)

#Getting xwalk between 2010 and 2020 data
xwalk <- load_or_dl(url = "https://www2.census.gov/geo/docs/maps-data/data/rel2020/tract/tab20_tract20_tract10_natl.txt",
                    destdir = "build/cache/census") %>%
  clean_names() %>%
  select(geoid_tract_20,arealand_tract_20,areawater_tract_20,
         geoid_tract_10,arealand_tract_10,areawater_tract_10,
         arealand_part,areawater_part)


yr_list=c(2022,2023)
yr=2023

for(yr in yr_list){
  
  #base url for 5yr
  ftp_base <- paste0("https://www2.census.gov/programs-surveys/acs/summary_file/",yr,"/table-based-SF/data/5YRData/acsdt5y",yr)
  
  #For each file, read in and cache
  
  
  #Population
  pop_dat <- load_or_dl(url=paste0(ftp_base,"-b01001.dat"),
                        destdir = "build/cache/census") %>%
    select(GEO_ID,total_pop = B01001_E001) %>% #select only total pop
    inner_join(tract_subset) 
  
  #Median Age
  age_dat <- load_or_dl(url=paste0(ftp_base,"-b01002.dat"),
                        destdir = "build/cache/census") %>%
    select(GEO_ID,med_age = B01002_E001) %>% #select only total pop
    inner_join(tract_subset) 
  
  #Education - % bachelors
  bach_dat <- load_or_dl(url=paste0(ftp_base,"-b06009.dat"),
                         destdir = "build/cache/census") %>%
    select(GEO_ID,B06009_E001,B06009_E005,B06009_E006) %>% #select only total pop
    inner_join(tract_subset) %>%
    mutate(bach_perc = (B06009_E005+B06009_E006)/B06009_E001) %>%
    select(GEO_ID,bach_perc)
  
  #Median household income
  # med_hh_inc_dat <- load_or_dl(url=paste0(ftp_base,"-b19013.dat"),
  #                              delim = "|") %>%
  #   select(GEO_ID,med_inc = B19013_E001) %>% #select only total pop
  #   inner_join(tract_subset)
  
  med_inc_dat <- load_or_dl(url=paste0(ftp_base,"-b19301.dat"),
                               destdir = "build/cache/census") %>%
    select(GEO_ID,med_inc = B19301_E001) %>% #select only total pop
    inner_join(tract_subset)
  
  #Household size
  hh_size_dat <- load_or_dl(url=paste0(ftp_base,"-b25010.dat"),
                            destdir = "build/cache/census") %>%
    select(GEO_ID,hh_size = B25010_E001) %>% #select only total pop
    inner_join(tract_subset) 
  
  #Assembling census data
  tract_dat <- pop_dat %>%
    left_join(age_dat,by = join_by(GEO_ID)) %>%
    left_join(bach_dat,by = join_by(GEO_ID)) %>%
    left_join(med_inc_dat,by = join_by(GEO_ID)) %>%
    left_join(hh_size_dat,by = join_by(GEO_ID)) %>%
    mutate(GEO_ID = str_remove(GEO_ID,"1400000US")) %>%
    rename(geoid=GEO_ID)
  
  #Caching
  saveRDS(tract_dat,paste0("build/cache/census_data_",yr,".rds"))
  ################################
  # The Advan data was built on 2010 census
  #tracts so we use a crosswalk to approximate ACS values for tracts that no
  #longer exist by using a weighted average of the new tracts the old one
  #became. 
  #Map the data (2020 tracts) to the 2010 tracts reported by Advan
  xwalk_data <- xwalk %>%
    inner_join(tract_dat,by = c("geoid_tract_20"="geoid")) %>%
    mutate(weights = arealand_part/arealand_tract_10) %>%
    group_by(geoid_tract_10) %>%
    summarize(across(c(total_pop:hh_size),~sum(weights*.))) %>%
    ungroup() %>%
    rename(tract=geoid_tract_10)
  
  saveRDS(xwalk_data,paste0("build/cache/xwalk_data_",yr,".rds"))
}




################################################
#Constructing database of geographies

if(!dir.exists("build/cache/census_geo")) dir.create("build/cache/census_geo")
st_list <- unique(str_extract(tract_subset$GEO_ID,"(?<=US)\\d{2}"))

st=st_list[1]
yr=2019

tract_geo <- 
  map(st_list,
      function(st){
        fname=paste0("build/cache/census_geo/tl_",yr,"_",st,"_tract")
        
        #Downloading tiger file
        if(!file.exists(paste0(fname,".zip"))){
          download.file(url = paste0("https://www2.census.gov/geo/tiger/TIGER",yr,"/TRACT/tl_",yr,"_",st,"_tract.zip"),
                        destfile = paste0(fname,".zip"))
        }
        
        #Unzipping
        if(!file.exists(paste0(fname,".shp"))){
          unzip(zipfile = paste0(fname,".zip"),exdir = "build/cache/census_geo/")
        }
        
        #Read in and make multipolygon
        tig_temp <- st_read(paste0(fname,".shp")) %>%
          clean_names() %>%
          select(geoid,aland,awater) %>%
          st_cast('MULTIPOLYGON') #converts all to multipolygon because some are polygon and others are multi
        
        return(tig_temp)
        
      }) %>%
  data.table::rbindlist() %>% 
  st_as_sf()

saveRDS(tract_geo,paste0("build/cache/census_geo_",yr,".rds"))

#Convert to lat lons for distance calculations
tract_geo %>%
  st_centroid(of_largest_polygon = TRUE) %>%
  sfc_as_cols(.,names = c("longitude","latitude")) %>%
  st_drop_geometry() %>%
  saveRDS(paste0("build/cache/census_geo_points_",yr,".rds"))


###########
#Clean directories
list.files("build/cache/census_geo",full.names = TRUE) %>%
  file.remove()


  
    
#################################
#Grabbing zipcode population for AE

yr=2022

geos <- readRDS("build/cache/census/Geos20225YR.rds")

zips <- geos %>%
  filter(!is.na(ZCTA5)) %>%
  select(GEO_ID,ZCTA5,NAME)

pop_dat_22 <- readRDS(paste0("build/cache/census/acsdt5y",2022,"-b01001.rds"))
pop_dat_23 <- readRDS(paste0("build/cache/census/acsdt5y",2023,"-b01001.rds"))

zcta_pop_2022 <- pop_dat_22 %>%
  select(GEO_ID,pop_2022=B01001_E001) %>%
  inner_join(zips,.,by="GEO_ID")

zcta_pop_2023 <- pop_dat_23 %>%
  select(GEO_ID,pop_2023=B01001_E001) %>%
  inner_join(zips,.,by="GEO_ID")

zcta_pop <- inner_join(zcta_pop_2022,zcta_pop_2023,by = join_by(GEO_ID, ZCTA5, NAME))

saveRDS(zcta_pop,"build/cache/zcta_pop.rds")

###########################
#downloading and processing all 2020 zipcode geometries

fname=paste0("build/cache/census_geo/tl_2024_us_zcta520")

#Downloading tiger file
if(!file.exists(paste0(fname,".zip"))){
  download.file(url = paste0("https://www2.census.gov/geo/tiger/TIGER2024/ZCTA520/tl_2024_us_zcta520.zip"),
                destfile = paste0(fname,".zip"))
}

#Unzipping
if(!file.exists(paste0(fname,".shp"))){
  unzip(zipfile = paste0(fname,".zip"),exdir = "build/cache/census_geo/")
}

#Read in and make multipolygon
tig_temp <- st_read(paste0(fname,".shp")) %>%
  clean_names() %>%
  select(geoid20,aland20,awater20) %>%
  st_cast('MULTIPOLYGON')

tig_temp %>%
  st_centroid(of_largest_polygon = TRUE) %>%
  sfc_as_cols(.,names = c("longitude","latitude")) %>%
  st_drop_geometry() %>%
  saveRDS(file = "build/cache/census_geo_zip_2024.rds")



