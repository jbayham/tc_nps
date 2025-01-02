#This script uses tidycensus to compile tract demographics and location of all origin tracts

library(pacman)
p_load(tidyverse,sf,tidycensus,janitor)

source("project_init.R")
################################################
#Parameters

#Income percentile adjustment - 0.68 from survey data analysis/code/03_mobile_pop_compare.R
#inc_percentile = .68

################################################
#Read data
origins <- read_csv("build/cache/parks_home_tract.csv") %>%
  distinct(tract) #since it doesn't matter which park

# zip_origins <- read_csv("build/inputs/survey_data/survey_final.csv") %>%
#   distinct(zipcode)

#tigris pulls all tracts from county so it makes sense to break the data into counties, then call and merge the tracts
tract_counties <- origins %>%
  filter(!str_detect(tract,"CA")) %>% #remove Canada
  mutate(fips_state=str_sub(tract,1,2),  #extract county and state fips from census tract id
         fips_county=str_sub(tract,3,5)) %>%
  distinct(fips_state,fips_county) %>% #remove duplicates since the dataset is static
  arrange(fips_state)

#Check available variables
#v20 <- load_variables(2020, "acs5", cache = TRUE)

############################################################
#Create directory to cache temp files if it does not exist
dir_name="build/cache/tract_data"
if(!dir.exists(dir_name)) dir.create(dir_name)

#Check files already cached
tract_data_cached <- list.files(dir_name) %>%
  str_extract("[:digit:]{5}") %>%
  enframe(value = "geoid",name = NULL) %>%
  transmute(fips_state=str_sub(geoid,1,2),
            fips_county=str_sub(geoid,3,5))

#Remove from the list to process
tract_counties_remain <- tract_counties %>%
  anti_join(tract_data_cached) %>%
  drop_na()

#Loop through counties and download the census data as well as tract polygons for mapping and google calculation
#i=1
#census_vars <- load_variables(2022,dataset = "acs5")
#B06011_001: MEDIAN INCOME IN THE PAST 12 MONTHS (IN 2020 INFLATION-ADJUSTED DOLLARS)
#B19013_001: MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2020 INFLATION-ADJUSTED DOLLARS)
#B19001: Table with median income distributions
#B01001_001: Total pop
#B01002_001: median age
#B02001_002: white pop
#B06009_00*: education
#C18108_00*: disabilities (data pulled is without disabilities so need to subtract)
q_cen_vars <- c("B01001_001", #total pop
                "B01002_001", #median age
                "B02001_002", #race: white alone
                "B06011_001", #individual income
                paste0("B19001","_",str_pad(c(1:17),3,"left","0")), #all household income vars
                "B19013_001", #median income
                paste0("B06009","_",str_pad(c(1:6),3,"left","0")), #education (pop>25 years old)
                "C18108_005","C18108_009","C18108_013") #numbers without disability (sum to total pop)

for(i in 1:nrow(tract_counties_remain)){
  census_temp <- tryCatch({
    temp <- get_acs(geography = "tract", 
                    variables = q_cen_vars, #numbers without disability (sum to total pop)
                    state = tract_counties_remain$fips_state[i],
                    county = tract_counties_remain$fips_county[i],
                    geometry = TRUE, #ensure that tigris grabs polygons
                    survey = "acs5", 
                    year = 2022,
                    key = Sys.getenv("CENSUS_API_KEY"))
    
    
  }, error = function(e) {
    message(paste("Error in iteration", i, ":", e$message))
    NULL  # Return NULL on error to skip that iteration
  })
  
  if (is.null(census_temp)) next
  
  # If successful, format and cache results
  out <- census_temp %>%
    st_cast('POLYGON') %>%  #cast to polygons (from multipolygon)
    mutate(poly_wkt=st_as_text(st_geometry(.)), #convert sf object to text for caching in csv
           area=st_area(.)) %>%   #calculate area in case a single tract includes small satellite polygons
    st_centroid(of_largest_polygon = TRUE) %>% #extract polygon centroid (largest is irrelevant since casting to polygon)
    sfc_as_cols(names = c("longitude","latitude")) %>% #convert coordinates to dataframe and append to sf object
    st_set_geometry(NULL) #convert sf object to dataframe
  
  
  write_csv(out,paste0("build/cache/tract_data/census_data_",tract_counties_remain$fips_state[i],tract_counties_remain$fips_county[i],".csv"))
  
  Sys.sleep(sample(1:5,1)) #build in delays to avoid server timeouts
}



#################################
#Process the data downloaded from the census - removing duplicates based on area
#glimpse(read_csv(dir("build/cache/tract_data",full.names = T,pattern = "census_data_")[1]))
census_cached <- dir("build/cache/tract_data",full.names = T,pattern = "census_data_") %>%
  map_dfr(~read_csv(.,col_types = 'cccddcddd')) %>%
  clean_names() 

census_raw <- census_cached %>%
  select(-c(name,poly_wkt,area,longitude,latitude)) %>%
  mutate(geo="tract") %>%
  distinct(geoid,variable,.keep_all = T)

# census_raw %>%
#   select(geoid,area,longitude,latitude) %>%
#   group_by(geoid) %>%
#   slice_max(order_by=area,n=1)


#Process education - % with Bachelors degree
census_educ <- census_raw %>%
  filter(str_detect(variable,"B06009")) %>%
  select(-c(moe)) %>%
  pivot_wider(names_from = "variable",values_from = "estimate") %>%
  mutate(bach_degree_perc = (B06009_005 + B06009_006)/B06009_001) %>%
  filter(B06009_001!=0) %>% #cut out locations with no pop 
  select(geoid,bach_degree_perc)


#Process disabilities - % of non-institutionalized pop with disability (the data is without disability by age categories)
census_dis <- census_raw %>%
  filter(str_detect(variable,"C18108|B01001_001")) %>%
  select(-c(moe)) %>%
  pivot_wider(names_from = "variable",values_from = "estimate") %>%
  mutate(disable_perc = (B01001_001 - C18108_005 - C18108_009 - C18108_013)/B01001_001) %>%
  filter(B01001_001!=0) %>% #cut out locations with no pop (non-institutionalized)
  select(geoid,disable_perc)

summary(census_dis$disable_perc) #this figure is consistent with data here: https://www.census.gov/newsroom/facts-for-features/2021/disabilities-act.html

#Process % white - % 
census_white <- census_raw %>%
  filter(str_detect(variable,"B02001_002|B01001_001")) %>%
  select(-c(moe)) %>%
  pivot_wider(names_from = "variable",values_from = "estimate") %>%
  mutate(white_perc = B02001_002/B01001_001) %>%
  filter(B01001_001!=0) %>% #cut out locations with no pop (non-institutionalized)
  select(geoid,white_perc)

summary(census_white$white_perc)


#######################################

#Median household income and age + join back with other vars
census_tract <- census_raw %>%
  filter(variable %in% c("B19013_001","B01001_001","B01002_001")) %>% #household income
  select(-c(moe)) %>%
  pivot_wider(names_from = "variable",values_from = "estimate") %>%
  rename(tract=geo,med_hh_inc=B19013_001,total_pop=B01001_001,med_age=B01002_001) %>%
  left_join(census_educ, by = "geoid") %>%
  left_join(census_dis, by = "geoid") %>%
  left_join(census_white, by = "geoid") 


#cache census dataset
write_csv(census_tract,"build/cache/census_data.csv")
#########################

census_geo <- census_cached %>%
  distinct(geoid,longitude,latitude) 

write_csv(census_geo,"build/cache/census_geo.csv")
