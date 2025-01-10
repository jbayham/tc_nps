#Get travel distance and time

#install.packages("mapsapi")
library(pacman)
p_load(tidyverse,sf,mapsapi,janitor,measurements,progress,readxl)

source("project_init.R")

#Set google API key
key = Sys.getenv("GOOGLE_MAPS_API_KEY")

if(!dir.exists("build/cache/google_dist")) dir.create("build/cache/google_dist")

#####################################
#Read in parks
park_subset <- readRDS("build/cache/park_subset.rds")

arches = filter(park_placekeys,park=="Arches") %>%
  select(park,placekey)

#Read in data of all unique origins
census_geo <- read_csv("build/cache/census_geo.csv") 


od_dat <- read_csv("build/cache/parks_home_tract.csv") %>%
  distinct(placekey,tract) %>%
  inner_join(park_subset,by = join_by(placekey)) %>%
  left_join(select(census_geo,-area), by = c("tract"))


left_out <- origins %>%
  filter(is.na(orig_lon)) %>%
  select(placekey,tract)


# xwalk <- read_delim("build/inputs/tab20_tract20_tract10_natl.txt",delim = "|") %>%
#   clean_names()

od_dat <- od_dat %>%
  drop_na(dest_lon:orig_lat) %>%
  arrange(park,location_name,tract)

####################
#Already queried
dist_comp <- list.files("build/cache/google_dist",full.names = T,pattern = ".csv") %>%
  map(~read_csv(.,col_select = c(placekey,tract,trav_dist),col_types = "ccn")) %>%
  bind_rows() %>%
  drop_na() %>%
  distinct()

if(nrow(dist_comp)==0){
  remaining <- od_dat
} else {
  remaining <- anti_join(od_dat,dist_comp,by=c("placekey","tract"))
}


####################
#Break up origins into chunks of 25 with same destination
split_points <- remaining %>%
  group_split(group = grp_num_assign(tract,25),placekey) 


df=split_points[[2]] 

pb <- progress_bar$new(
  format = "/n  [:bar] :current/:total :elapsedfull eta: :eta",
  total = length(split_points))

split_points %>%
  walk(function(df){
    message(paste0("Starting group ",df$group[1]))
    pb$tick()
    #Check that all destinations are same
    if(nrow(df)>1 & var(df$dest_lon)!=0) stop("Not all destinations are the same.")
    
    doc = mp_matrix(
      origins = as.matrix(df[,c("orig_lon","orig_lat")]),
      destinations = as.matrix(df[1,c("dest_lon","dest_lat")]),
      mode = "driving",
      key = key,
      quiet = T
    )
    
    #pts <- mp_get_matrix(doc,value = "distance_m")
    
    #Extract distance component
    m = conv_unit(mp_get_matrix(doc, value = "distance_m"),"m","mi")
    s = conv_unit(mp_get_matrix(doc, value = "duration_s"),"sec","hr")
    
    #Build output df
    out <- bind_cols(
      df,
      rownames_to_column(as.data.frame(m),var = "origin_label") %>%
        rename(trav_dist = 2) %>%
        mutate(trav_time = as.numeric(s),
               dest_label = attr(s,"dimnames")[[2]])
    )
    
    
    #Sys.sleep(5)
    write_csv(out,paste0("build/cache/google_dist/group_",str_pad(out$group[1],3,"left","0"),"_",generate_random_string(length=5),".csv"))
    
  })

google_dist <- list.files("build/cache/google_dist",full.names = T) %>%
  map(~read_csv(.,col_select = c(park,placekey,tract,trav_dist,trav_time),col_types = "cccnn")) %>%
  bind_rows() %>%
  drop_na() %>%
  distinct(park,placekey,tract,.keep_all = T) 

write_csv(google_dist,"build/cache/google_dist.csv")


