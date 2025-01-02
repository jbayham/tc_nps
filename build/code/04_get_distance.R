#Get travel distance and time

#install.packages("mapsapi")
library(pacman)
p_load(tidyverse,sf,mapsapi,janitor,measurements)

source("project_init.R")

#Set google API key
key = Sys.getenv("GOOGLE_MAPS_API_KEY")

if(!dir.exists("build/cache/google_dist")) dir.create("build/cache/google_dist")

#####################################
#Read in data of all unique origins
census_geo <- read_csv("build/cache/census_geo.csv") %>%
  rename(orig_lon=longitude,orig_lat=latitude)

origins <- read_csv("build/cache/origin_tracts.csv") %>%
  distinct(tract) %>%
  inner_join(census_geo,by=c("tract"="geoid"))

left_out <- read_csv("build/cache/origin_tracts.csv") %>%
  distinct(tract) %>%
  anti_join(census_geo,by=c("tract"="geoid"))



#####################
grp_num_assign <- function(vec,set_length){
  rws=length(vec)
  grp_id = rep(1:ceiling(rws/set_length), each=set_length, length.out=rws)
  
  return(grp_id)
}
####################
#Break up origins into chunks of 25
split_points <- origins %>%
  mutate(dest_lon = -83.890749,
         dest_lat = 35.942799) %>%
  group_split(group = grp_num_assign(tract,25)) 


dist_out <- split_points %>%
  walk(function(df){
    message(paste0("Starting group ",df$group[1]))
    #Check that all destinations are same
    if(nrow(df)>1 & var(df$dest_lon)!=0) stop("Not all destinations are the same.")
    
    doc = mp_matrix(
      origins = as.matrix(df[,c("orig_lon","orig_lat")]),
      destinations = as.matrix(df[1,c("dest_lon","dest_lat")]),
      mode = "driving",
      key = key,
      quiet = T
    )
    
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
    write_csv(out,paste0("build/cache/google_dist/group_",str_pad(out$group[1],3,"left","0"),".csv"))
    
  },.progress = TRUE)

google_dist <- list.files("build/cache/google_dist",full.names = T) %>%
  map(~read_csv(.,col_select = c(tract,trav_dist,trav_time),col_types = "cnn")) %>%
  bind_rows()

write_csv(google_dist,"build/cache/google_dist.csv")


