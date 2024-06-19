library(pacman)
p_load(tidyverse, sf, mapview, arrow, tools, data.table, tidyr, lubridate)

arches_NPS <- read_csv("build/inputs/nps_visits/Arches_Visit_Rec_Month.csv", skip = 3) %>%
  select(-c("AnnualTotal", "Textbox4")) %>%
  pivot_longer(cols=-Year, names_to="month",values_to = "observations") %>%
  mutate(park="Arches") %>%
  bind_rows()

month_names <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
month_integers <- match(month_names, month.abb)
print(month_integers)

arches_nps_months <- data.frame(month = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"))

arches_nps_current <- arches_NPS %>%
  mutate(month_integer = match(month, month.abb))

unique_values <- unique(arches_NPS$month)
print(unique_values)
print(month.abb)

correction_month <- c("JAN" = "Jan", "FEB" = "Feb", "MAR" = "Mar", "APR" = "Apr", "MAY" = "May", 
                      "JUN" = "Jun", "JUL" = "Jul", "AUG" = "Aug", "SEP" = "Sep", "OCT" = "Oct", "NOV" = "Nov", "DEC" = "Dec")

arches_NPS$months_corrected <- correction_month[arches_NPS$month]

arches_NPS_corrected <- arches_NPS %>%
  mutate(month = match(months_corrected, month.abb)) %>%
  filter(park == "Arches")

arches_nps <- arches_NPS %>%
  mutate(month = as.integer(unlist(arches_NPS[,2])))

arches_sg <- readRDS("arches_visits.rds") %>%
  select("date_range_start", "date_range_end", "raw_visit_counts", "raw_visitor_counts", "location_name", "placekey") %>%
  mutate(year = year(date_range_start), month = month(date_range_start)) %>%
  mutate(visitors = raw_visit_counts) %>%
  mutate(Year = as.numeric(year))

arches_sg[is.na(arches_sg)] <- 0

str(arches_NPS_corrected)
str(arches_sg)

arches_data_both <- inner_join(arches_sg, arches_NPS_corrected, by = c("Year", "month"))
arches_data_both[is.na(arches_data_both)] <- 0


tunnel_scatter <- arches_data_both %>%
  filter(location_name == "Tunnel Arch")

ggplot(data = tunnel_scatter, mapping = aes(x = visitors, y = observations)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "SafeGraph Tunnel Arch Visitation vs. Arches Visitation For Those Rows")


arches_2021 <- arches_data_both %>%
  filter(Year == 2021) %>%
  select("Year", "month", "raw_visit_counts", "visitors","location_name") %>%
  str_pad(month,2,"left","0") %>% 
  mutate(measure_date=as_date(paste0(Year,"-",month,-,"01")))

NPS_arches_2021 <- arches_data_both %>%
  filter(park == "Arches", Year == 2021)

ggplot() +
  geom_line(data = arches_2021, aes(x = month, y = visitors, color = "SafeGraph Data for POI")) +
  geom_line(data = NPS_arches_2021, aes(x = month, y = observations, color = "NPS Data"))+
  labs(title = "SafeGraph Arches Visitation vs. NPS Arches Visitation, 2021", 
       x = "Month", 
       y = "Visitors") +
  scale_color_manual(values = c("SafeGraph Data for POI" = "blue", "NPS Data" = "orange"),
                     name = "legend") +
  facet_wrap(vars(location_name, Year))


sum_sg_arches_2021 <- aggregate(raw_visit_counts ~ month, data = arches_2021, FUN = sum)


scale_sum_sg_21_arches <- sum_sg_arches_2021 %>%
  mutate(visitors = raw_visit_counts * 20) %>% #fix scale
  str_pad(month,2,"left","0") %>% #correct
  mutate(measure_date=as_date(paste0(Year,"-",month,-,"01"))) #correct

ggplot() + 
  geom_line(data = scale_sum_sg_21_arches, aes(x = month, y = visitors, color = "SafeGraph Data for POI")) +
  geom_line(data = NPS_arches_2021, aes(x = month, y = observations, color = "NPS Data"))+
  labs(title = "SafeGraph Arches Visitation vs. NPS Arches Visitation, 2021", 
       x = "Month", 
       y = "Visitors") +
  scale_color_manual(values = c("SafeGraph Data for POI" = "blue", "NPS Data" = "orange"),
                     name = "legend")
