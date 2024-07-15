#Arches National Park
p_load(mapview, arrow, tools, data.table, tidyr)
conflicts_prefer(lubridate::year)
conflicts_prefer(lubridate::month)

#For csv from NPS Stats database
arches_NPS <- read_csv("build/inputs/nps_visits/Arches_NPS_visit_statistics.csv", skip = 3) %>%
  select(-c("AnnualTotal", "Textbox4")) %>%
  pivot_longer(cols=-Year, names_to="month",values_to = "observations") %>%
  mutate(park="Arches") %>%
  bind_rows()

arches_nps_current <- arches_NPS %>%
  mutate(month_integer = match(month, month.abb))

correction_month <- c("JAN" = "Jan", "FEB" = "Feb", "MAR" = "Mar", "APR" = "Apr", "MAY" = "May", 
                      "JUN" = "Jun", "JUL" = "Jul", "AUG" = "Aug", "SEP" = "Sep", "OCT" = "Oct", "NOV" = "Nov", "DEC" = "Dec")

arches_NPS$months_corrected <- correction_month[arches_NPS$month]

arches_NPS_corrected <- arches_NPS %>%
  mutate(month = match(months_corrected, month.abb)) %>%
  filter(park == "Arches")

#For SafeGraph data, scales SafeGraph Data
arches_sg <- readRDS("build/cache/arches_visits_months.rds") %>%
  select("date_range_start", "date_range_end", "raw_visit_counts", "raw_visitor_counts", "location_name", "placekey") %>%
  mutate(year = year(date_range_start), month = month(date_range_start)) %>%
  mutate(Year = as.numeric(year)) %>%
  mutate(Visits = raw_visit_counts * 10)

arches_sg[is.na(arches_sg)] <- 0

#Merges SafeGraph data with NPS statistics
arches_data_both <- inner_join(arches_sg, arches_NPS_corrected, by = c("Year", "month"))
arches_data_both[is.na(arches_data_both)] <- 0

#Scatter Plot (SafeGraph data scaled)
tunnel_scatter_data <- arches_data_both %>%
  filter(location_name == "Tunnel Arch", Year %in% c("2019", "2020", "2021", "2022"))
#the zero in this plot is from Dec 2022. Removed 2023 observations because they were zeros. Remove Dec 2022?
#should this not use scaled SafeGraph data?

ggplot(data = tunnel_scatter_data, mapping = aes(x = Visits, y = observations)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "SafeGraph Tunnel Arch Visitation vs. Arches Visitation For Those Rows")

tunnel_equation <- lm(observations ~ Visits, data = tunnel_scatter_data)
print(tunnel_equation)

#Graphs for locations
arches_graph_data <- arches_data_both %>%
  select("Year", "month", "raw_visit_counts", "Visits","location_name", "observations", "park") %>%
  mutate(measure_date=as.Date(paste0(Year,"-",month,"-01"))) %>%
  filter(park == "Arches") 

ggplot() +
  geom_line(data = arches_graph_data, aes(x = measure_date, y = Visits, color = "SafeGraph Data for POI")) +
  geom_line(data = arches_graph_data, aes(x = measure_date, y = observations, color = "NPS Data"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "SafeGraph Arches Visitation vs. NPS Arches Visitation", 
       x = "Year", 
       y = "Visitors") +
  scale_color_manual(values = c("SafeGraph Data for POI" = "blue", "NPS Data" = "orange"),
                     name = "legend") +
  facet_wrap(vars(location_name))

#need to correct this
sum_sg_arches <- aggregate(Visits ~ measure_date, data = arches_graph_data, FUN = sum)

scale_sum_sg_arches <- sum_sg_arches %>%
  select(Visits, measure_date)

ggplot() + 
  geom_line(data = scale_sum_sg_arches, aes(x = measure_date, y = Visits, color = "SafeGraph Data for POI")) +
  geom_line(data = arches_graph_data, aes(x = measure_date, y = observations, color = "NPS Data"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "SafeGraph Arches Visitation vs. NPS Arches Visitation, 2021", 
       x = "Month", 
       y = "Visitors") +
  scale_color_manual(values = c("SafeGraph Data for POI" = "blue", "NPS Data" = "orange"),
                     name = "legend")


#For specific year - 2021
arches_2021 <- arches_data_both %>%
  filter(Year == 2021) %>%
  select("Year", "month", "Visits","location_name", "observations") %>%
  mutate(measure_date=as.Date(paste0(Year,"-",month,"-01")))

NPS_arches_2021 <- arches_data_both %>%
  mutate(measure_date=as.Date(paste0(Year,"-",month,"-01"))) %>%
  filter(park == "Arches", Year == 2021) 

ggplot() +
  geom_line(data = arches_2021, aes(x = measure_date, y = Visits, color = "SafeGraph Data for POI")) +
  geom_line(data = NPS_arches_2021, aes(x = measure_date, y = observations, color = "NPS Data"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "SafeGraph Arches Visitation vs. NPS Arches Visitation, 2021", 
       x = "Month", 
       y = "Visitors") +
  scale_color_manual(values = c("SafeGraph Data for POI" = "blue", "NPS Data" = "orange"),
                     name = "legend") +
  facet_wrap(vars(location_name, Year))