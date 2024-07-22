#Bryce Canyon National Park
p_load(mapview, arrow, tools, data.table, tidyr)
conflicts_prefer(lubridate::year)
conflicts_prefer(lubridate::month)

#For csv from NPS Stats database
bc_NPS <- read_csv("build/inputs/nps_visits/Bryce_Canyon_NPS_visit_statistics.csv", skip = 3) %>%
  select(-c("AnnualTotal", "Textbox4")) %>%
  pivot_longer(cols=-Year, names_to="month",values_to = "observations") %>%
  mutate(park="Bryce Canyon") %>%
  bind_rows()

bc_nps_current <- bc_NPS %>%
  mutate(month_integer = match(month, month.abb))

correction_month <- c("JAN" = "Jan", "FEB" = "Feb", "MAR" = "Mar", "APR" = "Apr", "MAY" = "May", 
                      "JUN" = "Jun", "JUL" = "Jul", "AUG" = "Aug", "SEP" = "Sep", "OCT" = "Oct", "NOV" = "Nov", "DEC" = "Dec")

bc_NPS$months_corrected <- correction_month[bc_NPS$month]

bc_NPS_corrected <- bc_NPS %>%
  mutate(month = match(months_corrected, month.abb)) %>%
  filter(park == "Bryce Canyon")

#For SafeGraph data, scales SafeGraph Data
bc_sg <- readRDS("build/cache/bc_visits_months.rds") %>%
  select("date_range_start", "date_range_end", "raw_visit_counts", "raw_visitor_counts", "location_name", "placekey") %>%
  mutate(year = year(date_range_start), month = month(date_range_start)) %>%
  mutate(Year = as.numeric(year)) %>%
  mutate(Visits = raw_visit_counts * 20)

bc_sg[is.na(bc_sg)] <- 0

#Merges SafeGraph data with NPS statistics
bc_data_both <- inner_join(bc_sg, bc_NPS_corrected, by = c("Year", "month"))
bc_data_both[is.na(bc_data_both)] <- 0

#Scatter Plot (SafeGraph data scaled)
#why are these scatter datasets all the same?
scatter_data1 <- bc_data_both %>%
  filter(location_name == "Thor's Hammer", Year %in% c("2019", "2020", "2021","2022"), date_range_end != "2023-01-01")

ggplot(data = scatter_data1, mapping = aes(x = Visits, y = observations)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "SafeGraph Thor's Hammer Visitation vs. Bryce Canyon Visitation For Those Rows")

location_equation <- lm(observations ~ Visits, data = scatter_data1)
print(location_equation)

scatter_data2 <- bc_data_both %>%
  filter(location_name == "Rainbow Point", Year %in% c("2019", "2020", "2021","2022"), date_range_end != "2023-01-01")

ggplot(data = scatter_data2, mapping = aes(x = Visits, y = observations)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "SafeGraph Rainbow Point Visitation vs. Bryce Canyon Visitation For Those Rows")

location_equation2 <- lm(observations ~ Visits, data = scatter_data2)
print(location_equation2)

scatter_data_total <- bc_data_both %>%
  filter(placekey == "zzy-222@5yy-84c-52k", Year %in% c("2019", "2020", "2021","2022"),date_range_end != "2023-01-01")

ggplot(data = scatter_data_total, mapping = aes(x = Visits, y = observations)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "SafeGraph Bryce Canyon National Park POI Visitation vs. Bryce Canyon Visitation For Those Rows")

location_equation_total <- lm(observations ~ Visits, data = scatter_data_total)
print(location_equation_total)

#Graphs for locations
bc_graph_data <- bc_data_both %>%
  select("Year", "month", "raw_visit_counts", "Visits","location_name", "observations", "park") %>%
  mutate(measure_date=as.Date(paste0(Year,"-",month,"-01"))) %>%
  filter(park == "Bryce Canyon") 

ggplot() +
  geom_line(data = bc_graph_data, aes(x = measure_date, y = Visits, color = "SafeGraph Data for POI")) +
  geom_line(data = bc_graph_data, aes(x = measure_date, y = observations, color = "NPS Data"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "SafeGraph Bryce Canyon Visitation vs. Bryce Canyon NPS Visitation", 
       x = "Year", 
       y = "Visitors") +
  scale_color_manual(values = c("SafeGraph Data for POI" = "blue", "NPS Data" = "orange"),
                     name = "legend") +
  facet_wrap(vars(location_name))

#need to correct this
sum_sg_bc <- aggregate(Visits ~ measure_date, data = bc_graph_data, FUN = sum)

scale_sum_sg_bc <- sum_sg_bc %>%
  select(Visits, measure_date)

ggplot() + 
  geom_line(data = scale_sum_sg_bc, aes(x = measure_date, y = Visits, color = "SafeGraph Data for POI")) +
  geom_line(data = bc_graph_data, aes(x = measure_date, y = observations, color = "NPS Data"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "SafeGraph Bryce Canyon Visitation vs. NPS Bryce Canyon Visitation", 
       x = "Month", 
       y = "Visitors") +
  scale_color_manual(values = c("SafeGraph Data for POI" = "blue", "NPS Data" = "orange"),
                     name = "legend")