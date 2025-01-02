#Capitol Reef National Park
p_load(mapview, arrow, tools, data.table, tidyr)
conflicts_prefer(lubridate::year)
conflicts_prefer(lubridate::month)

#For csv from NPS Stats database
cr_NPS <- read_csv("build/inputs/nps_visits/Capitol_Reef_NPS_visit_statistics.csv", skip = 3) %>%
  select(-c("AnnualTotal", "Textbox4")) %>%
  pivot_longer(cols=-Year, names_to="month",values_to = "observations") %>%
  mutate(park="Capitol Reef") %>%
  bind_rows()

cr_nps_current <- cr_NPS %>%
  mutate(month_integer = match(month, month.abb))

correction_month <- c("JAN" = "Jan", "FEB" = "Feb", "MAR" = "Mar", "APR" = "Apr", "MAY" = "May", 
                      "JUN" = "Jun", "JUL" = "Jul", "AUG" = "Aug", "SEP" = "Sep", "OCT" = "Oct", "NOV" = "Nov", "DEC" = "Dec")

cr_NPS$months_corrected <- correction_month[cr_NPS$month]

cr_NPS_corrected <- cr_NPS %>%
  mutate(month = match(months_corrected, month.abb)) %>%
  filter(park == "Capitol Reef")

#For SafeGraph data, scales SafeGraph Data
cr_sg <- readRDS("build/cache/cr_visits_months.rds") %>%
  select("date_range_start", "date_range_end", "raw_visit_counts", "raw_visitor_counts", "location_name", "placekey") %>%
  mutate(year = year(date_range_start), month = month(date_range_start)) %>%
  mutate(Year = as.numeric(year)) %>%
  mutate(Visits = raw_visit_counts * 4.5)

cr_sg[is.na(cr_sg)] <- 0

#Merges SafeGraph data with NPS statistics
cr_data_both <- inner_join(cr_sg, cr_NPS_corrected, by = c("Year", "month"))
cr_data_both[is.na(cr_data_both)] <- 0

#Scatter Plot (SafeGraph data scaled)
scatter_data1 <- cr_data_both %>%
  filter(location_name == "Capitol Gorge", Year %in% c("2019", "2020", "2021","2022"))

ggplot(data = scatter_data1, mapping = aes(x = Visits, y = observations)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "SafeGraph Capitol Gorge Visitation vs. Capitol Reef Visitation For Those Rows")

location_equation_CG <- lm(observations ~ Visits, data = scatter_data1)
print(location_equation_CG)

#Graphs for locations
cr_graph_data <- cr_data_both %>%
  select("Year", "month", "raw_visit_counts", "Visits","location_name", "observations", "park") %>%
  mutate(measure_date=as.Date(paste0(Year,"-",month,"-01"))) %>%
  filter(park == "Capitol Reef") 

ggplot() +
  geom_line(data = cr_graph_data, aes(x = measure_date, y = Visits, color = "SafeGraph Data for POI")) +
  geom_line(data = cr_graph_data, aes(x = measure_date, y = observations, color = "NPS Data"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "SafeGraph Capitol Reef Visitation vs. Capitol Reef NPS Visitation", 
       x = "Year", 
       y = "Visitors") +
  scale_color_manual(values = c("SafeGraph Data for POI" = "blue", "NPS Data" = "orange"),
                     name = "legend") +
  facet_wrap(vars(location_name))

#need to correct this
sum_sg_cr <- aggregate(Visits ~ measure_date, data = cr_graph_data, FUN = sum)

scale_sum_sg_cr <- sum_sg_cr %>%
  select(Visits, measure_date)

ggplot() + 
  geom_line(data = scale_sum_sg_cr, aes(x = measure_date, y = Visits, color = "SafeGraph Data for POI")) +
  geom_line(data = cr_graph_data, aes(x = measure_date, y = observations, color = "NPS Data"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "SafeGraph Capitol Reef Visitation vs. NPS Capitol Reef Visitation", 
       x = "Month", 
       y = "Visitors") +
  scale_color_manual(values = c("SafeGraph Data for POI" = "blue", "NPS Data" = "orange"),
                     name = "legend")