#Guadalupe Mountains National Park
p_load(mapview, arrow, tools, data.table, tidyr)
conflicts_prefer(lubridate::year)
conflicts_prefer(lubridate::month)

#For csv from NPS Stats database
gp_NPS <- read_csv("build/inputs/nps_visits/Guadalupe_NPS_visit_statistics.csv", skip = 3) %>%
  select(-c("AnnualTotal", "Textbox4")) %>%
  pivot_longer(cols=-Year, names_to="month",values_to = "observations") %>%
  mutate(park="Guadalupe Moutains") %>%
  bind_rows()

gp_nps_current <- gp_NPS %>%
  mutate(month_integer = match(month, month.abb))

correction_month <- c("JAN" = "Jan", "FEB" = "Feb", "MAR" = "Mar", "APR" = "Apr", "MAY" = "May", 
                      "JUN" = "Jun", "JUL" = "Jul", "AUG" = "Aug", "SEP" = "Sep", "OCT" = "Oct", "NOV" = "Nov", "DEC" = "Dec")

gp_NPS$months_corrected <- correction_month[gp_NPS$month]

gp_NPS_corrected <- gp_NPS %>%
  mutate(month = match(months_corrected, month.abb))

#For SafeGraph data, scales SafeGraph Data
gp_sg <- readRDS("build/cache/gp_visits_months.rds") %>%
  select("date_range_start", "date_range_end", "raw_visit_counts", "raw_visitor_counts", "location_name", "placekey") %>%
  mutate(year = year(date_range_start), month = month(date_range_start)) %>%
  mutate(Year = as.numeric(year)) %>%
  mutate(Visits = raw_visit_counts * 1.5)

gp_sg[is.na(gp_sg)] <- 0

#Merges SafeGraph data with NPS statistics
gp_data_both <- inner_join(gp_sg, gp_NPS_corrected, by = c("Year", "month"))
gp_data_both[is.na(gp_data_both)] <- 0

#Scatter Plot (SafeGraph data scaled)
scatter_data1 <- gp_data_both %>%
  filter(location_name == "Guadalupe Peak Trail Hike", Year %in% c("2019", "2020", "2021","2022"), 
         date_range_end != "2023-01-01")

ggplot(data = scatter_data1, mapping = aes(x = Visits, y = observations)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "SafeGraph Guadalupe Peak Trail Hike Visitation vs. Bryce Canyon Visitation For Those Rows")

location_equation_1 <- lm(observations ~ Visits, data = scatter_data1)
print(location_equation_1)


scatter_data_total <- gp_data_both %>%
  filter(placekey == "zzy-222@8tk-m57-ch5", Year %in% c("2019", "2020", "2021","2022"), 
         date_range_end != "2023-01-01")

ggplot(data = scatter_data_total, mapping = aes(x = Visits, y = observations)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "SafeGraph Guadalupe Mountains National Park POI Visitation vs. Bryce Canyon Visitation For Those Rows")

location_equation_total <- lm(observations ~ Visits, data = scatter_data_total)
print(location_equation_total)

#Graphs for locations
gp_graph_data <- gp_data_both %>%
  select("Year", "month", "raw_visit_counts", "Visits","location_name", "observations", "park") %>%
  mutate(measure_date=as.Date(paste0(Year,"-",month,"-01")))

ggplot() +
  geom_line(data = gp_graph_data, aes(x = measure_date, y = Visits, color = "SafeGraph Data for POI")) +
  geom_line(data = gp_graph_data, aes(x = measure_date, y = observations, color = "NPS Data"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "SafeGraph Guadalupe Mountains Visitation vs. Guadalupe Mountains NPS Visitation", 
       x = "Year", 
       y = "Visitors") +
  scale_color_manual(values = c("SafeGraph Data for POI" = "blue", "NPS Data" = "orange"),
                     name = "legend") +
  facet_wrap(vars(location_name))

#need to correct this
sum_sg_gp <- aggregate(Visits ~ measure_date, data = gp_graph_data, FUN = sum)

scale_sum_sg_gp <- sum_sg_gp %>%
  select(Visits, measure_date)

ggplot() + 
  geom_line(data = scale_sum_sg_gp, aes(x = measure_date, y = Visits, color = "SafeGraph Data for POI")) +
  geom_line(data = gp_graph_data, aes(x = measure_date, y = observations, color = "NPS Data"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "SafeGraph Guadalupe Mountains Visitation vs. NPS Guadalupe Mountains Visitation", 
       x = "Month", 
       y = "Visitors") +
  scale_color_manual(values = c("SafeGraph Data for POI" = "blue", "NPS Data" = "orange"),
                     name = "legend")