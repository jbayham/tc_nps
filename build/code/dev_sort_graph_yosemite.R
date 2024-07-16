## install if needed (do this exactly once):
## install.packages("usethis")

library(usethis)
use_git_config(user.name = "inaschold", user.email = "inaschold@gmail.com")

library(pacman)
p_load(tidyverse, sf, mapview, arrow, tools, data.table, tidyr, lubridate)

yosemite_NPS <- read_csv("/RSTOR/bayham/projects/tc_nps/inputs/nps_visits/Official NPS Visitation Stats/Yosemite Visitation by Month.csv", skip = 3) %>%
  select(-c("AnnualTotal", "Textbox4")) %>%
  pivot_longer(cols=-Year, names_to="month",values_to = "observations") %>%
  mutate(park="Yosemite") %>%
  bind_rows()

month_names <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
month_integers <- match(month_names, month.abb)
print(month_integers)

yosemite_nps_months <- data.frame(month = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"))
                                            
yosemite_nps_current <- yosemite_NPS %>%
  mutate(month_integer = match(month, month.abb))

unique_values <- unique(yosemite_NPS$month)
print(unique_values)
print(month.abb)

correction_month <- c("JAN" = "Jan", "FEB" = "Feb", "MAR" = "Mar", "APR" = "Apr", "MAY" = "May", 
                      "JUN" = "Jun", "JUL" = "Jul", "AUG" = "Aug", "SEP" = "Sep", "OCT" = "Oct", "NOV" = "Nov", "DEC" = "Dec")

yosemite_NPS$months_corrected <- correction_month[yosemite_NPS$month]


yosemite_NPS_corrected <- yosemite_NPS %>%
  mutate(month = match(months_corrected, month.abb)) %>%
  filter(park == "Yosemite")

yosemite_nps <- yosemite_NPS %>%
  mutate(month = as.integer(unlist(yosemite_NPS[,2])))

yosemite_sg <- readRDS("/RSTOR/bayham/projects/tc_nps/inputs/nps_visits/Official NPS Visitation Stats/yosemite_visits.rds") %>%
  select("date_range_start", "date_range_end", "raw_visit_counts", "raw_visitor_counts", "location_name", "placekey") %>%
  mutate(year = year(date_range_start), month = month(date_range_start)) %>%
  mutate(visitors = raw_visit_counts) %>%
  mutate(Year = as.numeric(year))

yosemite_sg[is.na(yosemite_sg)] <- 0

str(yosemite_NPS_corrected)
str(yosemite_sg)

yosemite_data_both <- inner_join(yosemite_sg, yosemite_NPS_corrected, by = c("Year", "month"))
yosemite_data_both[is.na(yosemite_data_both)] <- 0

falls_scatter <- yosemite_data_both %>%
  filter(location_name == "Cascade Falls")

ggplot(data = falls_scatter, mapping = aes(x = visitors, y = observations)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "SafeGraph Cascade Falls Visitation vs. Yosemite Visitation For Those Rows")

yosemite_2021 <- yosemite_data_both %>%
  filter(Year == 2021) %>%
  select("Year", "month","raw_visit_counts", "visitors","location_name") %>%
  str_pad(month,2,"left","0") %>% 
  mutate(measure_date=as_date(paste0(Year,"-",month,-,"01")))

NPS_yosemite_2021 <- yosemite_data_both %>%
  mutate(measure_date=as.Date(paste0(Year,"-",month,"-01"))) %>%
  filter(park == "Yosemite", Year == 2021)

ggplot() +
  geom_line(data = yosemite_2021, aes(x = measure_date, y = visitors, color = "SafeGraph Data for POI")) +
  geom_line(data = NPS_yosemite_2021, aes(x = measure_date, y = observations, color = "NPS Data"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "SafeGraph Yosemite Visitation vs. NPS Yosemite Visitation, 2021", 
       x = "Month", 
       y = "Visitors") +
  scale_color_manual(values = c("SafeGraph Data for POI" = "blue", "NPS Data" = "orange"),
                     name = "Legend") +
  facet_wrap(vars(location_name, Year))
