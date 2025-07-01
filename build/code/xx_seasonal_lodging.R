#This script assembles the dataset for the cell TCM

library(pacman)
p_load(tidyverse,readxl,janitor,rvest)

source("project_init.R")

conflicts_prefer(lubridate::month)
conflicts_prefer(dplyr::filter)
######################


# Define the base URL of the GSA per diem files page
base_url <- "https://www.gsa.gov/travel/plan-a-trip/per-diem-rates/per-diem-files"

# Get the links to Excel files
links <- read_html(base_url) |>
  html_elements("a") |>
  html_attr("href") |>
  str_subset("\\.xlsx?$") |>
  keep(~ str_detect(., regex("ZipCode", ignore_case = TRUE)))

# Create a directory to store downloaded files
if (!dir.exists("build/inputs/gsa_per_diem")) {
  dir.create("build/inputs/gsa_per_diem", showWarnings = FALSE)
}

# Download all Excel files
walk(links, ~{
  destfile <- file.path("build/inputs/gsa_per_diem", basename(.x))
  if (!file.exists(destfile)) download.file(.x, destfile, mode = "wb")
})

# Function to extract lodging rates from each file
read_per_diem <- function(file) {
  df <- read_excel(file, .name_repair = janitor::make_clean_names) |>
    mutate(file = basename(file))
  df
}

# Read and combine all files
all_data <- list.files("build/inputs/gsa_per_diem", full.names = TRUE) |>
  map_dfr(read_per_diem)

# Pivot longer from Oct–Sep into a month column
monthly_data <- all_data |>
  pivot_longer(
    cols = oct:sep,
    names_to = "month_name",
    values_to = "lodging_rate"
  ) |>
  mutate(
    month = match(str_to_title(month_name), month.abb),
    year = as.integer(fiscal_year)
  )

# Summarize national average lodging rate by year and month
avg_by_year_month <- monthly_data |>
  filter(!is.na(lodging_rate), is.numeric(lodging_rate)) |>
  group_by(year, month, measure_date = make_date(year, month, 1)) |>
  summarise(national_avg_rate = mean(lodging_rate, na.rm = TRUE), .groups = "drop")



#Construct annual average to deduct from monthly
annual_rate <- monthly_data %>%
  filter(!is.na(lodging_rate), is.numeric(lodging_rate)) |>
  group_by(year) |>
  summarise(national_annual_avg_rate = mean(lodging_rate, na.rm = TRUE), .groups = "drop") %>%
  add_column(ahla = c(149.5,155.94,159,162.16)) # from https://www.ahla.com/sites/default/files/25_SOTI.pdf

#Create seasonal ahla adjustment
seasonal_index <- avg_by_year_month %>%
  inner_join(annual_rate,by = join_by(year)) %>%
  mutate(seasonal_ahla = (national_avg_rate - national_annual_avg_rate) + ahla)

#cache results
saveRDS(seasonal_index,"build/cache/hotel_costs.rds")

#Plotting - it doesn't matter much
ggplot(seasonal_index,aes(x=measure_date)) +
  geom_line(aes(y=national_avg_rate),color="blue") +
  annotate(geom = "text",x=as_date("2022-06-01"),y=115,label="GSA per diem",color="blue") +
  geom_line(aes(y=seasonal_ahla),color="darkorange") +
  annotate(geom = "text",x=as_date("2022-06-01"),y=153,label="Seasonal AHLA",color="darkorange") +
  labs(x=NULL,y="Dollars")
