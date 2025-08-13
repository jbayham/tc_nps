#This script assembles the dataset for the cell TCM

library(pacman)
p_load(tidyverse,readxl,janitor,rvest,scales,fixest)

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


fe_mod <- feols(lodging_rate ~ factor(month) | year + zip,
                data = monthly_data)

summary(fe_mod)
etable(fe_mod)

fes <- fixef(fe_mod)

as.data.frame(fes$year) %>%
  rename(rates=1) %>%
  rownames_to_column(var="year") %>%
  ggplot(aes(x=year,y=rates)) +
  geom_point(size=2) +
  scale_y_continuous(labels = label_dollar(),limits = c(135,160)) +
  theme_bw(base_size = 15) +
  labs(x=NULL,y="GSA Lodging Rates")

ggsave("analysis/outputs/gsa_rates_year.png")

as.data.frame(fes$zip) %>%
  rename(rates=1) %>%
  ggplot(aes(x=rates)) +
  geom_histogram(binwidth = 15) +
  theme_bw(base_size = 15) +
  labs(x="Scaled GSA Lodging Rates",y="Frequency")
  
ggsave("analysis/outputs/gsa_rates_zip.png")

sum(fes$zip<0)/length(fes$zip)


