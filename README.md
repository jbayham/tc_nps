# National Parks Travel Cost with Mobile Device Data

Jude Bayham, Leslie Richardson, Aaron Enriquez Kate Floersheim, Isa Naschold,

The goal of this project is to assess the quality of mobile device data for conducting recreation demand research. We will investigate the reliability of the data and compare consumer surplus estimates using mobile device data to those of conventional surveys.

# To Do 


********************************************

# Project and directory structure

This section describes the directory structure of the project.  The project is divided into two primary parts.  Part 1 builds the dataset(s) to be used in the analysis phase.  Part 2 contains scripts to run the analysis and generate output (tables and figures).  Note that the dataset produced in Part 1 is written to the inputs directory in Part 2.

## Part 1: Build

- `inputs` directory contains the raw data that should not be modified and overwritten
- `cache` directory stores copies of data during intermediate steps in the preprocessing   
- `code` directory contains all scripts to read in and preprocess the data  

The code folder includes a make file (`00-build.R`) for the dataset build phase of the project.

## Part 2: Analysis

- `inputs` directory contains the dataset built in Part 1
- `cache` directory stores copies of data during intermediate analysis steps     
- `code` directory contains all scripts to read in and preprocess the data   
- `output` directory contains figures and tables generated from analysis scripts

The `functions` folder contains all functions specific to this analysis.  
The `report` directory contains the write up of the project and may contain a link to a collaborative writing site.  
The `references` folder contains bib files for the project.

## Building the Project

The project root directory contains a file called `project_init.R` that initializes the project (installs/loads packages etc.).  You should run this file each time you open R to begin working on the project.  

*Note that all file references within project are relative to the root directory of the project.*

********************************************

# Data

This section describes the data sources and provides information about data processing.  I find it useful to describe these details during project development and refine them for the final write up.

- Socio-economic Monitoring: Survey administered to 24 parks per year to monitor visitation.

- Advan Monthly Patterns: Mobile device visitation. Specifically, we use the monthly data disaggregated by census tract of origin to county monthly visits. We sum visits over the time window to match the SEM survey

- American Community Survey 2022, 2023 (5 yr): We compile tract and zipcode level data on: population (table B01001), household median income (B19013), median age (B01002), education (B06009), and household size (B25010). Details in [build/code/03_census_data.R](build/code/03_census_data.R)  

  - 2010 - 2020 Census Crosswalk: <https://www.census.gov/geographies/reference-files/time-series/geo/relationship-files.2020.html>

## Sources

This section contains a bulleted list of data sources from the project.  If you pull intermediate data from another project, reference the project repo.

## Data Processing

This section describes how each script in the `build/inputs` folder processed the data into the analysis dataset.

### 03_census_data

We collect 2022 and 2023 5-year ACS data (population, household median income, median age, education, and household size) for all census tracts in the country. Education is reported as the number of people with different categories of educational attainment. We calculate the fraction of the total with at least a bachelors degree. Education is the single metric that is not complete.

Advan records visits by 2010 census tracts. However, we extract census data and locations from the 2023 5-year ACS, which uses the 2020 delineations. This leads to dropped data when an old tract geoid no longer exists because it was broken into several tracts. The Census publishes a crosswalk that maps between the 2010 and 2020 data. We use the 2019 tract delineations to construct distance. For the recent attribute data, we use the crosswalk to map data based on the 2020 delineations to the 2010 delineations to merge with the Advan tract data.
