# National Parks Travel Cost with Mobile Device Data

Jude Bayham, Kate Floersheim, Isa Naschold, Leslie Richardson, Aaron Enriquez

The goal of this project is to assess the quality of mobile device data for conducting recreation demand research. We will investigate the reliability of the data and compare consumer surplus estimates using mobile device data to those of conventional surveys.

# To Do 

- Advan records visits by 2010 census tracts. However, we extract census data and locations from the 2023 5-year ACS, which uses the 2020 delineations. This leads to mismatches when an old tract geoid no longer exists because it was broken into several tracts. There are crosswalks that map between the two. For now, we ignore the mismatches and treat them as missing at random. Next steps should be to use 2020-2010 tract crosswalk to fill in data. Since we extract data for all tracts in a county, the new tracts should be in the same county.

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

## Sources

This section contains a bulleted list of data sources from the project.  If you pull intermediate data from another project, reference the project repo.

## Data Processing

This section describes how each script in the `build/inputs` folder processed the data into the analysis dataset.