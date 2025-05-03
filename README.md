# README.md

## Project: Analysis of RA Economic Partnership Growth (2020-2024)

This project involves cleaning and analyzing data related to the economic partnership growth of the Republic of Armenia (RA) from 2020 to 2024. The workflow includes cleaning raw datasets and generating a comprehensive report based on the cleaned data.

### Project Structure

- `data/raw/`: Contains raw datasets.
- `data/processed/`: Contains cleaned datasets.
- `R/data_cleaning/`: Scripts for cleaning individual datasets.
- `R/functions/`: Reusable functions for specific data cleaning and analysis tasks.
- `report/`: Files to generate the final analysis report.

### Usage Instructions

#### Step 1: Run the Cleaning Script
Ensure all necessary libraries are installed (see [Requirements](#requirements)). Then, execute the `cleaning.Rmd` script to clean and preprocess the datasets. This step ensures all data is ready for analysis.

#### Step 2: Generate the Report
After the cleaning process, run the `Report.Rmd` script located in the `report/` directory. This script compiles the findings and generates the final report.

### Requirements

The project relies on the following R packages. Please ensure these are installed before running the scripts:

- `ggplot2`
- `dplyr`
- `tidyr`
- `networkD3`
- `openxlsx`
- `ggthemes`
- `gridExtra`
- `readxl`
- `stringr`
- `tidyverse`
- `here`
- `fs`
- `janitor`
- `zoo`
- `lubridate`
- `writexl`
- `rnaturalearth`

#### Installing R Packages

To install the required R packages, run the following commands in your R console:

```R
install.packages(c("ggplot2", "dplyr", "tidyr", "networkD3", "openxlsx", "ggthemes", "gridExtra", "readxl", "stringr", "tidyverse", "here", "fs", "janitor", "zoo", "lubridate", "writexl", "rnaturalearth"))
