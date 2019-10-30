# tes_3soils_2019

site (3 levels): CPCRW, DWP, SR

treatment (5 levels): baseline/timezero, timezero saturation, field moist, saturation, drought

tension for pore water (2 levels): -1.5 kPa (macropores), -50 kPa (micropores)


## `data` 
input data files

FTICR files are zipped. *Do not unzip, they are too big for GitHub.*

## `fticr` 
output files from the FTICR script 

use these files as inputs for the graphs. avoid grief by using these smaller files.

## `output`
output graphs and tables

## scripts
`0-packages.R`: packages and ggplot functions used in subsequent scripts

`1-characterization.R`: initial soil characterization for the three sites: chemistry, pore size distribution, water retention curves. processed output files are saved in `processed` folder.

    `1b-moisture.R`: calculates dry soil weight in each core. attempts to calculate moisture values. work in progress.

`2-wsoc.R`: water soluble organic carbon for porewater and soil extracts. report as mg/L and mg/g respectively.

`3-fticr_processing.R`: scripts for creating and processing fticr peaks metadata and sample data, for porewater and soil extracts. the output from this script is saved in `fticr` folder and will be used in script #4.

`4-fticr_abundance.R`: uses outputs from script #3 to count peaks and calculate relative abundance of the classes. output files are saved in `fticr` folder.

`000-3soils_markdown.Rmd`: uses output from all other scripts to generate figures and tables
