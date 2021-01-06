library(tidyverse)
library(lubridate)
install.packages("neonUtilities")
install.packages("raster")
devtools::install_github("NEONScience/NEON-geolocation/geoNEON")
library(neonUtilities)
library(geoNEON)
library(raster)

#
# CPCRW -------------------------------------------------------------------

# https://portal.edirepository.org/nis/metadataviewer?packageid=knb-lter-bnz.392.20
# volumetric 

cpcrw_water = read.csv("data/historical_site_moisture/392_SM_CRREL_2000-2016.txt")

cpcrw_water2 = 
  cpcrw_water %>% 
  mutate(date = ymd(date),
         value = as.numeric(VWC),
         year = year(date)) %>% 
  filter(flag=="G") %>% 
  filter(value > 0 & value < 1000 & year > 2005 & depth == 10) 

cpcrw_water2 %>% 
  ggplot(aes(x = date, y = value * 100))+
  geom_point()+
  labs(y = "volumetric water, %",
       title = "CPCRW",
       subtitle = "source = Chapin et al. Bon Cr LTER, Env Data Initiative")+
  NULL

cpcrw_water2 %>% 
  ggplot(aes(y = value, x = year))+
  geom_violin()




# DWP ---------------------------------------------------------------------
## https://data.neonscience.org/data-products/explore
## https://data.neonscience.org/data-products/DP1.00094.001#visualizations
## National Ecological Observatory Network. 2020. 
## Data Product DP1.00094.001, Soil water content and water salinity. 
## Provisional data downloaded from http://data.neonscience.org on December 15, 2020. 
## Battelle, Boulder, CO, USA NEON. 2020.


# stackByTable("data/historical_site_moisture/NEON_conc-h2o-soil-salinity.zip")

## extract data
## dwp_2017 = loadByProduct(dpID = "DP1.00094.001", site = "DSNY", startdate = "2017-01", enddate = "2017-12", check.size = TRUE)
## dwp_2018 = loadByProduct(dpID = "DP1.00094.001", site = "DSNY", startdate = "2018-01", enddate = "2018-12", check.size = TRUE)
## 
## names(dwp_2017)
## View(dwp_2017$SWS_30_minute)
## View(dwp_2017$readme_00094)
## View(dwp_2017$variables_00094)
## 
## write.csv(dwp_2017$SWS_30_minute, "data/historical_site_moisture/dwp_30min_2017.csv", row.names = FALSE)
## write.csv(dwp_2018$SWS_30_minute, "data/historical_site_moisture/dwp_30min_2018.csv", row.names = FALSE)
## write.csv(dwp_2017$readme_00094, "data/historical_site_moisture/dwp_30min_readme.csv", row.names = FALSE)
## write.csv(dwp_2017$variables_00094, "data/historical_site_moisture/dwp_30min_variables.csv", row.names = FALSE)

## clean data
### NOTE: these data are stored as .zip files because of size. extract first, then run this script. 

dwp_2017 = read.csv("data/historical_site_moisture/dwp_30min_2017.csv")
dwp_2018 = read.csv("data/historical_site_moisture/dwp_30min_2018.csv")

dwp_30min = 
  dwp_2017 %>% rbind(dwp_2018)

dwp_daily = 
  dwp_30min %>% 
  dplyr::select(siteID, horizontalPosition, startDateTime, VSWCMean) %>% 
  mutate(startDateTime = ymd_hms(startDateTime),
         date = as.Date(startDateTime)) %>% 
  group_by(horizontalPosition, date) %>% 
  dplyr::summarise(VSWC_cm3_cm3 = mean(VSWCMean, na.rm = TRUE),
                   VSWC_cm3_cm3 = as.numeric(VSWC_cm3_cm3),
                   year = year(date))


dwp_daily %>% 
  filter(horizontalPosition == 1) %>% 
  ggplot(aes(x = date, y = VSWC_cm3_cm3*100))+
  geom_point()+
  ylim(0,40)+
  labs(y = "volumetric water, %",
       title = "DWP (2017-2019)",
       subtitle = "source = NEON")+
  NULL

dwp_daily %>% 
  filter(horizontalPosition == 1) %>% 
  ggplot(aes(x = year, y = VSWC_cm3_cm3*100))+
  geom_violin()




#
# PLOT --------------------------------------------------------------------

water_summary = tribble(
  ~Site, ~Treatment, ~moisture_vol_perc, ~texture,
  "CPCRW", "drought", 0, 32, 
  "CPCRW", "fm", 31, 32, 
  "CPCRW", "flood", 129, 32,
  
  "DWP", "drought", 0, 90, 
  "DWP", "fm", 11, 90, 
  "DWP", "flood", 40, 90,
  
  "SR", "drought", 26, 14, 
  "SR", "fm", 64, 14, 
  "SR", "flood", 76, 14,
)

theme_set(theme_bw())

ggplot(water_summary, aes(x = moisture_vol_perc, y = texture))+
  geom_rect(xmin = 0.06, xmax = 40, ymin = 88, ymax = 92, alpha = 0.1, fill = "lightblue")+
  geom_rect(xmin = 0.06, xmax = 114, ymin = 30, ymax = 34, alpha = 0.1, fill = "lightblue")+
  geom_point()+
  xlim(0,150)+
  ylim(0,100)+
  annotate("text", label = "Alaska", x = 100, y = 38)+
  annotate("text", label = "Florida", x = 50, y = 90)+
  annotate("text", label = "Washington", x = 100, y = 15)+
  annotate("text", label = "niche 2017, 18", x = 25, y = 85, size = 3)+
  annotate("text", label = "niche 2006-13", x = 50, y = 28, size = 3)+
  labs(title = "3Soils",
       subtitle = "(change to gravimetric later)",
       x = "volumetric moisture, %",
       y = "texture (% sand)"
       )+
  NULL
  
ggplot(water_summary, aes(x = moisture_vol_perc, y = texture))+
  geom_violin(data = dwp_daily, aes(y = 90, x = VSWC_cm3_cm3*100))+
  geom_violin(data = cpcrw_water2, aes(y = 32, x = value*100))+
  geom_point()+
  xlim(0,150)+
  ylim(0,100)
