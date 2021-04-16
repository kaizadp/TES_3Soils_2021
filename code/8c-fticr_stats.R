# 3Soils
# fticr_stats 
# Kaizad F. Patel
# March 2020

# this file takes the processed fticr files and runs statistics 


source("0b-packages.R")

# PART 1. LOAD FILES ----
soil_raw_long = read.csv(FTICR_SOIL_RAW_LONG)# <- "fticr/fticr_soil_raw_longform.csv"
pore_raw_long = read.csv(FTICR_PORE_RAW_LONG)# <- "fticr/fticr_pore_raw_longform.csv"

#
## 1b. process files for analysis ----
# we want relative abundance for each core
## pores ----
relabund_temp = 
  pore_raw_long %>% 
  group_by(tension,site, treatment,Class,core) %>% 
  dplyr::summarize(compounds = n()) %>% # sum all COUNTS for each Class
  # now calculate relative abundance for each Class for each core
  group_by(tension,site, treatment, core) %>% 
  dplyr::mutate(total = sum(compounds),
                relabund = (compounds/total)*100)

relabund_pore = 
  relabund_temp %>% 
  dplyr::select(core, site, tension, treatment, Class, relabund) %>% 
  spread(Class, relabund) %>% 
  replace(is.na(.),0)

## soils ----
soil_relabund_temp = 
  soil_raw_long %>% 
  group_by(site, treatment,Class,core) %>% 
  dplyr::summarize(compounds = n()) %>% # sum all intensities for each Class
  # now calculate relative abundance for each Class for each core
  group_by(site, treatment, core) %>% 
  dplyr::mutate(total = sum(compounds),
                relabund = (compounds/total)*100)

relabund_soil = 
  soil_relabund_temp %>% 
  dplyr::select(core, site, treatment, Class, relabund) %>% 
  spread(Class, relabund) %>% 
  replace(is.na(.),0)  

#
# PART 3. MANOVA ----
## pores ----
relabund_pore$DV = as.matrix(relabund_pore[,5:13])

# since the relative abundances are not strictly independent and all add to 100 %,
# use the isometric log ratio transformation
# http://www.sthda.com/english/wiki/manova-test-in-r-multivariate-analysis-of-variance#import-your-data-into-r

library(compositions)

pore_man = manova(ilr(clo(DV)) ~ site*tension*treatment, data = relabund_pore)
summary(pore_man)

## time zero pores ----
relabund_pore_tz = 
  relabund_pore %>% 
  filter(treatment %in% "time zero saturation")

pore_tz_man = manova(ilr(clo(DV)) ~ site*tension, data = relabund_pore_tz)
summary(pore_tz_man)

#              Df   Pillai    approx F  num Df den Df   Pr(>F)    
# site          2   1.76068   14.7144   16     32       1.878e-10 ***


## soils ----
relabund_soil$DV = as.matrix(relabund_soil[,4:12])
soil_man = manova(ilr(clo(DV)) ~ site*treatment, data = relabund_soil)
summary(soil_man)

soil_man = manova(ilr(clo(DV)) ~ treatment, data = relabund_soil[relabund_soil$site=="SR",])
summary(soil_man)
#