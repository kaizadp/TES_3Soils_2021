### using output files from script 5 to plot graphs and run stats
### Kaizad F. Patel
### Jan 10, 2020

source("0-functions.R")
source("0b-packages.R")

#### ####
fluxdata = read_csv(FLUXDATA_FILE)

## fluxes during treatment incubations
fluxdata2 = 
  fluxdata %>% 
  dplyr::select(SampleID,Site,Treatment,TREATMENT_PHASE,
                inctime_hours, DryMass_SoilOnly_g, CO2_flux_mgC_hr, cumCO2_flux_mgC_gSoil) %>% 
  filter(TREATMENT_PHASE == "DROUGHT_INCUBATION" |
           TREATMENT_PHASE == "FIELD_MOIST_INCUBATION" |
           TREATMENT_PHASE == "SATURATION_INCUBATION") %>% 
  dplyr::mutate(CO2_flux_mgC_gSoil_hr = CO2_flux_mgC_hr/DryMass_SoilOnly_g,
                Treatment = case_when(Treatment=="field_moist"~"field moist",
                                      Treatment=="sat_II"~"saturation",
                                      Treatment=="drought"~"drought"))

# flux grouped, no time series 
ggplot(fluxdata2, aes(y = CO2_flux_mgC_gSoil_hr, x = Site, color = Treatment))+
  geom_point(position = position_dodge(width=0.5))

# flux time series
ggplot(fluxdata2, aes(y = CO2_flux_mgC_gSoil_hr, x = inctime_hours, color = Treatment))+
  geom_point()+
  facet_grid(Treatment~Site)


## summary table for flux
fluxdata_summary = 
  fluxdata2 %>% 
  group_by(SampleID,Site,Treatment) %>% 
  dplyr::summarise(mean_flux = mean(CO2_flux_mgC_gSoil_hr, na.rm = TRUE),
                   cum_flux = max(cumCO2_flux_mgC_gSoil, na.rm = TRUE)) %>% 
  ungroup %>% 
  group_by(Site, Treatment) %>% 
  dplyr::summarise(mean_flux_mean = mean(mean_flux*1000, na.rm = TRUE),
                   mean_flux_se = sd(mean_flux*1000, na.rm = TRUE)/(sqrt(n())),
                   cum_flux_mean = mean(cum_flux),
                   cum_flux_se = sd(cum_flux)/sqrt(n()),
                   MEAN_CO2_FLUX_ugC_gSoil_hr = paste(round(mean_flux_mean,2),"\U00B1",round(mean_flux_se,2)),
                   CUM_CO2_FLUX_mgC_gSoil = paste(round(cum_flux_mean,2),"\U00B1",round(cum_flux_se,2))) %>% 
  dplyr::select(Site, Treatment, MEAN_CO2_FLUX_ugC_gSoil_hr, CUM_CO2_FLUX_mgC_gSoil) %>% 
  gather(variable, value,MEAN_CO2_FLUX_ugC_gSoil_hr:CUM_CO2_FLUX_mgC_gSoil) %>% 
  dplyr::mutate(var = paste(variable, Treatment)) %>% 
  dplyr::select(Site, var,value) %>% 
  spread(var, value) %>% 
  knitr::kable()

#

## summary stats for flux -- Tukey HSD for treatment effects, for each soil ----
library(agricolae)

# cumulative flux 
fit_hsd_relabund_cum <- function(dat) {
   a <-aov(cumCO2_flux_mgC_gSoil ~ Treatment, data = dat)
   h <-HSD.test(a,"Treatment")
   #create a tibble with one column for each treatment
   #the hsd results are row1 = drought, row2 = saturation, row3 = time zero saturation, row4 = field moist. hsd letters are in column 2
   tibble(`drought` = h$groups["drought",2], 
          `saturation` = h$groups["saturation",2],
          `field moist` = h$groups["field moist",2])
 }
fluxdata2 %>% 
  group_by(Site) %>% 
  do(fit_hsd_relabund_cum(.)) %>% 
  gather(Treatment, cum_hsd, 2:4)->
  fluxdata_hsd_cum

# mean flux
fit_hsd_relabund_mean <- function(dat) {
  a <-aov(CO2_flux_mgC_gSoil_hr ~ Treatment, data = dat)
  h <-HSD.test(a,"Treatment")
  #create a tibble with one column for each treatment
  #the hsd results are row1 = drought, row2 = saturation, row3 = time zero saturation, row4 = field moist. hsd letters are in column 2
  tibble(`drought` = h$groups["drought",2], 
         `saturation` = h$groups["saturation",2],
         `field moist` = h$groups["field moist",2])
}
fluxdata2 %>% 
  group_by(Site) %>% 
  do(fit_hsd_relabund_mean(.)) %>% 
  gather(Treatment, mean_hsd, 2:4)->
  fluxdata_hsd_mean

# now summarize `fluxdata2` and merge with the hsd files

fluxdata_summary = 
  fluxdata2 %>% 
  group_by(SampleID,Site,Treatment) %>% 
  dplyr::summarise(mean_flux = mean(CO2_flux_mgC_gSoil_hr, na.rm = TRUE),
                   cum_flux = max(cumCO2_flux_mgC_gSoil, na.rm = TRUE)) %>% 
  ungroup %>% 
  group_by(Site, Treatment) %>% 
  dplyr::summarise(mean_mean = mean(mean_flux*1000, na.rm = TRUE),
                   mean_se = sd(mean_flux*1000, na.rm = TRUE)/sqrt(n()),
                   cum_mean = mean(cum_flux, na.rm = TRUE),
                   cum_se = sd(cum_flux, na.rm = TRUE)/sqrt(n()),
                   MEAN_CO2_FLUX_ugC_gSoil_hr = paste(round(mean_mean,2),"\U00B1",round(mean_se,2)),
                   CUM_CO2_FLUX_mgC_gSoil = paste(round(cum_mean,2),"\U00B1",round(cum_se,2))) %>% 
  left_join(fluxdata_hsd_mean, by = c("Site","Treatment")) %>% 
  left_join(fluxdata_hsd_cum, by = c("Site","Treatment")) %>% 
  ungroup %>% 
  dplyr::mutate(MEAN_CO2_FLUX_ugC_gSoil_hr = paste(MEAN_CO2_FLUX_ugC_gSoil_hr, mean_hsd),
                CUM_CO2_FLUX_mgC_gSoil = paste(CUM_CO2_FLUX_mgC_gSoil, cum_hsd)) %>% 
  dplyr::select(Site, Treatment, MEAN_CO2_FLUX_ugC_gSoil_hr, CUM_CO2_FLUX_mgC_gSoil)
  
### OUTPUT
write.csv(fluxdata2, FLUX_DATA)
write.csv(fluxdata_summary, FLUX_SUMMARY)

#


# temporal progression of respiration # moved to Markdown ----
#
# CLEANING UP FLUXDATA FILE ----
coreweights = read.csv(CORE_WEIGHTS)

fluxdata_kp = 
  fluxdata %>% 
  left_join(dplyr::select(coreweights, SampleID, dry_g), by = "SampleID") %>% 
  dplyr::mutate(gravimetric = round((Total_core_mass_pot_pie_pans_g - dry_g - DryMass_NONsoil_ALL_g - Additional_Wt_toRemove) / dry_g,4),
                CO2_mgC_gSoil_hr = CO2_flux_mgC_hr/dry_g,
                CH4_mgC_gSoil_hr = CH4_flux_mgC_hr/dry_g) %>% 
  dplyr::select(SampleID, Site, Treatment, TREATMENT_PHASE,
                dry_g, gravimetric,
                DATETIME, CO2_mgC_gSoil_hr, CH4_mgC_gSoil_hr) %>% 
  filter(!TREATMENT_PHASE == "DROUGHT_PRE_INCUBATION") %>% 
  filter(!TREATMENT_PHASE == "DROUGHT4C_PRE_INCUBATION") %>% 
  group_by(SampleID) %>% 
  dplyr::mutate(TREATMENT_PHASE = factor(TREATMENT_PHASE, levels = 
                                              c("DROUGHT4C_PRE_INCUBATION","DROUGHT_PRE_INCUBATION","DROUGHT_INCUBATION", "DROUGHT_SATURATION",
                                                "FIELD_MOIST_INCUBATION","FIELD_MOIST_SATURATION",
                                                "IMMEDIATE_SATURATION","SATURATION_SATURATION","SATURATION_INCUBATION")),
                time_hours = as.numeric(difftime(DATETIME, min(DATETIME), units = "hours")))
  
ggplot(fluxdata_kp, aes(x = TREATMENT_PHASE, y = gravimetric, color = Treatment, label=SampleID))+
  geom_point(position = position_dodge(width = 0.5))+
  facet_wrap(~SampleID)+
  #geom_text(aes(label = SampleID))+
  theme(axis.text.x = element_text(angle=45,hjust=1))

### OUTPUT
write_csv(fluxdata_kp, "processed/fluxdata_kp.csv")


## porewater volumes ----

pore_test = 
  porewater_weight %>%
  dplyr::rename(Core = CoreNo) %>% 
  left_join(dplyr::select(core_weights, Site, Treatment, Core), by = c("Site","Core"))

ggplot(pore_test, aes(x = Treatment, y = totalvolume_mL, color = Treatment))+
  geom_point()+
  #facet_grid(Site~Suction)
  facet_wrap(Site~Core)

##

## comparing with porewater WSOC ----

pore_wsoc = read.csv("processed/wsoc_pores_longform.csv")
fluxdata_kp = read.csv("processed/fluxdata_kp.csv")

flux_pca = 
  fluxdata_kp %>% 
  dplyr::mutate(CO2_mgC_gSoil_hr = if_else(CO2_mgC_gSoil_hr>0,CO2_mgC_gSoil_hr, as.numeric(NA)),
                CH4_mgC_gSoil_hr = if_else(CH4_mgC_gSoil_hr>0,CH4_mgC_gSoil_hr, as.numeric(NA))) %>% 
  group_by(SampleID, TREATMENT_PHASE) %>% 
  dplyr::summarise(CO2 = mean(CO2_mgC_gSoil_hr, na.rm = TRUE),
                   CH4 = mean(CH4_mgC_gSoil_hr, na.rm = TRUE)) %>% 
  right_join(pore_wsoc, by = c("SampleID" = "CoreID"), all.y = TRUE) %>% 
  dplyr::select(-CO2,-CH4,CO2,CH4) %>% 
  ungroup

# do a pca on this
library(ggbiplot)
library(vegan)
library("ape")

flux_pca_num_fine = 
  flux_pca %>% 
  filter(TREATMENT_PHASE %in% c("DROUGHT_INCUBATION","FIELD_MOIST_INCUBATION","SATURATION_INCUBATION")) %>% 
  filter(Suction=="50 kPa") %>% 
  dplyr::select(.,-(1:6)) %>% 
  replace(.,is.na(.),0)

flux_pca_grp_fine = 
  flux_pca %>% 
  filter(TREATMENT_PHASE %in% c("DROUGHT_INCUBATION","FIELD_MOIST_INCUBATION","SATURATION_INCUBATION")) %>% 
  filter(Suction=="50 kPa") %>% 
  dplyr::select(.,(1:6)) %>% 
  dplyr::mutate(row = row_number())

df_f <- flux_pca_num_fine[,apply(flux_pca_num_fine, 2, var, na.rm=TRUE) != 0]

pca = prcomp(flux_pca_num_fine, scale. = T)
summary(pca)

ggbiplot(pca, obs.scale = 1, var.scale = 1, 
         groups = flux_pca_grp_fine$Site, ellipse = TRUE, circle = TRUE,
         var.axes = TRUE)
ggbiplot(pca, obs.scale = 1, var.scale = 1, 
         groups = flux_pca_grp_fine$Treatment, ellipse = TRUE, circle = TRUE,
         var.axes = TRUE)


flux_pca_num_coarse = 
  flux_pca %>% 
  filter(TREATMENT_PHASE %in% c("DROUGHT_INCUBATION","FIELD_MOIST_INCUBATION","SATURATION_INCUBATION")) %>% 
 # filter(Suction=="1.5 kPa") %>% 
  dplyr::select(.,-(1:6)) %>% 
  replace(.,is.na(.),0)

flux_pca_grp_coarse = 
  flux_pca %>% 
  filter(TREATMENT_PHASE %in% c("DROUGHT_INCUBATION","FIELD_MOIST_INCUBATION","SATURATION_INCUBATION")) %>% 
  #filter(Suction=="1.5 kPa") %>% 
  dplyr::select(.,(1:6)) %>% 
  dplyr::mutate(row = row_number())

df_f <- flux_pca_num_fine[,apply(flux_pca_num_fine, 2, var, na.rm=TRUE) != 0]

pca = prcomp(flux_pca_num_coarse, scale. = T)
summary(pca)

ggbiplot(pca, obs.scale = 1, var.scale = 1, 
         groups = flux_pca_grp_coarse$Site, ellipse = TRUE, circle = TRUE,
         var.axes = TRUE)
ggbiplot(pca, obs.scale = 1, var.scale = 1, 
         groups = flux_pca_grp_coarse$Treatment, ellipse = TRUE, circle = TRUE,
         var.axes = TRUE)
#


## anova for birch effect ----
flux_drought = 
  fluxdata_kp %>% 
  na.omit() %>% 
  filter(!SampleID=="S5") %>% 
  filter(Treatment=="drought") %>% 
  dplyr::mutate(CO2_mgC_gSoil_hr = if_else(CO2_mgC_gSoil_hr>0,CO2_mgC_gSoil_hr, as.numeric(NA)),
                CH4_mgC_gSoil_hr = if_else(CH4_mgC_gSoil_hr>0,CH4_mgC_gSoil_hr, as.numeric(NA)))
  

## ## HSD. DONT DO ----
 fit_aov_birch <- function(dat) {
   a <-anova(lm(CO2_mgC_gSoil_hr ~ TREATMENT_PHASE, data = dat))
   #h <-HSD.test(a,"treatment")
   #create a tibble with one column for each treatment
   #the hsd results are row1 = drought, row2 = saturation, row3 = time zero saturation, row4 = field moist. hsd letters are in column 2
   tibble(`birch` = a$`Pr(>F)`[1])
 }
 
flux_drought %>%
  filter(!SampleID %in% c("S10","S25")) %>% 
   group_by(Site, SampleID) %>% 
   do(fit_aov_birch(.)) %>% 
  dplyr::mutate(birch = round(birch,4),
                p = if_else(birch<0.05,"*",as.character(NA))) %>% 
  ungroup()->
   flux_drought_anova

flux_drought_summary = 
  flux_drought %>% 
  group_by(Site, TREATMENT_PHASE) %>% 
  dplyr::summarise(CO2 = round(mean(CO2_mgC_gSoil_hr*1e3, na.rm=TRUE),2)) %>% 
  spread(TREATMENT_PHASE, CO2)  
  left_join(dplyr::select(flux_drought_anova,SampleID,p), by = "SampleID")



#### IGNORE THIS MESS BELOW ----
## testing for missing data in CPCRW drought
rawdata_samples = read_csv(RAWDATA_SAMPLES_FILE)

test = 
  rawdata_samples %>% 
  filter(Site=="CPCRW") %>% 
  left_join(dplyr::select(valvemap, SampleID,Site, Treatment, TREATMENT_PHASE, sequence_valve), by = c("MPVPosition"="sequence_valve"))



test = 
  valvemap %>% 
  filter(Site=="CPCRW") %>% 
  dplyr::select(SampleID,Site, Treatment, TREATMENT_PHASE, sequence_valve) %>% 
  left_join(rawdata_samples, by = c("sequence_valve"="MPVPosition"))

ggplot(test, aes(x = DATETIME, y = CO2_dry, color = TREATMENT_PHASE))+
  geom_point()


test = 
  rawdata_samples %>% 
  ungroup %>% 
  # we only look at first 45 seconds, after first few
  filter(elapsed_seconds <= MAX_MAXCONC_TIME,
         elapsed_seconds >= MIN_MEASUREMENT_TIME) %>% 
  # find max CO2 time for each sample
  group_by(samplenum) %>% 
  dplyr::mutate(N = n(),
                max_co2_time = nth(elapsed_seconds, which.max(CO2_dry))) %>%
  # filter for at least 3 data points and for max CO2 time window
  filter(N >= 3, 
         elapsed_seconds <= max_co2_time) 
test2 = 
  test %>% 
  left_join(dplyr::select(valvemap, SampleID,Site, Treatment, TREATMENT_PHASE, sequence_valve), by = c("MPVPosition"="sequence_valve")) %>% 
  filter(Site=="CPCRW") %>% 
  filter(TREATMENT_PHASE=="DROUGHT_INCUBATION") %>% 
  filter(DATETIME>2016-11-08)
group_by(DATETIME, SampleID, samplenum, Site, Treatment,TREATMENT_PHASE) %>% 
  dplyr::summarize(CO2_dry = mean(CO2_dry),
                   max_co2_time = max(max_co2_time)) %>% 
  group_by(SampleID, TREATMENT_PHASE) %>% 
  dplyr::mutate(inctime_hours = as.numeric(difftime(DATETIME, min(DATETIME), units = "hours")))


ggplot(test2, aes(x = inctime_hours, y = CO2_dry, color=SampleID))+
  geom_point()

valvemap %>% 
  filter(Site=="CPCRW") %>% 
  dplyr::select(SampleID,Site, Treatment, TREATMENT_PHASE, sequence_valve) %>% 
  left_join(dplyr::select(rawdata_samples, MPVPosition), by = c("sequence_valve"="MPVPosition"))

##




