
# load file ----
fluxdata_kp = read.csv("processed/fluxdata_kp.csv")

#

# anova for birch effect ----
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


## LME

fit_lme_birch <- function(dat) {
  l = lme(CO2_mgC_gSoil_hr ~ TREATMENT_PHASE, random = ~1|SampleID, na.action = na.omit, 
          data = dat)
  a = anova(l)
  #h <-HSD.test(a,"treatment")
  #create a tibble with one column for each treatment
  #the hsd results are row1 = drought, row2 = saturation, row3 = time zero saturation, row4 = field moist. hsd letters are in column 2
  tibble(`birch` = a$`p-value`[2])
}

flux_drought %>%
  filter(!SampleID %in% c("S10","S25")) %>% 
  group_by(Site) %>% 
  do(fit_lme_birch(.)) %>% 
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






lme = lme(CO2_mgC_gSoil_hr ~ TREATMENT_PHASE, random = ~1|SampleID, na.action = na.omit, data = flux_drought)
s = summary(lme)
t=s$tTable
t$`p-value`

a = anova(lme)
a$`p-value`[1]



