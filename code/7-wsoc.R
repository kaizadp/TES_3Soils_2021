# 3Soils 2019
# This script reads WSOC data files for (a) soil extracts and (b) pore water samples, 
# create summary tables and graphs in Markdown
# Kaizad F. Patel, Aug 2019

source("code/0b-packages.R")

#
# 1. WSOC concentrations -- soils ---- ----
## 1a. load and process files ----
# wsoc_soils = read_excel("data/3Soils_WSOC_CN_PoreCore_soils.xlsx")
wsoc_soils = read_csv("data/wsoc_cn_soil.csv")
wsoc_soils$wsoc_mg_g = wsoc_soils$`WSOC mgCg-1soil`
wsoc_soils$Treatment = factor(wsoc_soils$Treatment,
                              levels = c("Time Zero Saturation",
                                         "Field Moisture Incubation",
                                         "Saturation Incubation",
                                         "Drought Incubation"),
                              labels = c("Time Zero",
                                         "Field Moist",
                                         "Saturated",
                                         "Drought"))

wsoc_soils_rmisc = summarySE(wsoc_soils,measurevar = "wsoc_mg_g",groupvars = c("Site","Treatment"),na.rm = TRUE)
wsoc_soils_rmisc = wsoc_soils_rmisc[complete.cases(wsoc_soils_rmisc),]
wsoc_soils_rmisc$WSOC_mg_g = paste(round(wsoc_soils_rmisc$wsoc_mg_g,2),"\u00B1",round(wsoc_soils_rmisc$se,2))
#\u00b1 is plus-minus
#


#
## 1b. DUNNETT TEST ----

fit_dunnett_wsoc_soil <- function(dat) {
  d <-DescTools::DunnettTest(wsoc_mg_g~Treatment, control = "Time Zero", data = dat)
  #create a tibble with one column for each treatment
  # column 4 has the pvalue
  t = tibble(`Drought` = d$`Time Zero`["Drought-Time Zero",4], 
             `Saturated` = d$`Time Zero`["Saturated-Time Zero",4],
             `Field Moist` = d$`Time Zero`["Field Moist-Time Zero",4])
  # we need to convert significant p values to asterisks
  # since the values are in a single row, it is tricky
  t %>% 
    # first, gather all p-values into a single column, pval
    gather(trt, pval, 1:3) %>% 
    # conditionally replace all significant pvalues (p<0.05) with asterisks and the rest remain blank
    mutate(p = if_else(pval<0.05, "*","")) %>% 
    # remove the pval column
    dplyr::select(-pval) %>% 
    # spread the p (asterisks) column bnack into the three columns
    spread(trt, p)  ->
    t
}

wsoc_soils = wsoc_soils[complete.cases(wsoc_soils),]

wsoc_soils %>% 
  group_by(Site) %>% 
  do(fit_dunnett_wsoc_soil(.))  ->
  wsoc_soil_dunnett

wsoc_soil_dunnett %>% 
  gather(Treatment, dunnett, 2:4)-> #gather columns 4-7 (treatment levels)
  wsoc_soil_dunnett2

# merge the summary table with the hsd/dunnett table
wsoc_soils_rmisc %>% 
  left_join(wsoc_soil_dunnett2,by = c("Site","Treatment"), all.x = TRUE) %>% 
  replace(.,is.na(.),"") %>% 
  dplyr::mutate(wsoc_dunnett = paste(WSOC_mg_g,dunnett)) %>% 
  dplyr::select(-sd,-se,-ci,-dunnett)->
  wsoc_soils_summary


### OUTPUT
write.csv(wsoc_soils_summary, WSOC_SOIL, row.names = FALSE)

#

#
# ------------------------------------------------------ ----

# 2.  WSOC concentrations -- pores ----
## 2a. concentrations as mg/L ----
# wsoc_pores_temp = read_excel("data/3Soils_WSOC_CN_PoreCore.xlsx") 
wsoc_pores_temp = read_csv("data/wsoc_cn_pores.csv") 
wsoc_pores_temp %>% 
  dplyr::rename(wsoc_mg_L = `Water Soluble Organic Carbon (mg/L)`) %>% 
  dplyr::select(-FTICR_ID, -`FT-ICRvol_ml`, -`Pore Size Domain`, -`NPOC (M/L)`,-SampleID) %>% 
#rename and reorder factors in Treatment 
  dplyr::mutate(Treatment = factor(Treatment,
                              levels = c("Time Zero Saturation",
                                         "Field Moisture Incubation",
                                         "Saturation Incubation",
                                         "Drought Incubation"),
                              labels = c("Time Zero",
                                         "Field Moist",
                                         "Saturated",
                                         "Drought")))->
  wsoc_pores

#

## 2b. create summary table
wsoc_pores %>% 
  filter(!is.na(wsoc_mg_L)) %>% 
  group_by(Suction,Site, Treatment) %>% 
  # make summary columns for mg/L and mg/g values
  dplyr::summarize(wsoc_mgL_mean = mean(wsoc_mg_L, na.rm = TRUE),
                   mgL_se = sd(wsoc_mg_L)/sqrt(n())) %>% 
  dplyr::mutate(wsoc_mgL_mean_se = paste(round(wsoc_mgL_mean,2),"\u00B1",round(mgL_se,2))) %>% 
  ungroup %>% 
  dplyr::select(Site,Treatment,Suction,wsoc_mgL_mean, wsoc_mgL_mean_se)->
  wsoc_pores_summary


## 2c. WSOC concentrations -- pores -- stats DUNNETT ----

 fit_dunnett_wsoc <- function(dat) {
   d <-DescTools::DunnettTest(wsoc_mg_L~Treatment, control = "Time Zero", data = dat)
   #create a tibble with one column for each treatment
   # column 4 has the pvalue
   t = tibble(`Drought` = d$`Time Zero`["Drought-Time Zero",4], 
              `Saturated` = d$`Time Zero`["Saturated-Time Zero",4],
              `Field Moist` = d$`Time Zero`["Field Moist-Time Zero",4])
   # we need to convert significant p values to asterisks
   # since the values are in a single row, it is tricky
   t %>% 
     # first, gather all p-values into a single column, pval
     gather(trt, pval, 1:3) %>% 
     # conditionally replace all significant pvalues (p<0.05) with asterisks and the rest remain blank
     mutate(p = if_else(pval<0.05, "*","")) %>% 
     # remove the pval column
     dplyr::select(-pval) %>% 
     # spread the p (asterisks) column bnack into the three columns
     spread(trt, p)  ->
     t
 }
 
 wsoc_pores %>%
   group_by(Site, Suction) %>% 
   do(fit_dunnett_wsoc(.)) %>% 
   gather(Treatment, dunnett, 3:5)-> #gather columns 4-7 (treatment levels)
   wsoc_pores_dunnett
 
## 2d. merge the summary table with the hsd/dunnett table ----
 wsoc_pores_summary %>% 
   left_join(wsoc_pores_dunnett,by = c("Site","Suction","Treatment"), all.x = TRUE) %>% 
   replace(.,is.na(.),"") %>% 
   dplyr::mutate(wsoc_mgL = paste(wsoc_mgL_mean_se,dunnett))  %>% 
   dplyr::select(-dunnett, -wsoc_mgL_mean_se) ->
   wsoc_pores_summary
 
## 2e. ANOVA on SR treatments ----
 wsoc_pores_sr = 
   wsoc_pores %>% 
   filter(Site %in% "SR",
          !Treatment %in% "Time Zero")
 
 a_fine = aov(wsoc_mg_L~Treatment, data = wsoc_pores_sr[wsoc_pores_sr$Suction=="50 kPa",])
 a_coarse = aov(wsoc_mg_L~Treatment, data = wsoc_pores_sr[wsoc_pores_sr$Suction=="1.5 kPa",])
 a_all = aov(wsoc_mg_L~Treatment, data = wsoc_pores_sr)
 
 summary(a_fine)
 summary(a_coarse)
 summary(a_all)
 
 ### OUTPUT ---- 

write.csv(wsoc_pores_summary, WSOC_PORE, row.names = FALSE)
write.csv(wsoc_pores, "processed/wsoc_pores_longform.csv", row.names = FALSE)
 

##
#