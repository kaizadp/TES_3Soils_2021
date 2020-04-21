# 3Soils 2019
# This script reads WSOC data files for (a) soil extracts and (b) pore water samples, 
# create summary tables and graphs in Markdown
# Kaizad F. Patel, Aug 2019

source("0b-packages.R")

#
# 1. WSOC concentrations -- soils ---- ----
## 1a. load and process files ----
wsoc_soils = read_excel("data/3Soils_WSOC_CN_PoreCore_soils.xlsx")
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
## 2a. step 1: concentrations as mg/L ----
wsoc_pores = read_excel("data/3Soils_WSOC_CN_PoreCore.xlsx") %>% 
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

## 2b: calculating porewater volume ----
porewater = read.csv("data/Porewater_weights.csv")
names(porewater)

# select only the relevant columns
# these columns are formatted HORRIBLY. redo
# split into two dataframes, one for empty weights and one for full weights. format and then combine

porewater %>% 
  dplyr::rename(Core=`Core.`) %>% 
  dplyr::select(Site, Core, starts_with("X"))->
  porewater_subset

porewater_subset %>% 
  # gather all into columns "type" and "weight"
  gather(type, weight,3:12) %>% 
  # add columns for suction, empty/full, and vial number
  # vial number is by default 1, and for "extra vials", 2
  dplyr::mutate(suction = case_when(grepl("15mb", type) ~ "1.5 kPa",
                                    grepl("150mb", type) ~ "15 kPa",
                                    grepl("500mb",type) ~ "50 kPa"),
                emptyfull = case_when(grepl("empty",type) ~ "empty",
                                      grepl("full", type) ~ "full"),
                vial_num = if_else(grepl("_extra",type),"2","1")) %>%
  # remove the column "type", because tat will f-up the next spread
  dplyr::select(-type) %>% 
  # spread, to get two separate columns for empty vs. full
  spread(emptyfull, weight)->
  porewatersubsetlong

## some cells in this file have multiple entries together, which I cannot/ dont want to fix in R.
## download as a csv, fix this in Excel, and then re-upload here.
## cells with multiple entries will be split into multiple rows, with vial numbers 2,3,4,...

write.csv(porewatersubsetlong,"processed/porewater_subset.csv")

## 2c: NOW that the file has been edited in Excel, reupload it here ----
porewatersubset2 = read.csv("processed/porewater_subset.csv")

porewatersubset2 %>% 
  dplyr::select(-X) %>% 
  filter(!empty==0&!full==0) %>% 
  # create a new column to note missing weights  
  mutate(notes2 = case_when(empty == "no weight recorded"~"empty:no weight recorded",
                            full == "no weight recorded"~"full:no weight recorded")) %>% 
  # make the weight columns numeric
  dplyr::mutate(empty = as.numeric(as.character(empty)),
                full = as.numeric(as.character(full))) %>% 
  # for missing empty weight, take average of all the others
  mutate(empty_corr = if_else(full>80,(2*mean(empty,na.rm = TRUE)),
                              if_else(is.na(empty),mean(empty, na.rm=TRUE),empty))) %>% 
  dplyr::mutate(empty_corr = round(empty_corr,2),
                porewater_g = full-empty_corr) %>% 
  # now create a column for total volume  
  group_by(Site, Core, suction) %>% 
  #  dplyr::summarise(totalvolume_g = sum(porewater_g, na.rm = TRUE))->
  # summarise creates a new summary table
  # to view the totals in the same original table, comment that out and use the `mutate` function below
  dplyr::mutate(totalvolume_g = sum(porewater_g, na.rm = TRUE),
                totalvolume_g = na_if(totalvolume_g,0))->
  porewatersubset3

porewatersubset3 %>% 
  group_by(Site, Core, suction) %>% 
  dplyr::summarise(totalvolume_g = sum(porewater_g, na.rm = TRUE)) %>% 
  dplyr::mutate(totalvolume_g = na_if(totalvolume_g,0)) %>%
  dplyr::rename(totalvolume_mL = totalvolume_g,
                tension=suction) %>% 
  dplyr::rename(CoreNo = Core,
                Suction = tension)->
  porewater_weight

#
## 2d: get dry soil weight ----
core_weight = read.csv(CORE_WEIGHTS) %>% 
  dplyr::rename(CoreID = SampleID) %>% 
  dplyr::select(CoreID, dry_g)->core_weight

## 2e: now combine all, so we can convert mg/L to mg/g
wsoc_pores %>% 
  left_join(porewater_weight, by = c("Site","CoreNo","Suction")) %>% 
  left_join(core_weight, by = "CoreID") %>% 
  dplyr::select(CoreID, CoreNo,Site, Treatment, Suction, wsoc_mg_L, totalvolume_mL,dry_g) %>% 
  dplyr::mutate(wsoc_mgg = wsoc_mg_L * totalvolume_mL/dry_g)->
  wsoc_pores_wt

wsoc_pores_wt %>% 
  filter(!is.na(wsoc_mg_L)) %>% 
  group_by(Suction,Site, Treatment) %>% 
# make summary columns for mg/L and mg/g values
  dplyr::summarize(wsoc_mgg_mean = mean(wsoc_mgg, na.rm = TRUE),
                   mgg_se = sd(wsoc_mgg)/sqrt(n()),
                   wsoc_mgL_mean = mean(wsoc_mg_L, na.rm = TRUE),
                   mgL_se = sd(wsoc_mg_L)/sqrt(n())) %>% 
  dplyr::mutate(wsoc_mgg_mean_se = paste(round(wsoc_mgg_mean,2),"\u00B1",round(mgg_se,2)),
                wsoc_mgL_mean_se = paste(round(wsoc_mgL_mean,2),"\u00B1",round(mgL_se,2))) %>% 
  ungroup %>% 
  dplyr::select(Site,Treatment,Suction,wsoc_mgL_mean,wsoc_mgg_mean_se, wsoc_mgL_mean_se)->
  wsoc_pores_summary



#


## 2f. WSOC concentrations -- pores -- stats DUNNETT ----

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
 
 wsoc_pores_wt %>%
   group_by(Site, Suction) %>% 
   do(fit_dunnett_wsoc(.)) %>% 
   gather(Treatment, dunnett, 3:5)-> #gather columns 4-7 (treatment levels)
   wsoc_pores_dunnett
 
### merge the summary table with the hsd/dunnett table ----
 wsoc_pores_summary %>% 
   left_join(wsoc_pores_dunnett,by = c("Site","Suction","Treatment"), all.x = TRUE) %>% 
   replace(.,is.na(.),"") %>% 
   dplyr::mutate(wsoc_mgL = paste(wsoc_mgL_mean_se,dunnett))  %>% 
   dplyr::select(-dunnett, -wsoc_mgL_mean_se) %>% 
   dplyr::rename(wsoc_mgg = wsoc_mgg_mean_se)->
   wsoc_pores_summary
 
### OUTPUT ---- 

write.csv(wsoc_pores_summary, WSOC_PORE, row.names = FALSE)
write.csv(wsoc_pores, "processed/wsoc_pores_longform.csv", row.names = FALSE)
 

##
#