# 3Soils
# 4-fticr_abundance 
# Kaizad F. Patel
# October 2019

# this file takes the processed fticr files from script 3 and calculates peaks and abundances 

source("0-packages.R")

# ------------------------------------------------------- ----

# FILES ----
## use the raw-long files for relative abundance only
## use the long files for peaks

soil_meta = read.csv(FTICR_SOIL_META)# <- "fticr/fticr_soil_meta.csv"
#FTICR_SOIL_META_HCOC <- "fticr/soil_meta_hcoc.csv"
soil_raw_long = read.csv(FTICR_SOIL_RAW_LONG)# <- "fticr/fticr_soil_raw_longform.csv"
soil_long = read.csv(FTICR_SOIL_LONG)# <- "fticr/fticr_soil_longform.csv"

pore_meta = read.csv(FTICR_PORE_META)# <- "fticr/fticr_pore_meta.csv"
pore_long = read.csv(FTICR_PORE_LONG)# <- "fticr/fticr_pore_longform.csv"
pore_raw_long = read.csv(FTICR_PORE_RAW_LONG)# <- "fticr/fticr_pore_raw_longform.csv"

# ------------------------------------------------------- ----

# PART I: SOIL PEAKS ----

soil_long %>% 
  group_by(site,treatment,Class) %>% 
  dplyr::summarize(peaks = n()) %>% # get count of each group/class for each tension-site-treatment
  group_by(site,treatment) %>% 
  dplyr::mutate(total = sum(peaks))%>%  # then create a new column for sum of all peaks for each tension-site-treatment
# we need to combine the total value into the existing groups column
  ungroup %>% 
  spread(Class,peaks) %>% # first, convert into wide-form, so each group is a column
  dplyr::select(-total,total) %>% # move total to the end
  gather(Class,peaks_count,AminoSugar:total)-> # combine all the groups+total into a single column
  fticr_soil_peaks


### OUTPUT 
write.csv(fticr_soil_peaks,FTICR_SOIL_PEAKS, row.names = FALSE)

# PART II: SOIL UNIQUE PEAKS ----
soil_long %>% 
  spread(treatment, intensity) %>% 
# add columns for new/lost molecules
  dplyr::mutate(drought2 = case_when(!is.na(drought)&is.na(baseline) ~ "new",
                                        is.na(drought)&!is.na(baseline) ~ "lost"),
                  fm2 = case_when(!is.na(`field moist`)&is.na(baseline) ~ "new",
                                        is.na(`field moist`)&!is.na(baseline) ~ "lost"),
                  saturation2 = case_when(!is.na(saturation)&is.na(baseline) ~ "new",
                                        is.na(saturation)&!is.na(baseline) ~ "lost")) %>% 
# add columns for unique peaks
  dplyr:: mutate(unique = case_when((drought2=="new" & is.na(fm2) & is.na(saturation2)) ~ "drought",
                                    (saturation2=="new" & is.na(fm2) & is.na(drought2)) ~ "saturation",
                                    (fm2=="new" & is.na(drought2) & is.na(saturation2)) ~ "field moist")) %>% 
  dplyr::select(-drought, -saturation, -baseline, -`field moist`,-`time zero saturation`)-> 
  soil_unique_peaks

### OUTPUT
write.csv(soil_unique_peaks,FTICR_SOIL_UNIQUE, row.names = FALSE)



# PART III: SOIL AROMATIC PEAKS ----
meta_aromatic <- soil_meta %>% 
  dplyr::select(Mass, AI_Mod)
  
soil_raw_long %>%
  left_join(meta_aromatic, by = "Mass") %>% 
# create a column designating aromatic  vs. aliphatic
# aromatic == AI_Mod > 0.5, aliphatic == 1.5 < HC < 2.0
# see Bailey et al. 2017 SBB, Chasse et al. 2015 for references
  dplyr::mutate(aromatic = case_when(AI_Mod>0.5 ~ "aromatic", 
                                     (HC<2.0 & HC>1.5) ~ "aliphatic"))  ->
  soil_aromatic

soil_aromatic %>% 
  drop_na %>% 
  group_by(site, treatment, core, aromatic) %>% 
  dplyr::summarize(counts = n())->
  soil_aromatic_counts

### OUTPUT
# write.csv(fticr_soil_aromatic_counts,"fticr_soil_aromatic_counts.csv")
write_csv(soil_aromatic_counts, FTICR_SOIL_AROMATIC)

# ------------------------------------------------------- ----

# PART IV: SOIL RELATIVE ABUNDANCE ----
## step 1: create a summary table by group/treatment

soil_raw_long %>% 
  group_by(site, treatment,Class,core) %>% 
  dplyr::summarize(compounds = sum(intensity)) %>% # sum all intensities for each Class
# now calculate relative abundance for each Class for each core
  group_by(site, treatment, core) %>% 
  dplyr::mutate(total = sum(compounds),
                relabund = (compounds/total)*100)->
  relabund_temp

relabund_temp%>% 
  # now summarize by treatment. combine cores
  ungroup %>% 
  dplyr::group_by(site, treatment, Class) %>% 
  dplyr::summarize(relabund2 = mean(relabund),
                   se = sd(relabund)/sqrt(n())) %>% 
  # create a column of relabund +/- se  
  dplyr::mutate(relabund = paste(round(relabund2,2),"\u00B1",round(se,2))) %>% 
  # we need to add a total column
  dplyr::mutate(total = 100) %>% 
  dplyr::select(-se) -> 
  soil_relabund



relabund_temp%>% 
# now summarize by treatment. combine cores
  ungroup %>% 
  dplyr::group_by(site, treatment, Class) %>% 
  dplyr::summarize(relabund2 = mean(relabund),
                   se = sd(relabund)/sqrt(n())) %>% 
# create a column of relabund +/- se  
  dplyr::mutate(relabund = paste(round(relabund2,2),"\u00B1",round(se,2))) %>% 
# we need to add a total column
  dplyr::mutate(total = 100) %>% 
  dplyr::select(-se,-relabund2) %>% 
# we need to bring the total column into the Class.
# so first spread the class column and then melt back together
  spread(Class, relabund) %>% 
  melt(id = c("site","treatment")) %>% 
  dplyr::rename(Class = variable,
                relabund= value)->
  soil_relabund
# we will combine this file with the Dunnett test results below
  

## step 2: DUNNETT'S TEST 

fit_dunnett_relabund <- function(dat) {
  d <-DescTools::DunnettTest(relabund~treatment, control = "baseline", data = dat)
  #create a tibble with one column for each treatment
  # column 4 has the pvalue
  t = tibble(`drought` = d$`baseline`["drought-baseline",4], 
             `saturation` = d$`baseline`["saturation-baseline",4],
             `field moist` = d$`baseline`["field moist-baseline",4],
             `TZsaturation` = d$baseline["time zero saturation-baseline",4])
  # we need to convert significant p values to asterisks
  # since the values are in a single row, it is tricky
  t %>% 
    # first, gather all p-values into a single column, pval
    gather(trt, pval, 1:4) %>% 
    # conditionally replace all significant pvalues (p<0.05) with asterisks and the rest remain blank
    mutate(p = if_else(pval<0.05, "*","")) %>% 
    # remove the pval column
    dplyr::select(-pval) %>% 
    # spread the p (asterisks) column bnack into the three columns
    spread(trt, p)  ->
    t
}

relabund_temp[!relabund_temp$Class=="total",] %>% 
  group_by(site, Class) %>% 
  do(fit_dunnett_relabund(.)) %>% 
  melt(id = c("site","Class"), value.name = "dunnett", variable.name = "treatment")-> #gather columns 4-7 (treatment levels)
  soil_relabund_dunnett

## step 3: now merge this with `soil_relabund`

soil_relabund %>% 
  left_join(soil_relabund_dunnett,by = c("site","Class","treatment"), all.x = TRUE) %>% 
  replace(.,is.na(.),"") %>% 
  dplyr::mutate(relativeabundance = paste(relabund,dunnett)) %>% 
  dplyr::select(-relabund, -dunnett) ->
  fticr_soil_relativeabundance


#
      ## ## HSD. DONT DO ----
      ## fit_hsd_relabund <- function(dat) {
      ##   a <-aov(relabund ~ treatment, data = dat)
      ##   h <-HSD.test(a,"treatment")
      ##   #create a tibble with one column for each treatment
      ##   #the hsd results are row1 = drought, row2 = saturation, row3 = time zero saturation, row4 = field moist. hsd letters are in column 2
      ##   tibble(`drought` = h$groups["drought",2], 
      ##          `saturation` = h$groups["saturation",2],
      ##          `time zero saturation` = h$groups["time zero saturation",2],
      ##          `field moist` = h$groups["field moist",2],
      ##          `baseline` = h$groups["baseline",2])
      ## }
      ## 
      ## fticr_soil_relabundance_long[!fticr_soil_relabundance_long$group=="total",] %>% 
      ##   group_by(site, group) %>% 
      ##   do(fit_hsd_relabund(.))  ->
      ##   soil_relabund_hsd
      ## 
      ## soil_relabund_hsd %>% 
      ##   gather(treatment, hsd, 3:7)-> #gather columns 4-7 (treatment levels)
      ##   soil_relabund_hsd2
      ## 
      ## # now merge this with `fticr_soil_relabundance_summary`
      ## 
      ## fticr_soil_relabundance_summary2 = merge(fticr_soil_relabundance_summary, soil_relabund_hsd2, by = c("site","group","treatment"))
      ## 
      ## # combine hsd and values and thenremove unnecessary columns
      ## fticr_soil_relabundance_summary2 %>% 
      ##   mutate(relabund_hsd = paste(relativeabundance," ",hsd)) %>% 
      ##   select(-sd,-se,-ci,-hsd)->
      ##   fticr_soil_relabundance_summary2




### OUTPUT ----
write_csv(fticr_soil_relativeabundance, FTICR_SOIL_RELABUND)


# ------------------------------------------------------- ----
# ------------------------------------------------------- ----

# PART V: POREWATER PEAKS ----
pore_long %>% 
  group_by(tension,site,treatment,Class) %>% 
  dplyr::summarize(peaks = n()) %>% # get count of each group/class for each tension-site-treatment
  group_by(tension,site,treatment) %>% 
  dplyr::mutate(total = sum(peaks))%>%  # then create a new column for sum of all peaks for each tension-site-treatment
  # we need to combine the total value into the existing groups column
  ungroup %>% 
  spread(Class,peaks) %>% # first, convert into wide-form, so each group is a column
  dplyr::select(-total,total) %>% # move total to the end
  gather(Class,peaks_count,AminoSugar:total)-> # combine all the groups+total into a single column
  fticr_pore_peaks


### OUTPUT 
write_csv(fticr_pore_peaks,FTICR_PORE_PEAKS)

#
# PART VI: PORE UNIQUE PEAKS ----
pore_long %>% 
  spread(treatment, intensity) %>% 
  # add columns for new/lost molecules
  dplyr::mutate(drought2 = case_when(!is.na(drought)&is.na(`time zero saturation`) ~ "new",
                                     is.na(drought)&!is.na(`time zero saturation`) ~ "lost"),
                fm2 = case_when(!is.na(`field moist`)&is.na(`time zero saturation`) ~ "new",
                                is.na(`field moist`)&!is.na(`time zero saturation`) ~ "lost"),
                saturation2 = case_when(!is.na(saturation)&is.na(`time zero saturation`) ~ "new",
                                        is.na(saturation)&!is.na(`time zero saturation`) ~ "lost")) %>% 
  # add columns for unique peaks
  dplyr:: mutate(unique = case_when((drought2=="new" & is.na(fm2) & is.na(saturation2)) ~ "drought",
                                    (saturation2=="new" & is.na(fm2) & is.na(drought2)) ~ "saturation",
                                    (fm2=="new" & is.na(drought2) & is.na(saturation2)) ~ "field moist")) %>% 
  dplyr::select(-drought, -saturation, -`field moist`,-`time zero saturation`)-> 
  pore_unique_peaks

### OUTPUT
write.csv(pore_unique_peaks,FTICR_PORE_UNIQUE, row.names = FALSE)


#
# PART VII: PORE AROMATIC PEAKS ----
meta_aromatic <- pore_meta %>% 
  dplyr::select(Mass, AImod) %>% 
  dplyr::rename(AI_Mod = AImod)

pore_raw_long %>%
  left_join(meta_aromatic, by = "Mass") %>% 
  # create a column designating aromatic  vs. aliphatic
  # aromatic == AI_Mod > 0.5, aliphatic == 1.5 < HC < 2.0
  # see Bailey et al. 2017 SBB, Chasse et al. 2015 for references
  dplyr::mutate(aromatic = case_when(AI_Mod>0.5 ~ "aromatic", 
                                     (HC<2.0 & HC>1.5) ~ "aliphatic"))  ->
  pore_aromatic

pore_aromatic %>% 
  drop_na %>% 
  group_by(tension,site, treatment, core, aromatic) %>% 
  dplyr::summarize(counts = n())->
  pore_aromatic_counts



### OUTPUT
# write.csv(fticr_soil_aromatic_counts,"fticr_soil_aromatic_counts.csv")
write_csv(pore_aromatic_counts, FTICR_PORE_AROMATIC)

#





# ------------------------------------------------------- ----

# PART IV: PORE RELATIVE ABUNDANCE ----
## step 1: create a summary table by group/treatment

pore_raw_long %>% 
  group_by(tension,site, treatment,Class,core) %>% 
  dplyr::summarize(compounds = sum(intensity)) %>% # sum all intensities for each Class
  # now calculate relative abundance for each Class for each core
  group_by(tension,site, treatment, core) %>% 
  dplyr::mutate(total = sum(compounds),
                relabund = (compounds/total)*100)->
  relabund_temp

relabund_temp%>% 
  # now summarize by treatment. combine cores
  ungroup %>% 
  dplyr::group_by(tension,site, treatment, Class) %>% 
  dplyr::summarize(relabund2 = mean(relabund),
                   se = sd(relabund)/sqrt(n())) %>% 
  # create a column of relabund +/- se  
  dplyr::mutate(relabund = paste(round(relabund2,2),"\u00B1",round(se,2))) %>% 
  # we need to add a total column
  dplyr::mutate(total = 100) %>% 
  dplyr::select(-se) -> 
  pore_relabund
# we will combine this file with the Dunnett test results below


## step 2: DUNNETT'S TEST 

fit_dunnett_relabund <- function(dat) {
  d <-DescTools::DunnettTest(relabund~treatment, control = "time zero saturation", data = dat)
  #create a tibble with one column for each treatment
  # column 4 has the pvalue
  t = tibble(drought = d$`time zero saturation`["drought-time zero saturation",4], 
             saturation = d$`time zero saturation`["saturation-time zero saturation",4],
             `field moist` = d$`time zero saturation`["field moist-time zero saturation",4])
  # we need to convert significant p values to asterisks
  # since the values are in a single row, it is tricky
  t %>% 
    # first, gather all p-values into a single column, pval
    gather(trt, pval, 1:3) %>% 
    # conditionally replace all significant pvalues (p<0.05) with asterisks and the rest remain blank
    dplyr::mutate(p = if_else(pval<0.05, "*","")) %>% 
    # remove the pval column
    dplyr::select(-pval) %>% 
    # spread the p (asterisks) column bnack into the three columns
    spread(trt, p)  ->
    t
}

relabund_temp[!relabund_temp$Class=="Other",] %>% 
  ungroup %>% 
  dplyr::group_by(tension,site,Class) %>% 
  do(fit_dunnett_relabund(.)) %>% 
  melt(id = c("tension","site","Class"), value.name = "dunnett", variable.name = "treatment")-> #gather columns 4-7 (treatment levels)
  pore_relabund_dunnett

## step 3: now merge this with `soil_relabund`

pore_relabund %>% 
  left_join(pore_relabund_dunnett,by = c("tension","site","Class","treatment"), all.x = TRUE) %>% 
  replace(.,is.na(.),"") %>% 
  dplyr::mutate(relativeabundance = paste(relabund,dunnett)) %>% 
  dplyr::select(-relabund, -dunnett)->
  fticr_pore_relabundance


### OUTPUT
write.csv(fticr_pore_relabundance, FTICR_PORE_RELABUND)

# ------------------------------------------------------- ----

# PART V: SHANNON DIVERSITY ----
# Shannon diversity, H = - sum [p*ln(p)], where n = no. of individuals per species/total number of individuals
pore_raw_long %>% 
  group_by(tension,site,treatment, core,Class) %>% 
  dplyr::summarize(n = n()) %>%
  ungroup %>% 
  group_by(tension,site,treatment,core) %>% 
  dplyr::mutate(total = sum(n),
                p = n/total,
                log = log(p),
                p_logp = p*log) %>% 
  dplyr::summarize(H1 = sum(p_logp),
                H = round(-1*H1, 2)) %>% 
  dplyr::select(-H1)->pore_shannon 

### OUTPUT
write.csv(pore_shannon, FTICR_PORE_DIVERSITY)

#
# ------------------------------------------------------- ----









