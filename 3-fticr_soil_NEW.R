### FTICR soil data
### this script is for cleaning and processing and creating the smaller, separate files that can then be used for analysis and graphing

### Kaizad F. Patel
### August 2019

source("0-packages.R")

## step 1: load and merge the files ----
# data are split into (a) metadata and (b) sample data
# read_cvs reads the zipped files without extracting
fticr_soil_meta = read_csv("data/FTICR_INPUT_SOIL_META.csv.zip")
fticr_soil_data = read_csv("data/FTICR_INPUT_SOIL_DATA.csv.zip")
corekey = read.csv("data/COREKEY.csv")

# remove unnecessary columns from meta

fticr_soil_meta = fticr_soil_meta[,!colnames(fticr_soil_meta)%in%
                                    c("C13","Error_ppm", "Candidates", "GFE", "bs1_class","bs2_class")]
fticr_soil_meta %>% 
  dplyr::rename(OC = OtoC_ratio,
                HC = HtoC_ratio)->
  fticr_soil_meta

# merge metadata with sample data
# ¿¿¿ do this later instead?
fticr_soil = merge(fticr_soil_meta,fticr_soil_data,by = "Mass")

#
## step 2: clean and process ----

# remove "unassigned" molecules
fticr_soil = fticr_soil[!(fticr_soil$Class=="Unassigned"),]

# melt/gather. transform from wide to long form
fticr_soil_gather = fticr_soil %>% 
  gather(core, intensity, C1:S25) ## core = name of new categ column, intensity = name of values column, C1:C25 are columns that are collapsed

# remove all samples with zero intensity
fticr_soil_gather = fticr_soil_gather[!(fticr_soil_gather$intensity=="0"),]

# merge with the core key file
fticr_soil_gather = merge(corekey,fticr_soil_gather,by = "core")


fticr_soil_gather %>% 
  group_by(Mass,treatment,site) %>% 
  dplyr::mutate(reps = n()) ->
  fticr_soil_gather2

### remove peaks seen in < 3 replicates 
fticr_soil_gather2 = fticr_soil_gather2[!(fticr_soil_gather2$reps<3),]

#write.csv(fticr_soil_gather,"fticr_soil_gather.csv")

### OUTPUT
# write.csv(fticr_soil_gather2,"fticr_soil_longform.csv")
write_csv(fticr_soil_gather2, FTICR_SOIL_LONG)

#
## step 3: relative abundance ---- 

# summarizing by groups
fticr_soil_gather2 %>% 
  group_by(site, treatment,Class,core) %>% 
  dplyr::summarize(compounds = sum(intensity)) ->
  fticr_soil_groups

fticr_soil_groups$compounds = as.numeric(fticr_soil_groups$compounds)

fticr_soil_groups_wide = spread(fticr_soil_groups,Class,compounds)

fticr_soil_groups_wide %>%
  mutate(total = rowSums(.[4:12])) ->
  fticr_soil_groups_wide

## relative abundance
fticr_soil_groups_wide[,-c(1:3)] %>% 
  sapply('/', fticr_soil_groups_wide$total/100)->
  fticr_soil_abundance

fticr_soil_abundance2 = data.frame(fticr_soil_abundance)
corenames = data.frame(fticr_soil_groups_wide[,c(1:3)])

fticr_soil_relabundance = cbind(corenames,fticr_soil_abundance2)

### OUTPUT
# write.csv(fticr_soil_relabundance,"fticr_soil_relabund_cores.csv")
# write_csv(fticr_soil_relabundance,path = "fticr/fticr_soil_relabund_cores.csv")

## relative abundance by treatment/site
fticr_soil_relabundance_long = fticr_soil_relabundance %>% 
  gather(group, relabund, AminoSugar:total)


fticr_soil_relabundance_summary = summarySE(fticr_soil_relabundance_long, measurevar = "relabund", groupvars = c("site","treatment","group"),na.rm = TRUE)
fticr_soil_relabundance_summary$relativeabundance = paste((round(fticr_soil_relabundance_summary$relabund,2)),
                                                           "\u00B1",
                                                           round(fticr_soil_relabundance_summary$se,2))


# OPTION1: UNNECESSARY NOW
    # fticr_soil_relabundance_summarytable = dcast(fticr_soil_relabundance_summary,site+treatment~group,value.var = "relativeabundance") 
    
    # move Unnamed and total columns to the end
    # fticr_soil_relabundance_summarytable %>% 
    #  select(-Unnamed,Unnamed) %>% 
    #  select(-total,total) ->
    #  fticr_soil_relabundance_summarytable
    
    # remove +/- SE values for the total column
    # fticr_soil_relabundance_summarytable$total="100"
    
    ### OUTPUT
    # write.csv(fticr_soil_relabundance_summarytable,"fticr_soil_relabundance_groups.csv")
    # write_csv(fticr_soil_relabundance_summarytable,path = "fticr/fticr_soil_relabundance_groups.csv")
  

## option2
# relativeabundance for the total rows is 100 +/- 0. set it to 100
setDT(fticr_soil_relabundance_summary)[group=="total", relativeabundance := "100"]

# set total as last factor
old.lvl = levels(factor(fticr_soil_relabundance_summary$group))
fticr_soil_relabundance_summary$group = factor(fticr_soil_relabundance_summary$group, 
                                            levels=c(sort(old.lvl[old.lvl!="total"]), "total"))


    # cast the table in a different manner, with groups as rows
    # fticr_soil_relabundance_summarytable2 = dcast(fticr_soil_relabundance_summary,
    #                                              site+group~treatment,value.var = "relativeabundance") 


## HSD 
fit_hsd_relabund <- function(dat) {
  a <-aov(relabund ~ treatment, data = dat)
  h <-HSD.test(a,"treatment")
  #create a tibble with one column for each treatment
  #the hsd results are row1 = drought, row2 = saturation, row3 = time zero saturation, row4 = field moist. hsd letters are in column 2
  tibble(`drought` = h$groups["drought",2], 
         `saturation` = h$groups["saturation",2],
         `time zero saturation` = h$groups["time zero saturation",2],
         `field moist` = h$groups["field moist",2],
         `baseline` = h$groups["baseline",2])
}

fticr_soil_relabundance_long[!fticr_soil_relabundance_long$group=="total",] %>% 
  group_by(site, group) %>% 
  do(fit_hsd_relabund(.))  ->
  soil_relabund_hsd

soil_relabund_hsd %>% 
  gather(treatment, hsd, 3:7)-> #gather columns 4-7 (treatment levels)
  soil_relabund_hsd2

# now merge this with `fticr_soil_relabundance_summary`

fticr_soil_relabundance_summary2 = merge(fticr_soil_relabundance_summary, soil_relabund_hsd2, by = c("site","group","treatment"))

# combine hsd and values and thenremove unnecessary columns
fticr_soil_relabundance_summary2 %>% 
  mutate(relabund_hsd = paste(relativeabundance," ",hsd)) %>% 
  select(-sd,-se,-ci,-hsd)->
  fticr_soil_relabundance_summary2

### OUTPUT
write_csv(fticr_soil_relabundance_summary2, FTICR_SOIL_RELABUND)


## peaks ----
fticr_soil_gather2 %>% 
  group_by(site,treatment,Class) %>% 
  dplyr::summarize(peaks = n()) %>% # get count of each group/class for each tension-site-treatment
  group_by(site,treatment) %>% 
  dplyr::mutate(total = sum(peaks)) -> # then create a new column for sum of all peaks for each tension-site-treatment
  fticr_soil_peaks

# we need to combine the total value into the existing groups column
fticr_soil_peaks %>% 
  spread(Class,peaks) %>% # first, convert into wide-form, so each group is a column
  dplyr::select(-total,total) %>% # move total to the end
  gather(Class,peaks_count,AminoSugar:total)-> # combine all the groups+total into a single column
  fticr_soil_peaks2


### OUTPUT
write_csv(fticr_soil_peaks2,FTICR_SOIL_PEAKS)

#

## step 4: molecules added/lost ----
## to determine which molecules were created or lost due to the treatments

# first, combine all cores within each treatment
fticr_soil_gather_trtsummary = summarySE(fticr_soil_gather2, measurevar = "intensity", groupvars = c("site", "treatment", "Mass"))

# second, split the file into three, for the three sites
fticr_soil_gather_trtsummary_c = fticr_soil_gather_trtsummary[fticr_soil_gather_trtsummary$site=="CPCRW",1:4]
fticr_soil_gather_trtsummary_d = fticr_soil_gather_trtsummary[fticr_soil_gather_trtsummary$site=="DWP",1:4]
fticr_soil_gather_trtsummary_s = fticr_soil_gather_trtsummary[fticr_soil_gather_trtsummary$site=="SR",1:4]

# third, make a new column that is binary for presence (1) and absence (0). if N>0, a molecule is present
setDT(fticr_soil_gather_trtsummary_c)[N>0, presence := "1"]  

# fourth, split the treatment column into five separate columns. this will make further calculations easier.
fticr_soil_gather_trtsummary_c2 = dcast(fticr_soil_gather_trtsummary_c, Mass ~ treatment, value.var = "presence")

# fifth, replace all NA with 0 for subsequent subtraction
fticr_soil_gather_trtsummary_c2 %>% 
  replace(.,is.na(.),"0") ->
  fticr_soil_gather_trtsummary_c2

# sixth, convert all columns into numeric and transform this mess into a dataframe. the mutate functions (next) will not work if it is not a dataframe. fml
fticr_soil_gather_trtsummary_c2 = sapply(fticr_soil_gather_trtsummary_c2, as.numeric)
fticr_soil_gather_trtsummary_c2 = data.frame(fticr_soil_gather_trtsummary_c2)

# seventh, create new columns for molecules added/lost for each treatment. subtract all treatments from baseline
# +1 = new molecule, -1 = lost molecule, 0 = molecule present or absent in treatment&baseline
fticr_soil_gather_trtsummary_c2 %>% 
  mutate(drought_new = drought - baseline) %>% 
  mutate(saturation_new = saturation - baseline) %>% 
  mutate(fieldmoist_new = field.moist - baseline) %>% 
  mutate(tzsat_new = time.zero.saturation - baseline) ->
  fticr_soil_gather_trtsummary_c2

# eighth, create a new column for unique molecules
setDT(fticr_soil_gather_trtsummary_c2)[baseline==0 & time.zero.saturation==1 & saturation==0 & drought==0 & field.moist==0, unique := "time_zero_sat"]
fticr_soil_gather_trtsummary_c2[baseline==0 & time.zero.saturation==0 & saturation==1 & drought==0 & field.moist==0, unique := "saturation"]
fticr_soil_gather_trtsummary_c2[baseline==0 & time.zero.saturation==0 & saturation==0 & drought==1 & field.moist==0, unique := "drought"]
fticr_soil_gather_trtsummary_c2[baseline==0 & time.zero.saturation==0 & saturation==0 & drought==0 & field.moist==1, unique := "field_moist"]

# ninth, add 'site' column for when we bind later
fticr_soil_gather_trtsummary_c2$site="CPCRW"


## repeat all steps with DWP

setDT(fticr_soil_gather_trtsummary_d)[N>0, presence := "1"]  

# 
fticr_soil_gather_trtsummary_d2 = dcast(fticr_soil_gather_trtsummary_d, Mass ~ treatment, value.var = "presence")

# 
fticr_soil_gather_trtsummary_d2 %>% 
  replace(.,is.na(.),"0") ->
  fticr_soil_gather_trtsummary_d2

# 
fticr_soil_gather_trtsummary_d2 = sapply(fticr_soil_gather_trtsummary_d2, as.numeric)
fticr_soil_gather_trtsummary_d2 = data.frame(fticr_soil_gather_trtsummary_d2)

# 
fticr_soil_gather_trtsummary_d2 %>% 
  mutate(drought_new = drought - baseline) %>% 
  mutate(saturation_new = saturation - baseline) %>% 
  mutate(fieldmoist_new = field.moist - baseline) %>% 
  mutate(tzsat_new = time.zero.saturation - baseline) ->
  fticr_soil_gather_trtsummary_d2

# 
setDT(fticr_soil_gather_trtsummary_d2)[baseline==0 & time.zero.saturation==1 & saturation==0 & drought==0 & field.moist==0, unique := "time_zero_sat"]
fticr_soil_gather_trtsummary_d2[baseline==0 & time.zero.saturation==0 & saturation==1 & drought==0 & field.moist==0, unique := "saturation"]
fticr_soil_gather_trtsummary_d2[baseline==0 & time.zero.saturation==0 & saturation==0 & drought==1 & field.moist==0, unique := "drought"]
fticr_soil_gather_trtsummary_d2[baseline==0 & time.zero.saturation==0 & saturation==0 & drought==0 & field.moist==1, unique := "field_moist"]

# 
fticr_soil_gather_trtsummary_d2$site="DWP"


## now repeat all with SR

setDT(fticr_soil_gather_trtsummary_s)[N>0, presence := "1"]  

# 
fticr_soil_gather_trtsummary_s2 = dcast(fticr_soil_gather_trtsummary_s, Mass ~ treatment, value.var = "presence")

# 
fticr_soil_gather_trtsummary_s2 %>% 
  replace(.,is.na(.),"0") ->
  fticr_soil_gather_trtsummary_s2

# 
fticr_soil_gather_trtsummary_s2 = sapply(fticr_soil_gather_trtsummary_s2, as.numeric)
fticr_soil_gather_trtsummary_s2 = data.frame(fticr_soil_gather_trtsummary_s2)

# 
fticr_soil_gather_trtsummary_s2 %>% 
  mutate(drought_new = drought - baseline) %>% 
  mutate(saturation_new = saturation - baseline) %>% 
  mutate(fieldmoist_new = field.moist - baseline) %>% 
  mutate(tzsat_new = time.zero.saturation - baseline) ->
  fticr_soil_gather_trtsummary_s2

# 
setDT(fticr_soil_gather_trtsummary_s2)[baseline==0 & time.zero.saturation==1 & saturation==0 & drought==0 & field.moist==0, unique := "time_zero_sat"]
fticr_soil_gather_trtsummary_s2[baseline==0 & time.zero.saturation==0 & saturation==1 & drought==0 & field.moist==0, unique := "saturation"]
fticr_soil_gather_trtsummary_s2[baseline==0 & time.zero.saturation==0 & saturation==0 & drought==1 & field.moist==0, unique := "drought"]
fticr_soil_gather_trtsummary_s2[baseline==0 & time.zero.saturation==0 & saturation==0 & drought==0 & field.moist==1, unique := "field_moist"]

# 
fticr_soil_gather_trtsummary_s2$site="SR"


### now, combine all three dataframes

fticr_soil_new = rbind (fticr_soil_gather_trtsummary_c2,fticr_soil_gather_trtsummary_d2,fticr_soil_gather_trtsummary_s2)

## remove the presence/absence columns, they are now unnecessary
fticr_soil_new = data.frame(fticr_soil_new[,-c(2:6)])
fticr_soil_new[,2:7] = sapply(fticr_soil_new[,2:7], as.factor)

## recode 1, -1 to presence and absence. do this in new columnns
## this dataframe now has the original numeric coding for new/lost, the new character coding for new/lost, and a column for unique molecules in each treatment

fticr_soil_new %>% 
  mutate(drought = case_when(
    drought_new=="1"~"new",
    drought_new=="-1"~"lost")) %>% 
      mutate(saturation = case_when(
        saturation_new=="1"~"new",
        saturation_new=="-1"~"lost")) %>%
          mutate(field.moist = case_when(
            fieldmoist_new=="1"~"new",
            fieldmoist_new=="-1"~"lost")) %>% 
              mutate(time.zero.saturation = case_when(
                tzsat_new=="1"~"new",
                tzsat_new=="-1"~"lost")) ->
  fticr_soil_new

## subset only the character coding columns for new/lost 
fticr_soil_new %>% 
  select("Mass","site","time.zero.saturation","saturation","field.moist","drought")->
  fticr_soil_new2

## melt/gather into long-form. make new columns for treatment and newmolecules
fticr_soil_new2 = (fticr_soil_new2) %>% 
  gather(treatment, newmolecules,time.zero.saturation:drought)


## merge the file with the metadata

fticr_soil_new3 = merge(fticr_soil_meta,fticr_soil_new2, by = "Mass")

## OUTPUT
write_csv(fticr_soil_new3, path = "fticr/fticr_soil_newmolecules.csv")


#
## step 4b: unique molecules ----

fticr_soil_unique = fticr_soil_new %>% 
  select("Mass","site","unique")
  

## merge the file with metadata

fticr_soil_unique2 = merge(fticr_soil_meta, fticr_soil_unique, by = "Mass")

### OUTPUT
write_csv(fticr_soil_unique2, FTICR_SOIL_UNIQUE)


#
#
## unique peaks ----
fticr_soil_unique2 %>% 
  group_by(site, unique,Class) %>% 
  dplyr::summarize(peaks_count = n()) %>% 
  group_by(site,unique) %>% 
  dplyr::mutate(total = sum(peaks_count))->
  fticr_soil_unique_peaks

fticr_soil_unique_peaks %>% 
  spread(Class,peaks_count) %>% # first, convert into wide-form, so each group is a column
  dplyr::select(-total,total) %>% # move total to the end
  gather(Class,peaks_count,AminoSugar:total)-> # combine all the groups+total into a single column
  fticr_soil_unique_peaks2

fticr_soil_unique_peaks2 = fticr_soil_unique_peaks2[complete.cases(fticr_soil_unique_peaks2),]

### OUTPUT
write_csv(fticr_soil_unique_peaks2,FTICR_SOIL_UNIQUE_PEAKS)


#
## step 5: HC, OC data for van krevelen ----

# subset only select columns from fticr_soil_gather2

fticr_soil_gather2 %>% 
  select("core","Mass","HC","OC","intensity") %>% 
  mutate(HC = round(HC,2)) %>%   
  mutate(OC = round(OC,2))  %>% 
  mutate(intensity = round(intensity,2))  ->
  fticr_soil_hcoc
## R message: Adding missing grouping variables: `treatment`, `site`

### OUTPUT
# write.csv(fticr_soil_hcoc,"fticr_soil_hcoc.csv")
write_csv(fticr_soil_hcoc, FTICR_SOIL_HCOC)

#

## step 6: NOSC data ----
fticr_soil_gather2 %>% 
  select("core","Mass","NOSC","intensity") %>% 
  mutate(NOSC = round(NOSC,4)) ->
  fticr_soil_nosc

### OUTPUT
# write.csv(fticr_soil_nosc,"fticr_soil_nosc.csv")
write_csv(fticr_soil_nosc, FTICR_SOIL_NOSC)

#

## step 7: kendrick mass data ----
# subset

fticr_soil_gather2 %>% 
  select("core","Mass","kmass","kdefect","intensity") %>% 
  mutate(kmass = round(kmass,2)) %>% 
  mutate(kdefect = round(kdefect,4)) %>% 
  mutate(intensity = round(intensity,2))  ->
  fticr_soil_kendrick

### OUTPUT
# write.csv(fticr_soil_kendrick,"fticr_soil_kendrick.csv")
write_csv(fticr_soil_kendrick,path = "fticr/fticr_soil_kendrick.csv")

#

## step 8: aromatic peaks ----
fticr_soil_gather2 %>% 
  select("core","Mass","AI","AI_Mod","intensity") %>% 
  mutate(AI = round(AI,4)) %>% 
  mutate(AI_Mod = round(AI_Mod,4)) %>% 
  mutate(intensity = round(intensity,2))->
  fticr_soil_aromatic

setDT(fticr_soil_aromatic)[AI_Mod>0.5, aromatic := "aromatic"]

### OUTPUT
# write.csv(fticr_soil_aromatic,"fticr_soil_aromatic.csv")

fticr_soil_aromatic %>% 
  group_by(site, treatment, core, aromatic) %>% 
  dplyr::mutate(arom_core_counts = n()) ->
  fticr_soil_aromatic

fticr_soil_aromatic = fticr_soil_aromatic[complete.cases(fticr_soil_aromatic),]
write_csv(fticr_soil_aromatic,path = "fticr/fticr_soil_aromatic.csv")

# summary by treatment. then remove NA to keep only aromatic counts
fticr_soil_aromatic_counts = summarySE(fticr_soil_aromatic, measurevar = "arom_core_counts", groupvars = c("core","aromatic","site","treatment"))
fticr_soil_aromatic_counts = fticr_soil_aromatic_counts[complete.cases(fticr_soil_aromatic_counts),]

### OUTPUT
# write.csv(fticr_soil_aromatic_counts,"fticr_soil_aromatic_counts.csv")
write_csv(fticr_soil_aromatic_counts, FTICR_SOIL_AROMATIC)

#