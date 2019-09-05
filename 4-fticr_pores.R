### FTICR soil data
### this script is for cleaning and processing and creating the smaller, separate files that can then be used for analysis and graphing

### Kaizad F. Patel
### August 2019

source("0-packages.R")

## step 1: load the files ----

fticr_porewater = read_csv("data/FTICR_INPUT_SOILPORE.csv")
corekey = read.csv("data/COREKEY.csv")

### need to use google sheets instead of csv files
# https://drive.google.com/file/d/1dMjnCnMUYa5XY2ypVjz2HBQKx7E0YJY1/view?usp=sharing
# use report sn3

write_csv(fticr_porewater, path = "fticr/fticr_porewater.csv")

#
## step 2: clean and process ----

# 2a: remove unnecessary columns. LOTS of unnecessary columns. fml. #### 
fticr_porewater=select(fticr_porewater, -c(`C13`,
                                           `3use`,
                                           `Error_ppm`,
                                           `21T_CCS-2_Day1_1-D7_04Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day1_1-S23_04Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day1_5-C7_04Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day1_5-S17-b_04Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day1_SRFAII_04Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day1_SRFAII-1_04Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day1_SRFAII-2_04Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day10_1-D7-rr_400scans`,
                                           `21T_CCS-2_Day10_5-C11_15Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day10_5-C7-rr_400scans`,
                                           `21T_CCS-2_Day10_SRFAII_15Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day2_1-C7_04Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day2_1-S23_04Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day2_5-D10_04Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day2_5-S17_04Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day2_SRFAII_04Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day2_SRFAII_end_04Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day3_1-C7-a_08Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day3_1-D7_08Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day3_5-C7_08Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day3_5-D10_08Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day3_SRFAII_08Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day3_SRFAII-1_08Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day3_SRFAII-2_08Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day3-rr_SRFAII_08Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day4_1-D7_08Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day4_1-S23_08Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day4_5-C7-a_08Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day4_5-D10_08Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day4_5-S17_08Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day4_SRFAII_08Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day5_1-C7_08Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day5_1-S23_08Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day5_5-D10_08Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day5_5-S17_08Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day5_SRFAII_08Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day6_1-C7_11Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day6_1-D7_11Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day6_5-C7_11Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day6_5-D10_11Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day6_5-D14_11Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day6_SRFAII_11Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day6_SRFAII-1_11Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day6_SRFAII-2_11Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day7_1-D7-rr_400scans`,
                                           `21T_CCS-2_Day7_1-S23-1_11Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day7_5-C7_11Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day7_5-S17_11Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day7_SRFAII_11Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day8_1-C11_11Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day8_1-C16_11Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day8_1-C20_11Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day8_1-C7_11Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day8_1-S23_11Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day8_1-S23_15Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day8_5-C16_11Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day8_5-D10_11Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day8_5-S11_11Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day8_5-S17_11Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day8_5-S17-rr_400scans`,
                                           `21T_CCS-2_Day8_SRFAII_15Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day8_SRFAII-1_15Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day8_SRFAII-2_15Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day9_1-C7_15Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day9_1-C7-rr_15Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day9_1-D15-rr_15Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day9_1-D7_15Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day9_5-C24_15Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day9_5-C7_15Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day9_5-D10_15Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day9_5-D10-rr_15Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day9_SRFAII_15Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day9_SRFAII-1_15Jan18_Leopard_Infuse-qb`,
                                           `21T_CCS-2_Day9_SRFAII-2_15Jan18_Leopard_Infuse-qb`)
)


# cleaning up sample names because WTF 
# removing strings in increments
names(fticr_porewater) = gsub("21T_CCS-2_", "", names(fticr_porewater))
names(fticr_porewater) = gsub("Day1_", "", names(fticr_porewater))
names(fticr_porewater) = gsub("Day2_", "", names(fticr_porewater))
names(fticr_porewater) = gsub("Day3_", "", names(fticr_porewater))
names(fticr_porewater) = gsub("Day4_", "", names(fticr_porewater))
names(fticr_porewater) = gsub("Day5_", "", names(fticr_porewater))
names(fticr_porewater) = gsub("Day6_", "", names(fticr_porewater))
names(fticr_porewater) = gsub("Day7_", "", names(fticr_porewater))
names(fticr_porewater) = gsub("Day8_", "", names(fticr_porewater))
names(fticr_porewater) = gsub("Day9_", "", names(fticr_porewater))
names(fticr_porewater) = gsub("Day10_", "", names(fticr_porewater))
names(fticr_porewater) = gsub("_04Jan18_Leopard_Infuse-qb", "", names(fticr_porewater))
names(fticr_porewater) = gsub("_08Jan18_Leopard_Infuse-qb", "", names(fticr_porewater))
names(fticr_porewater) = gsub("_11Jan18_Leopard_Infuse-qb", "", names(fticr_porewater))
names(fticr_porewater) = gsub("_15Jan18_Leopard_Infuse-qb", "", names(fticr_porewater))
names(fticr_porewater) = gsub("-rr", "", names(fticr_porewater))
names(fticr_porewater) = gsub("-rr_400scans", "", names(fticr_porewater))
names(fticr_porewater) = gsub("_400scans", "", names(fticr_porewater))
names(fticr_porewater) = gsub("-a", "", names(fticr_porewater))
names(fticr_porewater) = gsub("-b", "", names(fticr_porewater))
names(fticr_porewater) = gsub("-2", "", names(fticr_porewater))


names(fticr_porewater)


## split porewater file into metadata and sample data. ----
## sample data split by pore size (50 kPa and 1.5 kPa). 
fticr_pore_meta = as.data.frame(fticr_porewater[,1:11])
fticr_pore_data_50 = (select(fticr_porewater, "Mass", starts_with("5")))
fticr_pore_data_1 = (select(fticr_porewater, "Mass", starts_with("1")))

## meta: remove componds without class
fticr_pore_meta = fticr_pore_meta[!(fticr_pore_meta$Class=="None"),]

## create new columns for Aromatic Index, Nominal Oxidative State of Compounds, H/C, and O/C

#AImod = (1+C-1/2 O-S-1/2H)/(C=1/2 O -S-N-P). Formula from Bailey et al. 2017 SOC pore water
fticr_pore_meta$AImod = with(fticr_pore_meta, (1+C-(0.5*O)-S-(0.5*H))/(C-(0.5*O)-S-N-P))

#NOSC = 4-((4C+H-3N-2O-2S)/C). From Riedel et al. in Coward et al. 2019
fticr_pore_meta$NOSC = with(fticr_pore_meta, 4-(((4*C)+H-(3*N)-(2*O)-(2*S))/C))

fticr_pore_meta$HC = with(fticr_pore_meta, H/C)
fticr_pore_meta$OC = with(fticr_pore_meta, O/C)


# melt/gather. transform from wide to long form
## using "gather" in the name despite using melt, to keep it consistent with the fticr_soil script
fticr_pore_gather_50 = fticr_pore_data_50 %>% 
  melt(id = "Mass")
fticr_pore_gather_1 = fticr_pore_data_1 %>% 
  melt(id = "Mass")


# remove all samples with zero intensity
fticr_pore_gather_50 = fticr_pore_gather_50[!(fticr_pore_gather_50$value=="0"),]
fticr_pore_gather_1 = fticr_pore_gather_1[!(fticr_pore_gather_1$value=="0"),]

# combine the 50 and 1.5 kPa files. rbind combines vertically
fticr_pore_gather = rbind(fticr_pore_gather_1,fticr_pore_gather_50)

# create columns for core # and tension
# use tension_2 to code the tension column using the stringr extraction function, and then delete tension_2 because it is unnecessary
fticr_pore_gather %>% 
  dplyr::rename(intensity = value) %>% # the melt function gave us a column value, rename to intensity
  mutate(tension_2 = substr(variable,start=1,stop=1)) %>% 
  mutate(core = substr(variable,start=3,stop=7)) %>% 
  mutate(tension = case_when(
    tension_2=="1"~"1.5 kPa",
    tension_2=="50"~"50 kPa")) ->
  fticr_pore_gather

fticr_pore_gather %>% 
  select(-tension_2,-variable) ->
  fticr_pore_gather

# merge with the core key file
fticr_pore_gather = left_join(corekey,fticr_pore_gather, by = "core")
#remove NA
fticr_pore_gather = fticr_pore_gather[complete.cases(fticr_pore_gather),]


### remove peaks seen in < 3 replicates 
# first, group 
# then, remove

fticr_pore_gather %>% 
  group_by(Mass,treatment,site, tension) %>% 
  dplyr::mutate(reps = n()) ->
  fticr_pore_gather2

fticr_pore_gather2 = fticr_pore_gather2[!(fticr_pore_gather2$reps<3),]

# write.csv(fticr_soil_gather,"fticr_soil_gather.csv")

### OUTPUT
write_csv(fticr_pore_gather2, path = "fticr/fticr_pore_longform.csv")

#
## step 3: relative abundance ---- 
# merge the file with the metadata
fticr_pore_gather2  = left_join(fticr_pore_meta, fticr_pore_gather2, by = "Mass")
fticr_pore_gather2 = fticr_pore_gather2[complete.cases(fticr_pore_gather2),]

# summarizing by groups
fticr_pore_gather2 %>% 
  group_by(site, treatment,Class,core, tension) %>% 
  dplyr::summarize(compounds = sum(intensity)) ->
  fticr_pore_groups

fticr_pore_groups$compounds = as.numeric(fticr_pore_groups$compounds)

fticr_pore_groups_wide = spread(fticr_pore_groups,Class,compounds)
fticr_pore_groups_wide = as.data.frame(fticr_pore_groups_wide)

# create a `total` column adding counts across all "group" columns (columns 5-13)
fticr_pore_groups_wide %>%
  replace(.,is.na(.),0) ->
  fticr_pore_groups_wide

fticr_pore_groups_wide %>%
  mutate(total = rowSums(.[5:13])) ->
  fticr_pore_groups_wide

## relative abundance:
# split the dataset into (a) just the abundance values for easy calculations, and (b) the core key. Then combine again.
fticr_pore_groups_wide[,-c(1:4)] %>% 
  sapply('/', fticr_pore_groups_wide$total)->
  fticr_pore_abundance

fticr_pore_abundance2 = data.frame(fticr_pore_abundance)
corenames = data.frame(fticr_pore_groups_wide[,c(1:4)])

fticr_pore_relabundance = cbind(corenames,fticr_pore_abundance2)

### OUTPUT
# write.csv(fticr_soil_relabundance,"fticr_soil_relabund_cores.csv")
write_csv(fticr_pore_relabundance,path = "fticr/fticr_pore_relabund_cores.csv")

## relative abundance by treatment/site
fticr_pore_relabundance_long = fticr_pore_relabundance %>% 
  gather(group, relabund, AminoSugar:total)


fticr_pore_relabundance_summary = summarySE(fticr_pore_relabundance_long, measurevar = "relabund", groupvars = c("site","treatment","group","tension"),na.rm = TRUE)
fticr_pore_relabundance_summary$relativeabundance = paste((round(fticr_pore_relabundance_summary$relabund,3)),
                                                           "\u00B1",
                                                           round(fticr_pore_relabundance_summary$se,3))

fticr_pore_relabundance_summarytable = dcast(fticr_pore_relabundance_summary,site+treatment+tension~group,value.var = "relativeabundance") 

# move Unnamed and total columns to the end
# "Unnamed" is "Other" for pores
fticr_pore_relabundance_summarytable %>% 
  select(-Other,Other) %>% 
  select(-total,total) ->
  fticr_pore_relabundance_summarytable

# remove +/- SE values for the total column
fticr_pore_relabundance_summarytable$total="1"
## ## some cells have +/- 0. probably because n=1 for those. (??!!) double-check. 

### OUTPUT
# write.csv(fticr_soil_relabundance_summarytable,"fticr_soil_relabundance_groups.csv")
write_csv(fticr_pore_relabundance_summarytable,path = "fticr/fticr_pore_relabundance_groups.csv")

#


## step 4: molecules added/lost ----
## step 4b: unique molecules 
### ??? DO WE WANT TO DO THESE? it will become way too complex and not sure if it is giving us anything of value.
#
## step 5: HC, OC data for van krevelen ----

# subset only select columns from fticr_soil_gather2

fticr_pore_gather2 %>% 
  select("core","site","treatment","tension","Mass","HC","OC","intensity") %>% 
  mutate(HC = round(HC,2)) %>%   
  mutate(OC = round(OC,2))  %>% 
  mutate(intensity = round(intensity,2))  ->
  fticr_pore_hcoc
## R message: Adding missing grouping variables: `treatment`, `site` -- happened for soil but not for pores. weird

### OUTPUT
# write.csv(fticr_soil_hcoc,"fticr_soil_hcoc.csv")
write_csv(fticr_pore_hcoc,path = "fticr/fticr_pore_hcoc.csv")

#

## step 6: NOSC data ----
fticr_pore_gather2 %>% 
  select("core","site","treatment","tension","Mass","NOSC","intensity") %>% 
  mutate(NOSC = round(NOSC,4)) ->
  fticr_pore_nosc

### OUTPUT
# write.csv(fticr_soil_nosc,"fticr_soil_nosc.csv")
write_csv(fticr_pore_nosc,path = "fticr/fticr_pore_nosc.csv")

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

setDT(fticr_soil_melt)[variable=="C1"|variable=="C2"|variable=="C3"|variable=="C4"|variable=="C5" ,treatment := "Time Zero"]

setDT(fticr_soil_aromatic)[AI_Mod>0.5, aromatic := "aromatic"]

### OUTPUT
# write.csv(fticr_soil_aromatic,"fticr_soil_aromatic.csv")
write_csv(fticr_soil_aromatic,path = "fticr/fticr_soil_aromatic.csv")

fticr_soil_aromatic %>% 
  group_by(site, treatment, core, aromatic) %>% 
  dplyr::mutate(arom_core_counts = n()) ->
  fticr_soil_aromatic

# summary by treatment. then remove NA to keep only aromatic counts
fticr_soil_aromatic_counts = summarySE(fticr_soil_aromatic, measurevar = "arom_core_counts", groupvars = c("aromatic","site","treatment"))
fticr_soil_aromatic_counts = fticr_soil_aromatic_counts[complete.cases(fticr_soil_aromatic_counts),]

### OUTPUT
# write.csv(fticr_soil_aromatic_counts,"fticr_soil_aromatic_counts.csv")
write_csv(fticr_soil_aromatic_counts,path = "fticr/fticr_soil_aromatic_counts.csv")

#

## MOVE TO NEW SCRIPT step 8b: aromatic peaks - summary ----



ggplot(fticr_soil_aromatic_counts, aes(x = site, y = arom_core_counts, color = treatment, fill = treatment))+
  geom_bar(stat="summary",width=0.1,position=position_dodge(0.7),size=1)+
  geom_errorbar(aes(ymin=`arom_core_counts`-se, ymax=`arom_core_counts`+se),width=0.3,position=position_dodge(0.7),color="black",size=1)
  

#
## MOVE TO NEW SCRIPT step 5: van krevelen plots ----

#
## MOVE TO NEW SCRIPT step 7: NOSC plots ----

ggplot(fticr_soil_nosc[fticr_soil_nosc$site=="CPCRW",], aes(x = NOSC, fill = treatment))+
  geom_histogram(binwidth = 0.25, color = "black")+
  xlim(-2.5, 2.5)

ggplot(fticr_soil_nosc[fticr_soil_nosc$site=="DWP",], aes(x = NOSC, fill = treatment))+
  geom_histogram(binwidth = 0.25, color = "black")

ggplot(fticr_soil_nosc[fticr_soil_nosc$site=="SR",], aes(x = NOSC, fill = treatment))+
  geom_histogram(binwidth = 0.25, color = "black")

#

## MOVE TO NEW SCRIPT step 9: plot kendrick ----

ggplot(fticr_soil_kendrick[fticr_soil_kendrick$site=="CPCRW" & 
                             fticr_soil_kendrick$treatment=="drought" | fticr_soil_kendrick$treatment=="saturation"|fticr_soil_kendrick$treatment=="field moist",], 
       aes(x = kmass, y = kdefect, color = treatment, shape = treatment))+
  geom_point(size=0.5)

ggplot(fticr_soil_kendrick[fticr_soil_kendrick$site=="DWP",], aes(x = kmass, y = kdefect, color = treatment))+
  geom_point(size=1)

ggplot(fticr_soil_kendrick[fticr_soil_kendrick$site=="SR",], aes(x = kmass, y = kdefect, color = treatment))+
  geom_point(size=1)
#


