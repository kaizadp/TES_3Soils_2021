# 3Soils
# 3-fticr_initial processing
# Kaizad F. Patel
# October 2019

 ## this script will process the input data and metadata files and
 ## generate clean files that can be used for subsequent analysis.
 ## each dataset will generate longform files of (a) all cores, (b) summarized data for each treatment (i.e. cores combined) 

source("0b-packages.R")

# ------------------------------------------------------- ----

# PART I: FTICR DATA FOR SOIL EXTRACTS ----

## step 1: load the files ----
# data are split into (a) metadata and (b) sample data
# read_csv reads the zipped files without extracting
fticr_soil_meta = read_csv("data/FTICR_INPUT_SOIL_META.csv.zip")
fticr_soil_data = read_csv("data/FTICR_INPUT_SOIL_DATA.csv.zip")
corekey = read.csv("data/COREKEY.csv")

fticr_soil_meta %>% 
  # remove unnecessary columns
  dplyr::select(-C13,-Error_ppm,-Candidates,-GFE,-bs1_class,-bs2_class) %>% 
  # rename columns
  dplyr::rename(OC = OtoC_ratio,
                HC = HtoC_ratio) %>% 
  filter(!Class=="Unassigned")->
  fticr_soil_meta
## the aromatic index is the corrected index from Dittmar and Koch 2015, https://doi.org/10.1002/rcm.7433

# make a subset for just HCOC and class
fticr_soil_meta %>% 
  dplyr::select(Mass, Class, HC, OC) %>% 
  dplyr::mutate(HC = round(HC, 4),
                OC = round(OC,4))->
  fticr_meta_hcoc

#
## step 2: clean and process ----
fticr_soil_data %>% 
  # melt/gather. transform from wide to long form
  gather(core, intensity, C1:S25) %>% ## core = name of new categ column, intensity = name of values column, C1:C25 are columns that are collapsed
  # remove all samples with zero intensity
  filter(!intensity=="0") %>% 
  # merge with the core key file
  left_join(corekey,by = "core") %>% 
  # merge with hcoc file
  left_join(fticr_meta_hcoc, by = "Mass") %>% 
  group_by(Mass,treatment,site) %>% 
  dplyr::mutate(reps = n())%>% 
  # remove peaks seen in < 3 replicates 
  filter(reps>2) %>% 
  # remove "unassigned" molecules
  filter(!Class=="Unassigned")  ->
  fticr_soil_raw_long
# used to be called:  `fticr_soil_gather2`

## now create a summary of this
fticr_soil_raw_long %>% 
  ungroup %>% 
  group_by(Mass,site,treatment) %>% 
  dplyr::summarize(intensity = mean(intensity)) %>% 
  # merge with hcoc file
  left_join(fticr_meta_hcoc, by = "Mass") ->
  fticr_soil_long

#
### FTICR-SOIL OUTPUT ----

write.csv(fticr_soil_meta, FTICR_SOIL_META, row.names = FALSE)
write.csv(fticr_meta_hcoc, FTICR_SOIL_META_HCOC, row.names = FALSE)
write.csv(fticr_soil_long, FTICR_SOIL_LONG, row.names = FALSE)
write.csv(fticr_soil_raw_long, FTICR_SOIL_RAW_LONG, row.names = FALSE)

#
# ------------------------------------------------------- ----

# PART II: FTICR DATA FOR POREWATER ----
## step 1: load the files ----
# the porewater file contains the Mass/Peak metadata as well as sample intensities data
fticr_porewater = read_csv("data/FTICR_INPUT_SOILPORE.csv.zip")
corekey = read.csv("data/COREKEY.csv")

#
## step 2: clean and process ----

## 2a: remove unnecessary columns. LOTS of unnecessary columns. fml. 
# This uses a seemingly arbitrary list that's experiment-specific. Kind of sucky

# Create a file with the list of columns to drop. 
# use the sample meta file for this. retain SampleType `sample` and `as`. (I don't know what `as` is.)
# metadata of sample information
pore_sample_meta = read_excel("data/FTICR_INPUT_SOILPORE_meta.xlsx")

pore_sample_meta %>% 
  filter(!Sample_Type=="sample") %>% 
  filter(!Sample_Type=="as") %>% 
  dplyr::rename(code = `21T_CCS-2_Day8_1-C11_11Jan18_Leopard_Infuse-qb`) -> 
  # ^^^ rename the f-ing column. WTAF is this column name. Checked -- it's not because a row was moved up. 
  pore_sample_meta

write.csv(pore_sample_meta$code, "data/fticr_columns_to_drop2.txt", row.names = FALSE, quote = FALSE)

# drop unnecessary sample columns 
drops <- readLines("data/fticr_columns_to_drop2.txt")
fticr_porewater[names(fticr_porewater) %in% drops] <- NULL

# clean up sample names because WTF 
# find the sample code (1 number followed by hyphen followed by letter followed by 1-2 numbers)
# example of sample code: 5_C10 == core C10 from CPCRW, -50 kPa porewater

matches <- regexec("[0-9]-[A-Z][0-9]{1,2}", names(fticr_porewater))
matches_n <- unlist(matches)
lengths <- sapply(matches, function(x) attr(x, "match.length"))
# extract the part of the name we want and change
names <- substr(names(fticr_porewater), matches_n, matches_n + lengths - 1)
names(fticr_porewater)[matches_n > 0] <- names[matches_n > 0]

# remove addiitonal unnecessary names that couldn't be automated above
fticr_porewater %>% 
  dplyr::select(-`3use`,-`Error_ppm`)->
  fticr_porewater

### create meta file ----
## sample data split by pore size (50 kPa and 1.5 kPa). 
fticr_porewater %>%
# exclude C13 peaks  
  filter(C13==0) %>% 
  dplyr::select(-C13) %>% 
  dplyr::select(1:11) %>% 
# remove compounds without class. har har. 
  filter(!Class=="None") %>% 
# create new columns  
  dplyr::mutate(AImod = (1+C-(0.5*O)-S-(0.5*(N+P+H)))/(C-(0.5*O)-S-N-P),
                NOSC =  4-(((4*C)+H-(3*N)-(2*O)-(2*S))/C),
                HC = round(H/C,2),
                OC = round(O/C,2))->
  fticr_pore_meta

# calculate molecular formula
# not using molform for further analyses, because it did not change much vs. peaks
fticr_pore_meta %>% 
  dplyr::select(1:8) %>% 
  dplyr::mutate(mol_C = case_when(C==1 ~ paste("C"),
                                  C>1 ~ paste0("C",C),
                                  C==0 ~ NA_character_),
                mol_H = case_when(H==1 ~ paste("H"),
                                  H>1 ~ paste0("H",H),
                                  H==0 ~ NA_character_),
                mol_O = case_when(O==1 ~ paste("O"),
                                  O>1 ~ paste0("O",O),
                                  O==0 ~ NA_character_),
                mol_N = case_when(N==1 ~ paste("N"),
                                  N>1 ~ paste0("N",N),
                                  N==0 ~ NA_character_),
                mol_S = case_when(S==1 ~ paste("S"),
                                  S>1 ~ paste0("S",S),
                                  S==0 ~ NA_character_),
                mol_P = case_when(P==1 ~ paste("P"),
                                  P>1 ~ paste0("P",P),
                                  P==0 ~ NA_character_),
                mol_Na = case_when(Na==1 ~ paste("Na"),
                                  Na>1 ~ paste0("Na",Na),
                                  Na==0 ~ NA_character_),
                MolForm = paste0(mol_C,mol_H,mol_N,mol_O,mol_S,mol_Na),
                MolForm = str_replace_all(MolForm,"NA","")) %>% 
  dplyr::select(Mass,MolForm)->molform_temp


# create subset for HCOC and class
fticr_pore_meta %>% 
  dplyr::select(Mass,HC, OC, Class)->
  fticr_pore_meta_hcoc

#
### create data file ----
fticr_porewater %>%
  dplyr::select(Mass, starts_with("5"), starts_with("1")) %>% 
# collapse all core columns into a single column
  melt(id="Mass") %>% 
  dplyr::rename(sample = variable,
                intensity = value) %>% 
# remove all peaks with intensity==0  
  filter(!intensity==0) %>% 
# using `sample` column, create columns for tension and core
  dplyr::mutate(tension_temp = substr(sample,start=1,stop=1),
                core = substr(sample,start=3,stop=7),
                tension = case_when(
                  tension_temp=="1"~"1.5 kPa",
                  tension_temp=="5"~"50 kPa")) %>% 
# remove unnecessary columns
  dplyr::select(-tension_temp,-sample) %>% 
# merge with the corekey and then remove NA containing rows
  right_join(corekey, by = "core") %>% 
  drop_na->
  temp_pore

# remove peaks seen in < 3 replicates
temp_pore %>% 
  group_by(Mass,tension,site,treatment) %>% 
  dplyr::mutate(reps = n()) %>% 
  filter(reps >2) %>% 
  # merge with hcoc file
  left_join(fticr_pore_meta_hcoc, by = "Mass") %>% 
  drop_na->
  fticr_pore_raw_long

# now create a summary by treatment
fticr_pore_raw_long %>% 
  group_by(Mass,tension,site,treatment) %>% 
  dplyr::summarise(intensity = mean(intensity)) %>% 
  # merge with hcoc file
  left_join(fticr_pore_meta_hcoc, by = "Mass") %>% 
  drop_na->
  fticr_pore_long


### FTICR-PORE OUTPUT ----
write.csv(fticr_pore_meta, FTICR_PORE_META, row.names = FALSE)
write.csv(fticr_pore_long, FTICR_PORE_LONG,row.names = FALSE)
write.csv(fticr_pore_raw_long, FTICR_PORE_RAW_LONG,row.names = FALSE)

#
# ------------------------------------------------------- ----

# PART III: PROCESSING FOR OTHER ANALYSES ----
## a. NOSC  ----
### soil
soil_meta_nosc <- fticr_soil_meta %>% 
  dplyr::select(Mass,NOSC)
soil_nosc <- merge(fticr_soil_long,soil_meta_nosc, by="Mass")

### pore
pore_meta_nosc <- fticr_pore_meta %>% 
  dplyr::select(Mass,NOSC)
pore_nosc <- merge(fticr_pore_long,pore_meta_nosc, by="Mass")

### OUTPUT
write.csv(soil_nosc,FTICR_SOIL_NOSC,row.names = FALSE)
write.csv(pore_nosc,FTICR_PORE_NOSC,row.names = FALSE)


## b. unique to each site ---- pores ----
#### MOVE THIS TO MARKDOWN ----
fticr_pore_raw_long = read.csv(FTICR_PORE_RAW_LONG)# <- "fticr/fticr_pore_longform.csv"
fticr_pore_meta = read.csv(FTICR_PORE_META)

unique_pore_temp = 
  fticr_pore_raw_long %>% 
#  filter(reps==5) %>% 
  group_by(Mass, tension, site,  treatment) %>% 
  dplyr::summarize(presence=1) %>% 
  filter(treatment=="time zero saturation") %>% 
  group_by(Mass,tension) %>% 
  dplyr::mutate(reps=sum(presence)) %>% 
  left_join(dplyr::select(fticr_pore_meta, Mass, HC, OC), by = "Mass")

unique_pore = 
  unique_pore_temp %>% 
  filter(reps==1)

common_pore = 
  unique_pore_temp %>% 
  filter(reps>1) 

gg_vankrev(unique_pore, aes(x = OC, y = HC, color = site))+facet_wrap(~tension)

gg_vankrev(unique_pore_temp, aes(x = OC, y = HC, color = site))+
  scale_color_manual(values = c("blue","yellow","red"))+
  facet_wrap(tension~reps)+
  theme_kp()

gg_vankrev(molform[molform$treatment=="time zero saturation",], aes(x = OC, y = HC, color = site))+
  scale_color_manual(values = c("blue","yellow","red"))+
  facet_wrap(treatment+tension~reps)

molform = 
  fticr_pore_raw_long %>% 
  left_join(molform_temp, by = "Mass") %>% 
  group_by(MolForm, core, site, treatment, tension) %>% 
  dplyr::summarise() %>% 
  group_by(MolForm, site, treatment, tension) %>% 
  dplyr::summarise(reps=n()) %>% 
  left_join(dplyr::select(molform_temp, MolForm, HC, OC), by = "MolForm")

molform_temp = 
  molform_temp %>% 
  left_join(dplyr::select(fticr_pore_meta, Mass, HC, OC), by = "Mass")


unique_pore_temp = 
  molform %>% 
  #  filter(reps==5) %>% 
  group_by(MolForm, tension, site,  treatment) %>% 
  dplyr::summarize(presence=1) %>% 
  filter(treatment=="time zero saturation") %>% 
  group_by(MolForm,tension) %>% 
  dplyr::mutate(reps=sum(presence)) %>% 
  left_join(dplyr::select(molform_temp, MolForm, HC, OC), by = "MolForm")


## unique to each site by treatment ----

unique_pore_temp = 
  fticr_pore_raw_long %>% 
  #  filter(reps==5) %>% 
  group_by(Mass, tension, site,  treatment) %>% 
  dplyr::summarize(presence=1) %>% 
  #filter(treatment=="time zero saturation") %>% 
  group_by(Mass,tension, treatment) %>% 
  dplyr::mutate(reps=sum(presence)) %>% 
  left_join(dplyr::select(fticr_pore_meta, Mass, HC, OC), by = "Mass")

# trying to track molecules. only peaks that were initially unique to each site are plotted across the treatments
# not sure if this even makes sense. remove?
temp = 
  unique_pore_temp %>% 
  dplyr::mutate(remove=case_when((treatment=="time zero saturation" & reps==1)~"keep")) %>% 
  ungroup %>% 
  dplyr::select(Mass, tension, site, remove) %>% 
  na.omit()

unique_pore_temp = 
  unique_pore_temp %>% 
  left_join(temp,  by = c("Mass","tension","site"))
  

gg_vankrev(unique_pore_temp, aes(x = OC, y = HC, color = site))+
  scale_color_manual(values = c("blue","yellow","red"))+
  facet_grid(treatment~reps)
