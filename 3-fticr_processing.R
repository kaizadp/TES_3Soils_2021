# 3Soils
# 3-fticr_initial processing
# Kaizad F. Patel
# October 2019

 ## this script will process the input dta and metadata files and
 ## generate clean files that can be used for subsequent analysis
 ## each dataset will generate longform files of (a) all cores, (b) summarized data for each treatment (i.e. cores combined) 

source("0-packages.R")

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

# make a subset for just HCOC and class
fticr_soil_meta %>% 
  dplyr::select(Mass, Class, HC, OC) %>% 
  dplyr::mutate(HC = round(HC, 4),
                OC = round(OC,4))->
  fticr_meta_hcoc

# make a subset for relevant columns


### OUTPUT

      # merge metadata with sample data
      # ¿¿¿ do this later instead? YES
      # fticr_soil = merge(fticr_soil_meta,fticr_soil_data,by = "Mass")

#
## step 2: clean and process ----
fticr_soil_data %>% 
  # melt/gather. transform from wide to long form
  gather(core, intensity, C1:S25) %>% ## core = name of new categ column, intensity = name of values column, C1:C25 are columns that are collapsed
  # remove all samples with zero intensity
  filter(!intensity=="0") %>% 
  # merge with the core key file
  left_join(corekey,by = "core") %>% 
  ## now we need to filter only those peaks seen in 3 or more replicates
  # add a column with no. of replicates
  group_by(Mass,treatment,site) %>% 
  dplyr::mutate(reps = n()) %>% 
  # remove peaks seen in < 3 replicates 
  filter(reps>2) %>% 
  # merge with hcoc file
  left_join(fticr_meta_hcoc, by = "Mass") %>% 
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

### FTICR-SOIL OUTPUT ----
    # write.csv(fticr_soil_gather2,"fticr_soil_longform.csv")

write.csv(fticr_meta_hcoc, FTICR_META_HCOC, row.names = FALSE)
write.csv(fticr_soil_long, FTICR_SOIL_LONG, row.names = FALSE)
write.csv(fticr_soil_raw_long, FTICR_SOIL_RAW_LONG, row.names = FALSE)

#
# ------------------------------------------------------- ----
# PART II: FTICR DATA FOR POREWATER ----
## step 1: load the files ----
# the porewater file contains the Mass/Peak metadata as well as sample intensities data
fticr_porewater = read_csv("data/FTICR_INPUT_SOILPORE.csv.zip")
corekey = read.csv("data/COREKEY.csv")

# this is the link to the google drive file
# https://drive.google.com/file/d/1dMjnCnMUYa5XY2ypVjz2HBQKx7E0YJY1/view?usp=sharing
# use report sn3

# write_csv(fticr_porewater, path = "fticr/fticr_porewater.csv")

#
## step 2: clean and process ----

## 2a: remove unnecessary columns. LOTS of unnecessary columns. fml. #### 
# This uses a seemingly arbitrary list that's experiment-specific. Kind of sucky

# Create a file with the list of columns to drop. 
# use the sample meta file for this. retain SampleType `sample` and `as`. (I don't know what `as` is.)
# metadata of sample information
pore_sample_meta = read.csv("data/FTICR_INPUT_SOILPORE_meta.csv")

pore_sample_meta %>% 
  filter(!Sample_Type=="sample") %>% 
  filter(!Sample_Type=="as") %>% 
  dplyr::rename(code = `X21T_CCS.2_Day8_1.C11_11Jan18_Leopard_Infuse.qb`) -> 
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
  select(-`C13`,-`3use`,-`Error_ppm`)->
  fticr_porewater

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

### OUTPUT
write_csv(fticr_pore_meta, FTICR_PORE_META)

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
    tension_2=="5"~"50 kPa")) ->
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
write_csv(fticr_pore_gather2, FTICR_PORE_LONG)
