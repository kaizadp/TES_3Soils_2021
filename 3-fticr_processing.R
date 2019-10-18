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

## step 1: load and merge the files ----
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

## FTICR-SOIL OUTPUT ----
    # write.csv(fticr_soil_gather2,"fticr_soil_longform.csv")

write.csv(fticr_meta_hcoc, FTICR_META_HCOC, row.names = FALSE)
write.csv(fticr_soil_long, FTICR_SOIL_LONG, row.names = FALSE)
write.csv(fticr_soil_raw_long, FTICR_SOIL_RAW_LONG, row.names = FALSE)

#
# ------------------------------------------------------- ----

# PART II: FTICR DATA FOR POREWATER ----
