# 3Soils
# 4-fticr_abundance 
# Kaizad F. Patel
# October 2019

# this file takes the processed fticr files from script 3 and calculates peaks and abundances 

source("0-packages.R")

# ------------------------------------------------------- ----
## use the raw-long files for relative abundance only
## use the long files for peaks

soil_meta = read.csv(FTICR_SOIL_META)# <- "fticr/fticr_soil_meta.csv"
#FTICR_SOIL_META_HCOC <- "fticr/soil_meta_hcoc.csv"
soil_raw_long = read.csv(FTICR_SOIL_RAW_LONG)# <- "fticr/fticr_soil_raw_longform.csv"
soil_long = read.csv(FTICR_SOIL_LONG)# <- "fticr/fticr_soil_longform.csv"

#FTICR_PORE_META <- "fticr/fticr_pore_meta.csv"
#FTICR_PORE_LONG <- "fticr/fticr_pore_longform.csv"
#FTICR_PORE_RAW_LONG <- "fticr/fticr_pore_raw_longform.csv"

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
write_csv(fticr_soil_peaks,FTICR_SOIL_PEAKS)

# ------------------------------------------------------- ----

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

# PART III: SOIL RELATIVE ABUNDANCE ----

  
  













