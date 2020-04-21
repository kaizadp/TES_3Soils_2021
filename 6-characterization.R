# 3Soils 2019
# This script reads and processes data files for soil characterization parameters
# summary tables and graphs will be made in the Markdown script
# Kaizad F. Patel, Aug 2019

source("0b-packages.R")


# 1. nutrients and soil characterization ----

## input files

soil_char = read_excel("data/soil_character.xlsx")

# convert variables to numeric form

soil_character  = 
  soil_char %>%
# remove the grouping variables
# then make all the variables numeric
  dplyr::select(-c(1:3)) %>% 
  dplyr::mutate_if(is.character, as.factor) %>% 
  dplyr::mutate_if(is.factor, as.numeric) %>% 
# now bring back the original grouping variables
  cbind(dplyr::select(soil_char, c(1:3)))


## 1a. creating summary table----
# select only those columns we want for the characterization summary
# manual step

soil_character %>% 
  dplyr::rename(site=Soil) %>% 
  dplyr::select(site,
         TC_perc, TN_perc, TOC_perc, WSOC_mg_g,
         Ca_meq100g, Mg_meq100g,
         pH, EC_dS_m, 
         Sand_perc, Silt_perc, Clay_perc)->
  soil_character2  

# gather
soil_character2 %>% 
  gather(variable, value, 2:12) %>% 
  mutate(value = round(value,2)) %>% 
  mutate(variable = factor(variable, levels = c("TC_perc", "TN_perc", "TOC_perc", "WSOC_mg_g",
                                                "Ca_meq100g", "Mg_meq100g",
                                                "pH", "EC_dS_m", 
                                                "Sand_perc", "Silt_perc", "Clay_perc"))) %>% 
  drop_na()->
  soil_character2_long

soil_character2_long %>% 
  group_by(site, variable) %>% 
  dplyr::summarise(mean = mean(value),
                   se = sd(value)/sqrt(n())) %>% 
  dplyr::mutate(summary = paste(round(mean,2), "\u00B1",round(se,2)))->
  soil_character_summary

#
## 1b. characterization -- stats ----

fit_hsd <- function(dat) {
  a <-aov(value ~ site, data = dat)
  h <-HSD.test(a,"site")
  #create a tibble with one column for each treatment
  #the hsd results are row1 = drought, row2 = saturation, row3 = time zero saturation, row4 = field moist. hsd letters are in column 2
  tibble(`SR` = h$groups["SR",2], 
         `CPCRW` = h$groups["CPCRW",2],
         `DWP` = h$groups["DWP",2])
}

soil_character2_long %>% 
  group_by(variable) %>% 
  do(fit_hsd(.))  ->
  soil_charac_hsd

soil_charac_hsd %>% 
  gather(site, hsd, 2:4)-> #gather columns 4-7 (treatment levels)
  soil_charac_hsd2

# merge `summary` and `hsd`
soil_character_summary2 = merge(soil_character_summary, soil_charac_hsd2, by = c("site", "variable"))

soil_character_summary2 %>% 
  mutate(summary_hsd = paste(summary," ",hsd)) %>% 
  dplyr::select(-hsd)->
  soil_character_summary2

### OUTPUT
write.csv(soil_character_summary2, CHARACTERIZATION, row.names = FALSE)

##

# 2. pore size ----
pores = read_excel("data/pore_size.xlsx")
names(pores)

#creating frequency distribution tables for each site
cpcrw_pore_scores = pores$CPCRW
dwp_pore_scores = pores$DWP
sr_pore_scores = pores$SR
bins = seq(0,4000, by = 100)

cpcrw_scores = cut(cpcrw_pore_scores,bins)
dwp_scores = cut(dwp_pore_scores,bins)
sr_scores = cut(sr_pore_scores,bins)

freq_cpcrw = transform(table(cpcrw_scores))
c = transform(freq_cpcrw,Cum_Freq=cumsum(Freq),Perc_Freq = prop.table(Freq)*100)
c$scores = seq(0,3999,by = 100)

freq_dwp = transform(table(dwp_scores))
d = transform(freq_dwp,Cum_Freq=cumsum(Freq),Perc_Freq = prop.table(Freq)*100)
d$scores = seq(0,3999,by = 100)

freq_sr = transform(table(sr_scores))
s = transform(freq_sr,Cum_Freq=cumsum(Freq),Perc_Freq = prop.table(Freq)*100)
s$scores = seq(0,3999,by = 100)

#combining the three sites, two at a time
#the file contains frequency, cum freq, and % frequency
combined_pore_freq = merge(c,d,by = "scores")
combined_pore_freq = merge(combined_pore_freq,s,by = "scores")

names(combined_pore_freq)

# extracting only the % frequency columns and saving as a new file
combined_pore_perc_freq = data.frame(combined_pore_freq$scores,
                                     combined_pore_freq$Perc_Freq.x,
                                     combined_pore_freq$Perc_Freq.y,
                                     combined_pore_freq$Perc_Freq)
names(combined_pore_perc_freq) = c("pore_size","cpcrw","dwp","sr")

#melting the three sites into a single column
pores_melt = melt(combined_pore_perc_freq,id = "pore_size")
names(pores_melt) = c("pore_size","site","freq")

###OUTPUT
write.csv(pores_melt,PORE_DISTRIBUTION, row.names = FALSE)


##
##