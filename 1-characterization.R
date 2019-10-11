# 3Soils 2019
# This script reads data files for soil characterization parameters, 
# creates summary tables and graphs
# Kaizad F. Patel, Aug 2019

source("0-packages.R")


## 1. nutrients and soil characterization ----

### input files

soil_character = read_excel("data/soil_character.xlsx")
names(soil_character)

#converting variables to numeric form
soil_character$K_meq100g = soil_character$`K_ppm`/390
soil_character$`NO3-N_ppm` = as.numeric(soil_character$`NO3-N_ppm`)
soil_character$Ca_meq100g = as.numeric(soil_character$Ca_meq100g)
soil_character$Mg_meq100g = as.numeric(soil_character$Mg_meq100g)
soil_character$`pH` = as.numeric(soil_character$`pH`)
soil_character$EC_dS_m = as.numeric(soil_character$EC_dS_m)
soil_character$`Sand_perc` = as.numeric(soil_character$`Sand_perc`)
soil_character$`Silt_perc` = as.numeric(soil_character$`Silt_perc`)
soil_character$`Clay_perc` = as.numeric(soil_character$`Clay_perc`)


# creating summary table----
# option 1 don't do ----
soil_char2 = 
  dplyr::mutate(soil_character,
                soil_factor = factor(soil_character$Soil,
                                     levels = c("CPCRW","DWP","SR")))
str(soil_char2)

soil_char_summary = 
  list ("C" = list("TC, %" = ~qwraps2::mean_sd(TC_perc)),
        "N" = list("TN, %" = ~qwraps2::mean_sd(TN_perc)),
        "TOC" = list("TOC, %" = ~qwraps2::mean_sd(TOC_perc)),
        "WSOC" = list("WSOC, mgC/g" = ~qwraps2::mean_sd(WSOC_mg_g)),
        "Ca"= list("Ca, meq/100g" = ~qwraps2::mean_sd(Ca_meq100g)),
        "Mg" = list("Mg, meq/100g" = ~qwraps2::mean_sd(Mg_meq100g)),
        "pH" = list("pH" = ~qwraps2::mean_sd(pH)),
        "EC" = list("EC, dS/m" = ~qwraps2::mean_sd(EC_dS_m)),
        "sand" = list("sand, %" = ~qwraps2::mean_sd(Sand_perc)),
        "silt" = list("silt, %" = ~qwraps2::mean_sd(Silt_perc)),
        "clay" = list("clay, %" = ~qwraps2::mean_sd(Clay_perc))
  )

### need to omit NA for SR. HOW??

soil_char_table = summary_table(dplyr::group_by(soil_char2,Soil),soil_char_summary)

# print(soil_char_table,
#      cnames = c("CPCRW","DWP","SR"))
# write.csv(soil_char_table, file = "soil_charac_summary.csv")

##

# option 2 do this----

# select only those rows we want for the characterization summary
# manual step

soil_character %>% 
  dplyr::rename(site=Soil) %>% 
  select(site,
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

## 2. characterization -- stats ----

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
  select(-hsd)->
  soil_character_summary2

### OUTPUT
write.csv(soil_character_summary2, CHARACTERIZATION)

##

## 3. pore size ----
pores = read_excel("data/pore_size.xlsx")
names(pores)

#creating frequency distribution tables for each site
cpcrw_pore_scores = pores$CPCRW
dwp_pore_scores = pores$DWP
sr_pore_scores = pores$SR
bins = seq(0,4000, by = 50)

cpcrw_scores = cut(cpcrw_pore_scores,bins)
dwp_scores = cut(dwp_pore_scores,bins)
sr_scores = cut(sr_pore_scores,bins)

freq_cpcrw = transform(table(cpcrw_scores))
c = transform(freq_cpcrw,Cum_Freq=cumsum(Freq),Perc_Freq = prop.table(Freq)*100)
c$scores = seq(50,4000,by = 50)

freq_dwp = transform(table(dwp_scores))
d = transform(freq_dwp,Cum_Freq=cumsum(Freq),Perc_Freq = prop.table(Freq)*100)
d$scores = seq(50,4000,by = 50)

freq_sr = transform(table(sr_scores))
s = transform(freq_sr,Cum_Freq=cumsum(Freq),Perc_Freq = prop.table(Freq)*100)
s$scores = seq(50,4000,by = 50)

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
write.csv(combined_pore_perc_freq,"processed/pore_size_perc_freq2.csv")

#melting the three sites into a single column
pores_melt = melt(combined_pore_perc_freq,id = "pore_size")
names(pores_melt) = c("pore_size","site","freq")

###OUTPUT
write.csv(pores_melt,PORE_DISTRIBUTION)


#plotting the frequency distribution -- NOT DOING THIS ANY MORE. MAKING FIGURES IN MARKDOWN
gg_pore_distrib = ggplot(pores_melt,aes(x = pore_size, y=freq,color = site))+
  geom_line(size = 1)+
  #geom_density(adjust=0.5)+
  
  labs (y = expression (bold ("distribution, %"),
                        x = expression (bold ("pore size" ))))+
  theme_bw()+
  theme (legend.position = c(0.7,0.7))+
  theme (legend.key = element_rect(size = 3))+
  theme (legend.title = element_blank())+
  theme (legend.text=element_text(size=12))+
  theme (legend.key = element_rect(size = 2),
         legend.key.size = unit(2, 'lines'))+
  ggtitle ("pore size distribution")+
  theme (plot.title = element_text(hjust = 0.05,size = 14))+
  theme (axis.text=element_text(size=14,face="bold",color="black"),
         axis.title=element_text(size=14,face="bold",color="black")); gg_pore_distrib
#save_plot("poresizedist2.tiff", gg_pore_distrib, base_aspect_ratio = 2)

##


## 4. water retention curves ----
water_retention = read_excel("data/water_retention.xlsx")
names(water_retention)
attach(water_retention)

# van genuchten plots
gg_water_retention_vg = ggplot()+
  geom_line(data = water_retention,aes(x = Tension_c, y = CPCRW_water_VG, color = "CPCRW"), size=1)+
  geom_line(data = water_retention,aes(x = Tension_d, y = DWP_water_VG,color = "DWP"), size=1)+
  geom_line(data = water_retention,aes(x = Tension_s, y = SR_water_VG,color = "SR"), size=1)+
  
  # geom_point(data = water_retention,aes(x = Tension_c, y = CPCRW_water_measured))+
  # geom_point(data = water_retention,aes(x = Tension_d, y = DWP_water_measured))+
  # geom_point(data = water_retention,aes(x = Tension_s, y = SR_water_measured))
  
  annotate("text", label = "DWP, fine sand", x = 25, y = 0.03,size=4,fontface="bold")+ 
  annotate("text", label = "CPCRW, silt loam", x = 75, y = 0.13,size=4,fontface="bold")+ 
  annotate("text", label = "SR, silty clay loam", x = 55, y = 0.3,size=4,fontface="bold")+ 
  
  labs (y = expression (bold ("volumetric moisture content"),
                        x = expression (bold ("tension, kPa" ))))+
  ylim(0,0.7)+
  theme_bw()+
  theme (legend.position = "none")+
  theme (legend.key = element_rect(size = 3))+
  theme (legend.title = element_blank())+
  theme (legend.text=element_text(size=12))+
  theme (legend.key = element_rect(size = 2),
         legend.key.size = unit(2, 'lines'))+
  #  ggtitle ("water retention curves: Van Genuchten")+
  theme (plot.title = element_text(hjust = 0.05,size = 14))+
  theme (axis.text=element_text(size=14,face="bold",color="black"),
         axis.title=element_text(size=14,face="bold",color="black")); gg_water_retention_vg

save_plot("water_retention_vangenuch.tiff", gg_water_retention_vg, base_width = 5, base_height = 4)

# retc curves
gg_water_retention_retc = ggplot()+
 
   geom_line(data = water_retention,aes(x = Tension_c, y = CPCRW_water_RETC, color = "CPCRW"))+
   geom_line(data = water_retention,aes(x = Tension_d, y = DWP_water_RETC,color = "DWP"))+
   geom_line(data = water_retention,aes(x = Tension_s, y = SR_water_RETC,color = "SR"))+
  
  # geom_point(data = water_retention,aes(x = Tension_c, y = CPCRW_water_measured))+
  # geom_point(data = water_retention,aes(x = Tension_d, y = DWP_water_measured))+
  # geom_point(data = water_retention,aes(x = Tension_s, y = SR_water_measured))
  
  annotate("text", label = "DWP, fine sand", x = 25, y = 0.03,size=4,fontface="bold")+ 
  annotate("text", label = "CPCRW, silt loam", x = 75, y = 0.13,size=4,fontface="bold")+ 
  annotate("text", label = "SR, silty clay loam", x = 55, y = 0.3,size=4,fontface="bold")+ 
  
  labs (y = expression (bold ("volumetric moisture content"),
                        x = expression (bold ("tension, kPa" ))))+
  ylim(0,0.7)+
  theme_bw()+
  theme (legend.position = "none")+
  theme (legend.key = element_rect(size = 3))+
  theme (legend.title = element_blank())+
  theme (legend.text=element_text(size=12))+
  theme (legend.key = element_rect(size = 2),
         legend.key.size = unit(2, 'lines'))+
  #  ggtitle ("water retention curves: Van Genuchten")+
  theme (plot.title = element_text(hjust = 0.05,size = 14))+
  theme (axis.text=element_text(size=14,face="bold",color="black"),
         axis.title=element_text(size=14,face="bold",color="black")); gg_water_retention_retc

save_plot("water_retention_retc.tiff", gg_water_retention_retc, base_width = 5, base_height = 4)

##
##