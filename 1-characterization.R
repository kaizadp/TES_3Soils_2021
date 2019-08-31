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


# creating summary table
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

print(soil_char_table,
      cnames = c("CPCRW","DWP","SR"))
write.csv(soil_char_table, file = "soil_charac_summary.csv")

##

## 2. characterization -- stats ----

### test for normaility


### anova and hsd

a_tc = aov(TC_perc~Soil,data = soil_character)
hsd_tc = HSD.test(a_tc,trt = "Soil")

a_tn = aov(TN_perc~Soil,data = soil_character)
hsd_tn = HSD.test(a_tn,trt = "Soil")
hsd_tn

a_toc = aov(TOC_perc~Soil,data = soil_character)
hsd_toc = HSD.test(a_toc,trt = "Soil")
hsd_toc

a_wsoc = aov(WSOC_mg_g~Soil,data = soil_character)
hsd_wsoc = HSD.test(a_wsoc,trt = "Soil")
hsd_wsoc

a_ca = aov(Ca_meq100g~Soil,data = soil_character)
hsd_ca = HSD.test(a_ca,trt = "Soil")
hsd_ca

a_mg = aov(Mg_meq100g~Soil,data = soil_character)
hsd_mg = HSD.test(a_mg,trt = "Soil")
hsd_mg

a_pH = aov(pH~Soil,data = soil_character)
hsd_pH = HSD.test(a_pH,trt = "Soil")
hsd_pH

a_ec = aov(EC_dS_m~Soil,data = soil_character)
hsd_ec = HSD.test(a_ec,trt = "Soil")
hsd_ec

a_sand = aov(Sand_perc~Soil,data = soil_character)
hsd_sand = HSD.test(a_sand,trt = "Soil")
hsd_sand

a_silt = aov(Silt_perc~Soil,data = soil_character)
hsd_silt = HSD.test(a_silt,trt = "Soil")
hsd_silt

a_clay = aov(Clay_perc~Soil,data = soil_character)
hsd_clay = HSD.test(a_clay,trt = "Soil")
hsd_clay

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
write.csv(combined_pore_perc_freq,"pore_size_perc_freq2.csv")

#melting the three sites into a single column
pores_melt = melt(combined_pore_perc_freq,id = "pore_size")
names(pores_melt) = c("pore_size","site","freq")

#plotting the frequency distribution
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
save_plot("poresizedist2.tiff", gg_pore_distrib, base_aspect_ratio = 2)

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