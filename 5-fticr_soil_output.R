### FTICR-soil output
### this script will create summary tables and graphs for the FTICR data.
### use the subset files created in scripts 3&4 as inputs here. Do not use the original data because those files are huge.

### Kaizad F. Patel
### September 2019

source("0-packages.R")

## do NOT source scripts 3 & 4.

## 1: aromatic peaks - summary ----
fticr_soil_aromatic = read_csv("fticr/fticr_soil_aromatic.csv")

ggplot(fticr_soil_aromatic[fticr_soil_aromatic$aromatic=="aromatic",], aes(x = site, y = arom_core_counts, color = treatment, fill = treatment))+
  #geom_bar(stat="summary",width=0.1,position=position_dodge(0.7),size=1)+
  #geom_errorbar(aes(ymin=`arom_core_counts`-se, ymax=`arom_core_counts`+se),width=0.3,position=position_dodge(0.7),color="black",size=1)+
  geom_boxplot(fill = "white")
 # geom_point()


#
## 2: NOSC plots ----
fticr_soil_nosc = read_csv("fticr/fticr_soil_nosc.csv")

ggplot(fticr_soil_nosc[fticr_soil_nosc$site=="CPCRW",], aes(x = NOSC, fill = treatment))+
  geom_histogram(binwidth = 0.25, color = "black")+
  xlim(-2.5, 2.5)+
  ggtitle("CPCRW")

ggplot(fticr_soil_nosc[fticr_soil_nosc$site=="DWP",], aes(x = NOSC, fill = treatment))+
  geom_histogram(binwidth = 0.25, color = "black")+
  ggtitle("DWP")

ggplot(fticr_soil_nosc[fticr_soil_nosc$site=="SR",], aes(x = NOSC, fill = treatment))+
  geom_histogram(binwidth = 0.25, color = "black")+
  ggtitle("SR")

#

## 3: plot kendrick ----

fticr_soil_kendrick = read_csv("fticr/fticr_soil_kendrick.csv")

## plotting kmd with only three treatments

ggplot(fticr_soil_kendrick[fticr_soil_kendrick$site=="CPCRW" & 
                             fticr_soil_kendrick$treatment=="drought" | fticr_soil_kendrick$treatment=="saturation"|fticr_soil_kendrick$treatment=="field moist",], 
       aes(x = kmass, y = kdefect, color = treatment))+
  geom_point(size=0.5)+
  ggtitle("CPCRW")

ggplot(fticr_soil_kendrick[fticr_soil_kendrick$site=="DWP"& 
                             fticr_soil_kendrick$treatment=="drought" | fticr_soil_kendrick$treatment=="saturation"|fticr_soil_kendrick$treatment=="field moist",], 
       aes(x = kmass, y = kdefect, color = treatment))+
  geom_point(size=0.5)+
  ggtitle("DWP")

ggplot(fticr_soil_kendrick[fticr_soil_kendrick$site=="SR" & 
                             fticr_soil_kendrick$treatment=="drought" | fticr_soil_kendrick$treatment=="saturation"|fticr_soil_kendrick$treatment=="field moist",], 
       aes(x = kmass, y = kdefect, color = treatment))+
  geom_point(size=0.5)+
  ggtitle("SR")
#




## 4: van krevelen plots ----

#
