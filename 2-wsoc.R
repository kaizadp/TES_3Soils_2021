# 3Soils 2019
# This script reads WSOC data files for (a) soil extracts and (b) pore water samples, 
# creates summary tables and graphs
# Kaizad F. Patel, Aug 2019

source("0-packages.R")


## WSOC concentrations -- pores ####
wsoc_pores = read_excel("data/3Soils_WSOC_CN_PoreCore.xlsx")
names(wsoc_pores)

wsoc_pores$wsoc = wsoc_pores$`Water Soluble Organic Carbon (mg/L)`

#rename and reorder factors in Treatment 
wsoc_pores$Treatment = factor(wsoc_pores$Treatment,
                              levels = c("Time Zero Saturation",
                                         "Field Moisture Incubation",
                                         "Saturation Incubation",
                                         "Drought Incubation"),
                              labels = c("Time Zero",
                                         "Field Moist",
                                         "Saturated",
                                         "Drought"))

#creating summary
wsoc_rmisc_cpcrw=summarySE(wsoc_pores[wsoc_pores$Site=="CPCRW",],measurevar = "wsoc", groupvars=c("Site","Suction","Treatment"),na.rm=TRUE)
wsoc_rmisc_dwp=summarySE(wsoc_pores[wsoc_pores$Site=="DWP",],measurevar = "wsoc", groupvars=c("Site","Suction","Treatment"),na.rm=TRUE)
wsoc_rmisc_sr=summarySE(wsoc_pores[wsoc_pores$Site=="SR",],measurevar = "wsoc", groupvars=c("Site","Suction","Treatment"),na.rm=TRUE)

wsoc_cpcrw = wsoc_pores[wsoc_pores$Site=="CPCRW",]
wsoc_dwp = wsoc_pores[wsoc_pores$Site=="DWP",]
wsoc_sr = wsoc_pores[wsoc_pores$Site=="SR",]

#plot

gg_wsoc_pores_cpcrw = ggplot(wsoc_rmisc_cpcrw, 
                       aes(x = Treatment, y = wsoc,color = Suction,fill=Suction))+
  geom_bar(stat="summary",width=0.5,position=position_dodge(0.6),color="black",size=1)+
  geom_errorbar(aes(ymin=`wsoc`-sd, ymax=`wsoc`+sd),width=0.2,position=position_dodge(0.6),color="black",size=1)+
  geom_point(data = wsoc_cpcrw,aes(x = Treatment, y = wsoc),color = "black",position = position_dodge(0.6))+
  
  labs (y = expression (bold ("WSOC, mg L"^-1),
                        x = expression (bold (""))))+
  xlab("")+
  ylim(0,350)+
  
  annotate("text", label = "B", x = 0.85, y = 70 ,size=4,fontface="bold")+ 
  annotate("text", label = "B", x = 1.15, y = 70 ,size=4,fontface="bold")+ 
  annotate("text", label = "B", x = 1.85, y = 80 ,size=4,fontface="bold")+ 
  annotate("text", label = "B", x = 2.15, y = 80 ,size=4,fontface="bold")+ 
  annotate("text", label = "B", x = 2.85, y = 120 ,size=4,fontface="bold")+ 
  annotate("text", label = "AB", x = 3.15, y = 180 ,size=4,fontface="bold")+ 
  annotate("text", label = "AB", x = 3.85, y = 300 ,size=4,fontface="bold")+ 
  annotate("text", label = "A", x = 4.15, y = 345 ,size=4,fontface="bold")+ 
  
  theme_bw()+
  theme(panel.border=element_rect(color="black",size=1.5))+
  theme (legend.position = c(0.2,0.8))+
  theme (legend.key = element_rect(size = 3))+
  theme (legend.title = element_blank())+
  theme (legend.text=element_text(size=12))+
  theme (legend.key = element_rect(size = 2),
         legend.key.size = unit(2, 'lines'))+
  ggtitle ("CPCRW")+
  theme (plot.title = element_text(hjust = 0.05,size = 14))+
  theme (axis.text=element_text(size=14,face="bold",color="black"),
         axis.title=element_text(size=14,face="bold",color="black")); gg_wsoc_pores_cpcrw
save_plot("output/wsoc_cpcrw.tiff", gg_wsoc_pores_cpcrw, base_height = 10, base_width = 10)

gg_wsoc_pores_dwp = ggplot(wsoc_rmisc_dwp, 
                     aes(x = Treatment, y = wsoc,color = Suction,fill=Suction))+
  geom_bar(stat="summary",width=0.5,position=position_dodge(0.6),color="black",size=1)+
  geom_errorbar(aes(ymin=`wsoc`-sd, ymax=`wsoc`+sd),width=0.2,position=position_dodge(0.6),color="black",size=1)+
  geom_point(data = wsoc_dwp,aes(x = Treatment, y = wsoc),color = "black",position = position_dodge(0.6))+
  
  
  labs (y = expression (bold ("WSOC, mg L"^-1),
                        x = expression (bold (" "))))+
  xlab("")+
  ylim(0,350)+
  
  annotate("text", label = "A", x = 0.85, y = 170 ,size=4,fontface="bold")+ 
  annotate("text", label = "A", x = 1.15, y = 170 ,size=4,fontface="bold")+ 
  annotate("text", label = "A", x = 1.85, y = 100 ,size=4,fontface="bold")+ 
  annotate("text", label = "A", x = 2.15, y = 100 ,size=4,fontface="bold")+ 
  annotate("text", label = "A", x = 2.85, y = 80 ,size=4,fontface="bold")+ 
  annotate("text", label = "A", x = 3.15, y = 170 ,size=4,fontface="bold")+ 
  annotate("text", label = "A", x = 3.85, y = 150 ,size=4,fontface="bold")+ 
  annotate("text", label = "A", x = 4.15, y = 150 ,size=4,fontface="bold")+ 
  
  theme_bw()+
  theme(panel.border=element_rect(color="black",size=1.5))+
  theme (legend.position = "none")+
  theme (legend.key = element_rect(size = 3))+
  theme (legend.title = element_blank())+
  theme (legend.text=element_text(size=12))+
  theme (legend.key = element_rect(size = 2),
         legend.key.size = unit(2, 'lines'))+
  ggtitle ("DWP")+
  theme (plot.title = element_text(hjust = 0.05,size = 14))+
  theme (axis.text=element_text(size=14,face="bold",color="black"),
         axis.title=element_text(size=14,face="bold",color="black")); gg_wsoc_pores_dwp
save_plot("wsoc_dwp.tiff", gg_wsoc_pores_dwp, base_height = 10, base_width = 10)


gg_wsoc_pores_sr = ggplot(wsoc_rmisc_sr, 
                    aes(x = Treatment, y = wsoc,color = Suction,fill=Suction))+
  geom_bar(stat="summary",width=0.5,position=position_dodge(0.6),color="black",size=1)+
  geom_errorbar(aes(ymin=`wsoc`-sd, ymax=`wsoc`+sd),width=0.2,position=position_dodge(0.6),color="black",size=1)+
  geom_point(data = wsoc_sr,aes(x = Treatment, y = wsoc),color = "black",position = position_dodge(0.6))+
  
  labs (y = expression (bold ("WSOC, mg L"^-1),
                        x = expression (bold (" "))))+
  xlab("")+
  ylim(0,350)+
  
  annotate("text", label = "B", x = 0.85, y = 20 ,size=4,fontface="bold")+ 
  annotate("text", label = "B", x = 1.15, y = 20 ,size=4,fontface="bold")+ 
  annotate("text", label = "AB", x = 1.85, y = 50 ,size=4,fontface="bold")+ 
  annotate("text", label = "AB", x = 2.15, y = 50 ,size=4,fontface="bold")+ 
  annotate("text", label = "A", x = 2.85, y = 50 ,size=4,fontface="bold")+ 
  annotate("text", label = "A", x = 3.15, y = 50 ,size=4,fontface="bold")+ 
  annotate("text", label = "AB", x = 3.85, y = 50 ,size=4,fontface="bold")+ 
  annotate("text", label = "AB", x = 4.15, y = 50 ,size=4,fontface="bold")+ 
  
  theme_bw()+
  theme(panel.border=element_rect(color="black",size=1.5))+
  theme (legend.position = "none")+
  theme (legend.key = element_rect(size = 3))+
  theme (legend.title = element_blank())+
  theme (legend.text=element_text(size=12))+
  theme (legend.key = element_rect(size = 1),
         legend.key.size = unit(1.5, 'lines'))+
  ggtitle ("SR")+
  theme (plot.title = element_text(hjust = 0.05,size = 14))+
  theme (axis.text=element_text(size=14,face="bold",color="black"),
         axis.title=element_text(size=14,face="bold",color="black")); gg_wsoc_pores_sr
save_plot("wsoc_sr.tiff", gg_wsoc_pores_sr,base_height = 10, base_width = 10)

porewater = plot_grid(gg_wsoc_pores_cpcrw,gg_wsoc_pores_dwp,gg_wsoc_pores_sr,nrow=2,ncol=2,align="hv", axis = "bt");porewater
save_plot("output/wsoc_pores.tiff",porewater, base_width =12, base_height = 12)


plot_grid(gg_wsoc_pores_cpcrw,gg_wsoc_pores_dwp, gg_wsoc_pores_sr,
          ncol=2,nrow=2,align="hv")

##


#
### WSOC concentrations -- pores -- stats ----

#remove NA
wsoc_pores = wsoc_pores[complete.cases(wsoc_pores),]

#overall LME site/treatment/suction
lme_trt_pore=lme(log(wsoc)~Site*Treatment*Suction,random=~1|CoreNo,data=wsoc_pores);Anova(lme_trt_pore)
capture.output(Anova(lme_trt_pore),file = "pore_wsoc_anova.txt")
attach(wsoc_pores)
wsoc_pore_hsd = HSD.test(aov(lme_trt_pore),trt = "Treatment")

# HSD for CPCRW
int_c = with(wsoc_pores[Site=="CPCRW",],interaction(Treatment,Suction))
amod_c = aov(log(wsoc)~int_c,data = wsoc_pores[Site=="CPCRW",])
wsoc_pore_hsd_c = HSD.test(amod_c,"int_c",group = TRUE)

# HSD for DWP
int_d = with(wsoc_pores[Site=="DWP",],interaction(Treatment,Suction))
amod_d = aov(wsoc~int_d,data = wsoc_pores[Site=="DWP",])
wsoc_pore_hsd_d = HSD.test(amod_d,"int_d",group = TRUE)

# HSD for SR
int_s = with(wsoc_pores[Site=="SR",],interaction(Treatment,Suction))
amod_s = aov(log(wsoc)~int_s,data = wsoc_pores[Site=="SR",])
wsoc_pore_hsd_s = HSD.test(amod_s,"int_s",group = TRUE)

# HSD for CPCRW_50 and CPCRW_1.5
int_c_50 = with(wsoc_pores[Site=="CPCRW"&Suction=="50 kPa",],interaction(Treatment,Suction))
amod_c_50 = aov(wsoc~Treatment,data = wsoc_pores[Site=="CPCRW"&Suction=="50 kPa",])
wsoc_pore_hsd_c_50 = HSD.test(amod_c_50,"Treatment",group = TRUE)

int_c_1.5 = with(wsoc_pores[Site=="CPCRW"&Suction=="1.5 kPa",],interaction(Treatment,Suction))
amod_c_1.5 = aov(wsoc~Treatment,data = wsoc_pores[Site=="CPCRW"&Suction=="1.5 kPa",])
wsoc_pore_hsd_c_1.5 = HSD.test(amod_c_1.5,"Treatment",group = TRUE)




### WSOC concentrations -- pores -- summary table ----
wsoc_rmisc=summarySE(wsoc_pores,measurevar = "wsoc", groupvars=c("Site","Suction","Treatment"),na.rm=TRUE)
wsoc_rmisc$wsoc_mg_L = paste(round(wsoc_rmisc$wsoc,2),"\u00B1",round(wsoc_rmisc$se,2))
#\u00b1 is plus-minus
wsoc_pore_summary = dcast(wsoc_rmisc,Treatment~Site+Suction,value.var = "wsoc_mg_L") 
write.csv(wsoc_pore_summary, file="wsoc_pores_summary.csv")


##
#

## WSOC concentrations -- soils ---- ----
wsoc_soils = read_excel("data/3Soils_WSOC_CN_PoreCore_soils.xlsx")
names(wsoc_soils)
wsoc_soils$wsoc_mg_g = wsoc_soils$`WSOC mgCg-1soil`
wsoc_soils$Treatment = factor(wsoc_soils$Treatment,
                              levels = c("Time Zero Saturation",
                                         "Field Moisture Incubation",
                                         "Saturation Incubation",
                                         "Drought Incubation"),
                              labels = c("Time Zero",
                                         "Field Moist",
                                         "Saturated",
                                         "Drought"))

wsoc_soils_rmisc = summarySE(wsoc_soils,measurevar = "wsoc_mg_g",groupvars = c("Site","Treatment"),na.rm = TRUE)
wsoc_soils_rmisc = wsoc_soils_rmisc[complete.cases(wsoc_soils_rmisc),]
wsoc_soils_rmisc$WSOC_mg_g = paste(round(wsoc_soils_rmisc$wsoc_mg_g,2),"\u00B1",round(wsoc_soils_rmisc$se,2))
#\u00b1 is plus-minus

wsoc_soils_summary = dcast(wsoc_soils_rmisc,Treatment~Site,value.var = "WSOC_mg_g") 
write.csv(wsoc_soils_summary, file="wsoc_soils_summary.csv")
#
#
### WSOC concentrations -- soils -- stats ----
wsoc_soils = wsoc_soils[complete.cases(wsoc_soils),]

wsoc_soils_lme = lme(wsoc_mg_g ~ Treatment*Site, random = ~1|CoreNo,data = wsoc_soils)
Anova(wsoc_soils_lme, data = wsoc_soils)
capture.output(Anova(wsoc_soils_lme),file = "wsoc_soils_Anova.txt")

wsoc_soils_aov = aov(wsoc_soils_lme, data = wsoc_soils)
wsoc_soils_hsd_trt = HSD.test(wsoc_soils_aov,trt = "Site")

wsoc_soils_aov_c = aov(wsoc_mg_g~Treatment,data = wsoc_soils[wsoc_soils$Site=="CPCRW",])
wsoc_soils_hsd_c = HSD.test(wsoc_soils_aov_c,trt="Treatment"); wsoc_soils_hsd_c

wsoc_soils_aov_d = aov(wsoc_mg_g~Treatment,data = wsoc_soils[wsoc_soils$Site=="DWP",])
wsoc_soils_hsd_d = HSD.test(wsoc_soils_aov_d,trt="Treatment"); wsoc_soils_hsd_d

wsoc_soils_aov_s = aov(wsoc_mg_g~Treatment,data = wsoc_soils[wsoc_soils$Site=="SR",])
wsoc_soils_hsd_s = HSD.test(wsoc_soils_aov_s,trt="Treatment"); wsoc_soils_hsd_s

#

#