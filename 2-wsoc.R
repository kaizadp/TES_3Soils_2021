# 3Soils 2019
# This script reads WSOC data files for (a) soil extracts and (b) pore water samples, 
# creates summary tables and graphs
# Kaizad F. Patel, Aug 2019

source("0-packages.R")

#
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
#
## making separate plots for sites DON'T DO THIS ----
      ## THIS IS CRAZY. USE FACETS INSTEADDON'
      # #creating summary
      # wsoc_rmisc_cpcrw=summarySE(wsoc_pores[wsoc_pores$Site=="CPCRW",],measurevar = "wsoc", groupvars=c("Site","Suction","Treatment"),na.rm=TRUE)
      # wsoc_rmisc_dwp=summarySE(wsoc_pores[wsoc_pores$Site=="DWP",],measurevar = "wsoc", groupvars=c("Site","Suction","Treatment"),na.rm=TRUE)
      # wsoc_rmisc_sr=summarySE(wsoc_pores[wsoc_pores$Site=="SR",],measurevar = "wsoc", groupvars=c("Site","Suction","Treatment"),na.rm=TRUE)
      # 
      # wsoc_cpcrw = wsoc_pores[wsoc_pores$Site=="CPCRW",]
      # wsoc_dwp = wsoc_pores[wsoc_pores$Site=="DWP",]
      # wsoc_sr = wsoc_pores[wsoc_pores$Site=="SR",]
      # 
      # #plot
      # 
      # gg_wsoc_pores_cpcrw = ggplot(wsoc_rmisc_cpcrw, 
      #                              aes(x = Treatment, y = wsoc,color = Suction,fill=Suction))+
      #   geom_bar(stat="summary",width=0.5,position=position_dodge(0.6),color="black",size=1)+
      #   geom_errorbar(aes(ymin=`wsoc`-sd, ymax=`wsoc`+sd),width=0.2,position=position_dodge(0.6),color="black",size=1)+
      #   geom_point(data = wsoc_cpcrw,aes(x = Treatment, y = wsoc),color = "black",position = position_dodge(0.6))+
      #   
      #   labs (y = expression (bold ("WSOC, mg L"^-1),
      #                         x = expression (bold (""))))+
      #   xlab("")+
      #   ylim(0,350)+
      #   
      #   annotate("text", label = "B", x = 0.85, y = 70 ,size=4,fontface="bold")+ 
      #   annotate("text", label = "B", x = 1.15, y = 70 ,size=4,fontface="bold")+ 
      #   annotate("text", label = "B", x = 1.85, y = 80 ,size=4,fontface="bold")+ 
      #   annotate("text", label = "B", x = 2.15, y = 80 ,size=4,fontface="bold")+ 
      #   annotate("text", label = "B", x = 2.85, y = 120 ,size=4,fontface="bold")+ 
      #   annotate("text", label = "AB", x = 3.15, y = 180 ,size=4,fontface="bold")+ 
      #   annotate("text", label = "AB", x = 3.85, y = 300 ,size=4,fontface="bold")+ 
      #   annotate("text", label = "A", x = 4.15, y = 345 ,size=4,fontface="bold")+ 
      #   
      #   theme_kp() + 
      #   theme (legend.position = c(0.2, 0.8),
      #          legend.key = element_rect(size = 2),
      #          legend.key.size = unit(2, 'lines')) +
      #   ggtitle ("CPCRW")
      # 
      # print(gg_wsoc_pores_cpcrw)
      # save_plot("output/wsoc_cpcrw.tiff", gg_wsoc_pores_cpcrw, base_height = 10, base_width = 10)
      # 
      # gg_wsoc_pores_dwp = ggplot(wsoc_rmisc_dwp, 
      #                            aes(x = Treatment, y = wsoc,color = Suction,fill=Suction))+
      #   geom_bar(stat="summary",width=0.5,position=position_dodge(0.6),color="black",size=1)+
      #   geom_errorbar(aes(ymin=`wsoc`-sd, ymax=`wsoc`+sd),width=0.2,position=position_dodge(0.6),color="black",size=1)+
      #   geom_point(data = wsoc_dwp,aes(x = Treatment, y = wsoc),color = "black",position = position_dodge(0.6))+
      #   
      #   
      #   labs (y = expression (bold ("WSOC, mg L"^-1),
      #                         x = expression (bold (" "))))+
      #   xlab("")+
      #   ylim(0,350)+
      #   
      #   annotate("text", label = "A", x = 0.85, y = 170 ,size=4,fontface="bold")+ 
      #   annotate("text", label = "A", x = 1.15, y = 170 ,size=4,fontface="bold")+ 
      #   annotate("text", label = "A", x = 1.85, y = 100 ,size=4,fontface="bold")+ 
      #   annotate("text", label = "A", x = 2.15, y = 100 ,size=4,fontface="bold")+ 
      #   annotate("text", label = "A", x = 2.85, y = 80 ,size=4,fontface="bold")+ 
      #   annotate("text", label = "A", x = 3.15, y = 170 ,size=4,fontface="bold")+ 
      #   annotate("text", label = "A", x = 3.85, y = 150 ,size=4,fontface="bold")+ 
      #   annotate("text", label = "A", x = 4.15, y = 150 ,size=4,fontface="bold")+ 
      #   
      #   theme_bw()+
      #   theme(panel.border=element_rect(color="black",size=1.5))+
      #   theme (legend.position = "none")+
      #   theme (legend.key = element_rect(size = 3))+
      #   theme (legend.title = element_blank())+
      #   theme (legend.text=element_text(size=12))+
      #   theme (legend.key = element_rect(size = 2),
      #          legend.key.size = unit(2, 'lines'))+
      #   ggtitle ("DWP")+
      #   theme (plot.title = element_text(hjust = 0.05,size = 14))+
      #   theme (axis.text=element_text(size=14,face="bold",color="black"),
      #          axis.title=element_text(size=14,face="bold",color="black")); gg_wsoc_pores_dwp
      # save_plot("wsoc_dwp.tiff", gg_wsoc_pores_dwp, base_height = 10, base_width = 10)
      # 
      # 
      # gg_wsoc_pores_sr = ggplot(wsoc_rmisc_sr, 
      #                           aes(x = Treatment, y = wsoc,color = Suction,fill=Suction))+
      #   geom_bar(stat="summary",width=0.5,position=position_dodge(0.6),color="black",size=1)+
      #   geom_errorbar(aes(ymin=`wsoc`-sd, ymax=`wsoc`+sd),width=0.2,position=position_dodge(0.6),color="black",size=1)+
      #   geom_point(data = wsoc_sr,aes(x = Treatment, y = wsoc),color = "black",position = position_dodge(0.6))+
      #   
      #   labs (y = expression (bold ("WSOC, mg L"^-1),
      #                         x = expression (bold (" "))))+
      #   xlab("")+
      #   ylim(0,350)+
      #   
      #   annotate("text", label = "B", x = 0.85, y = 20 ,size=4,fontface="bold")+ 
      #   annotate("text", label = "B", x = 1.15, y = 20 ,size=4,fontface="bold")+ 
      #   annotate("text", label = "AB", x = 1.85, y = 50 ,size=4,fontface="bold")+ 
      #   annotate("text", label = "AB", x = 2.15, y = 50 ,size=4,fontface="bold")+ 
      #   annotate("text", label = "A", x = 2.85, y = 50 ,size=4,fontface="bold")+ 
      #   annotate("text", label = "A", x = 3.15, y = 50 ,size=4,fontface="bold")+ 
      #   annotate("text", label = "AB", x = 3.85, y = 50 ,size=4,fontface="bold")+ 
      #   annotate("text", label = "AB", x = 4.15, y = 50 ,size=4,fontface="bold")+ 
      #   
      #   theme_bw()+
      #   theme(panel.border=element_rect(color="black",size=1.5))+
      #   theme (legend.position = "none")+
      #   theme (legend.key = element_rect(size = 3))+
      #   theme (legend.title = element_blank())+
      #   theme (legend.text=element_text(size=12))+
      #   theme (legend.key = element_rect(size = 1),
      #          legend.key.size = unit(1.5, 'lines'))+
      #   ggtitle ("SR")+
      #   theme (plot.title = element_text(hjust = 0.05,size = 14))+
      #   theme (axis.text=element_text(size=14,face="bold",color="black"),
      #          axis.title=element_text(size=14,face="bold",color="black")); gg_wsoc_pores_sr
      # save_plot("wsoc_sr.tiff", gg_wsoc_pores_sr,base_height = 10, base_width = 10)
      # 
      # porewater = plot_grid(gg_wsoc_pores_cpcrw,gg_wsoc_pores_dwp,gg_wsoc_pores_sr,nrow=2,ncol=2,align="hv", axis = "bt");porewater
      # save_plot("output/wsoc_pores.tiff",porewater, base_width =12, base_height = 12)
      # 
      # 
      # ##
## redoing plots with facet ----
# REMOVED SCRIPT, MOVED TO MARKDOWN

#
### WSOC concentrations -- pores -- stats ----

#remove NA
wsoc_pores = wsoc_pores[complete.cases(wsoc_pores),]

#overall LME site/treatment/suction
lme_trt_pore=lme(log(wsoc)~Site*Treatment*Suction,random=~1|CoreNo,data=wsoc_pores);Anova(lme_trt_pore)
#capture.output(Anova(lme_trt_pore),file = "pore_wsoc_anova.txt")
attach(wsoc_pores)
wsoc_pore_hsd = HSD.test(aov(lme_trt_pore),trt = "Treatment")

## HSD
fit_hsd_wsoc <- function(dat) {
  a <-aov(wsoc ~ Treatment, data = dat)
  h <-HSD.test(a,"Treatment")
  #create a tibble with one column for each treatment
  #the hsd results are row1 = drought, row2 = saturation, row3 = time zero saturation, row4 = field moist. hsd letters are in column 2
  tibble(`Drought` = h$groups["Drought",2], 
         `Saturated` = h$groups["Saturated",2],
         `Time Zero` = h$groups["Time Zero",2],
         `Field Moist` = h$groups["Field Moist",2])
}

wsoc_pores %>% 
  group_by(Site, Suction) %>% 
  do(fit_hsd_wsoc(.))  ->
  wsoc_pores_hsd

wsoc_pores_hsd %>% 
  gather(Treatment, hsd, 3:6)-> #gather columns 4-7 (treatment levels)
  wsoc_pores_hsd2

### WSOC concentrations -- pores -- summary table ----
wsoc_pores_rmisc=summarySE(wsoc_pores,measurevar = "wsoc", groupvars=c("Site","Suction","Treatment"),na.rm=TRUE)
wsoc_pores_rmisc$wsoc_mg_L = paste(round(wsoc_pores_rmisc$wsoc,2),"\u00B1",round(wsoc_pores_rmisc$se,2))
#\u00b1 is plus-minus


# merge the summary table with the hsd table
wsoc_pores_rmisc2 = merge(wsoc_pores_rmisc, wsoc_pores_hsd2, by = c("Site","Suction","Treatment"))

# combine the wsoc and hsd columns
wsoc_pores_rmisc2 %>% 
  mutate(wsoc_hsd = paste(wsoc_mg_L," ",hsd)) %>% 
  select(-sd,-se,-ci,-hsd)->
  wsoc_pores_summary

### OUTPUT
write.csv(wsoc_pores_summary, WSOC_PORE)


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

#hsd

fit_hsd_wsoc_soil <- function(dat) {
  a <-aov(wsoc_mg_g ~ Treatment, data = dat)
  h <-HSD.test(a,"Treatment")
  #create a tibble with one column for each treatment
  #the hsd results are row1 = drought, row2 = saturation, row3 = time zero saturation, row4 = field moist. hsd letters are in column 2
  tibble(`Drought` = h$groups["Drought",2], 
         `Saturated` = h$groups["Saturated",2],
         `Time Zero` = h$groups["Time Zero",2],
         `Field Moist` = h$groups["Field Moist",2])
}

wsoc_soils = wsoc_soils[complete.cases(wsoc_soils),]
wsoc_soils %>% 
  group_by(Site) %>% 
  do(fit_hsd_wsoc_soil(.))  ->
  wsoc_soils_hsd

wsoc_soils_hsd %>% 
  gather(Treatment, hsd, 2:5)-> #gather columns 4-7 (treatment levels)
  wsoc_soils_hsd2

# merge the summary table with the hsd table
wsoc_soils_rmisc2 = merge(wsoc_soils_rmisc, wsoc_soils_hsd2, by = c("Site","Treatment"))

# combine the wsoc and hsd columns
wsoc_soils_rmisc2 %>% 
  mutate(wsoc_hsd = paste(WSOC_mg_g," ",hsd)) %>% 
  select(-sd,-se,-ci,-hsd)->
  wsoc_soils_summary

###output
write.csv(wsoc_soils_summary, WSOC_SOIL)

#

#
## calculating porewater volume ----
porewater = read.csv("data/Porewater_weights.csv")
names(porewater)

# select only the relevant columns
# these columns are formatted HORRIBLY. redo
# split into two dataframes, one for empty weights and one for full weights. format and then combine

porewater %>% 
  dplyr::rename(Core=`Core.`) %>% 
  select(Site, Core, starts_with("X"))->
  porewater_subset

porewater_subset %>% 
# gather all into columns "type" and "weight"
  gather(type, weight,3:12) %>% 
# add columns for suction, empty/full, and vial number
# vial number is by default 1, and for "extra vials", 2
  dplyr::mutate(suction = case_when(grepl("15mb", type) ~ "1.5 kPa",
                             grepl("150mb", type) ~ "15 kPa",
                             grepl("500mb",type) ~ "50 kPa"),
         emptyfull = case_when(grepl("empty",type) ~ "empty",
                               grepl("full", type) ~ "full"),
         vial_num = if_else(grepl("_extra",type),"2","1")) %>%
# remove the column "type", because tat will f-up the next spread
  select(-type) %>% 
# spread, to get two separate columns for empty vs. full
  spread(emptyfull, weight)->
  porewatersubsetlong

## some cells in this file have multiple entries together, which I cannot/ dont want to fix in R.
## download as a csv, fix this in Excel, and then re-upload here.
## cells with multiple entries will be split into multiple rows, with vial numbers 2,3,4,...

write.csv(porewatersubsetlong,"processed/porewater_subset.csv")

## NOW that the file has been edited in Excel, reupload it here
porewatersubset2 = read.csv("processed/porewater_subset.csv")

porewatersubset2 %>% 
  select(-X) %>% 
  filter(!empty==0&!full==0) %>% 
# create a new column to note missing weights  
  mutate(notes2 = case_when(empty == "no weight recorded"~"empty:no weight recorded",
                            full == "no weight recorded"~"full:no weight recorded")) %>% 
# make the weight columns numeric
  dplyr::mutate(empty = as.numeric(as.character(empty)),
                full = as.numeric(as.character(full))) %>% 
# for missing empty weight, take average of all the others
  mutate(empty_corr = if_else(is.na(empty),mean(empty, na.rm=TRUE),empty)) %>% 
  dplyr::mutate(empty_corr = round(empty_corr,2),
                porewater_g = full-empty_corr) %>% 
# now create a column for total volume  
  group_by(Site, Core, suction) %>% 
#  dplyr::summarise(totalvolume_g = sum(porewater_g, na.rm = TRUE))->
# summarise creates a new summary table
# to view the totals in the same original table, comment that out and use the `mutate` function below
  dplyr::mutate(totalvolume_g = sum(porewater_g, na.rm = TRUE),
                totalvolume_g = na_if(totalvolume_g,0))->
  porewatersubset3

porewatersubset3 %>% 
  group_by(Site, Core, suction) %>% 
  dplyr::summarise(totalvolume_g = sum(porewater_g, na.rm = TRUE)) %>% 
  dplyr::mutate(totalvolume_g = na_if(totalvolume_g,0)) %>%
  dplyr::rename(totalvolume_mL = totalvolume_g,
         tension=suction)->
  porewater_weight

### COMBINE THIS WITH THE MG/L FILE
