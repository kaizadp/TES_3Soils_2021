### FTICR-pores output
### this script will create summary tables and graphs for the FTICR pores data.
### use the subset files created in scripts 3&4 as inputs here. Do not use the original data because those files are huge.

### Kaizad F. Patel
### September 2019

source("0-packages.R")

## do NOT source scripts 3 & 4.

## 1: aromatic peaks - summary ----
fticr_pore_aromatic = read_csv("fticr/fticr_pore_aromatic_counts.csv")

fticr_pore_aromatic_core_counts = summarySE()


fticr_pore_aromatic %>% 
  mutate(treatment = factor(treatment,
                            levels = c("baseline","time zero saturation", "field moist","saturation","drought")))->
  fticr_pore_aromatic

## splitting by peaks 
gg_pore_aromaticpeaks_50=
  ggplot(fticr_pore_aromatic[
    fticr_pore_aromatic$tension=="50 kPa" ,], 
       aes(x = site, y = arom_core_counts, color = treatment, fill = treatment))+
  geom_boxplot(position = "dodge", fill = "white", lwd = 1,fatten = 1)+ # fatten changes thickness of median line, lwd changes thickness of all lines
  geom_dotplot(binaxis = "y",position = position_dodge(0.75), stackdir = "center", 
               dotsize = 0.3, color = "black")+
  ylab("aromatic peaks")+
  ylim(0,350)+
  geom_vline(xintercept = 1.5)+
  geom_vline(xintercept = 2.5)+
  theme_bw()+
  ggtitle("50 kPa")+
  theme(
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.text=element_text(size=12),
    panel.border=element_rect(color="black",size=1.5),
    axis.text=element_text(size=12,color="black"),
    axis.title=element_text(size=14,color="black",face="bold")
    )

gg_pore_aromaticpeaks_1=
  ggplot(fticr_pore_aromatic[
  fticr_pore_aromatic$tension=="1.5 kPa" ,], 
  aes(x = site, y = arom_core_counts, color = treatment, fill = treatment))+
  geom_boxplot(position = "dodge", fill = "white", lwd = 1,fatten = 1)+ # fatten changes thickness of median line, lwd changes thickness of all lines
  geom_dotplot(binaxis = "y",position = position_dodge(0.75), 
               stackdir = "center", dotsize = 0.3, color = "black")+
  ylab("aromatic peaks")+
  ylim(0,350)+
  geom_vline(xintercept = 1.5)+
  geom_vline(xintercept = 2.5)+
  theme_bw()+
  ggtitle("1.5 kPa")+
  theme(
    legend.position = "none",
    legend.title=element_blank(),
    legend.text=element_text(size=12),
    panel.border=element_rect(color="black",size=1.5),
    axis.text=element_text(size=12,color="black"),
    axis.title=element_text(size=14,color="black",face="bold")
  )

gg_pore_aromaticpeaks = plot_grid(gg_pore_aromaticpeaks_1, gg_pore_aromaticpeaks_50,
                                  ncol = 2, align = "hv", axis = "bt")
  
save_plot("output/fticr_pore_aromaticpeaks.tiff", gg_pore_aromaticpeaks, 
          base_width = 20, base_height = 10)



#
## 2a: NOSC plots overlaid ----
fticr_pore_nosc = read_csv("fticr/fticr_pore_nosc.csv")

fticr_pore_nosc %>% 
  mutate(treatment = factor(treatment,
                            levels = c("baseline","time zero saturation", "field moist","saturation","drought")))->
  fticr_pore_nosc

gg_pore_nosc_c=
  ggplot(fticr_pore_nosc[fticr_pore_nosc$site=="CPCRW" &!fticr_pore_nosc$treatment=="baseline",], 
         aes(x = NOSC, fill = treatment, color = treatment))+
  geom_histogram(binwidth = 0.25, position = "identity", alpha = 0.2)+ # position = "identity" makes it overlaid. position = "dodge" makes them staggered
  facet_wrap(~tension)+
  xlim(-2.5, 2.5)+
  ylim(0,4000)+
  ggtitle("CPCRW")+
  theme_bw()+
  theme(
    legend.position = "top",
    legend.title=element_blank(),
    legend.text=element_text(size=12),
    panel.border=element_rect(color="black",fill = NA, size=1.5),
    axis.text=element_text(size=12,color="black"),
    axis.title=element_text(size=14,color="black",face="bold")
  )

gg_pore_nosc_d=
  ggplot(fticr_pore_nosc[fticr_pore_nosc$site=="DWP" &
                         !fticr_pore_nosc$treatment=="baseline",], 
       aes(x = NOSC, fill = treatment))+
  geom_histogram(binwidth = 0.25, color = "black")+
  facet_wrap(~tension)+
  xlim(-2.5, 2.5)+
  ylim(0,10000)+
  ggtitle("DWP")+
  theme_bw()+
  theme(
    legend.position = "none",
    legend.title=element_blank(),
    legend.text=element_text(size=12),
    panel.border=element_rect(color="black",fill = NA, size=1.5),
    axis.text=element_text(size=12,color="black"),
    axis.title=element_text(size=14,color="black",face="bold")
  )

gg_pore_nosc_s=
  ggplot(fticr_pore_nosc[fticr_pore_nosc$site=="SR" &
                         !fticr_pore_nosc$treatment=="baseline",], 
       aes(x = NOSC, fill = treatment))+
  geom_histogram(binwidth = 0.25, color = "black")+
  facet_wrap(~tension)+
  xlim(-2.5, 2.5)+
  ylim(0,10000)+
  ggtitle("SR")+
  theme_bw()+
  theme(
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.text=element_text(size=12),
    panel.border=element_rect(color="black",fill = NA, size=1.5),
    axis.text=element_text(size=12,color="black"),
    axis.title=element_text(size=14,color="black",face="bold")
  )

gg_pore_nosc_combined = plot_grid(gg_pore_nosc_c,gg_pore_nosc_d,gg_pore_nosc_s, nrow = 3, align = "hv", axis = "bt");gg_pore_nosc_combined
save_plot("output/fticr_pore_nosc.tiff", gg_pore_nosc_combined, 
          base_width = 7, base_height = 10)

#
## 2b: NOSC plots boxplot ----
fticr_pore_nosc = read_csv("fticr/fticr_pore_nosc.csv")

fticr_pore_nosc %>% 
  mutate(treatment = factor(treatment,
                            levels = c("baseline","time zero saturation", "field moist","saturation","drought")))->
  fticr_pore_nosc

gg_pore_nosc_boxplot = ggplot(fticr_pore_nosc[!fticr_pore_nosc$treatment=="baseline",], 
       aes(x = site,y = NOSC, fill = treatment, color = treatment))+
  #geom_histogram(binwidth = 0.25, position = "identity", alpha = 0.2)+
  #geom_histogram(data = subset(fticr_pore_nosc, site = "CPCRW" | treatment=="field moist"), fill = "red", alpha = 0.2)+
  geom_boxplot(width = 0.5, position = position_dodge(0.7), fill = "white", lwd = 1,fatten = 1)+ # fatten changes thickness of median line, lwd changes thickness of all lines
  geom_vline(xintercept = 1.5)+
  geom_vline(xintercept = 2.5)+
  #geom_dotplot(binaxis = "y",position = position_dodge(0.75), stackdir = "center", dotsize = 0.3, color = "black")+
  facet_wrap(~tension)+
  #xlim(-2.5, 2.5)+
  #ylim(0,4000)+
  #ggtitle("CPCRW")+
  theme_bw()+
  theme(
    legend.position = "top",
    legend.title=element_blank(),
    legend.text=element_text(size=12),
    panel.border=element_rect(color="black",fill = NA, size=1.5),
    axis.text=element_text(size=12,color="black"),
    axis.title=element_text(size=14,color="black",face="bold")
  )

save_plot("output/fticr_pore_nosc_boxplot.tiff", gg_pore_nosc_boxplot, 
          base_width = 15, base_height = 5)

#

## 2c: NOSC plots individual treatment ----
fticr_pore_nosc = read_csv("fticr/fticr_pore_nosc.csv")

fticr_pore_nosc %>% 
  mutate(treatment = factor(treatment,
                            levels = c("baseline","time zero saturation", "field moist","saturation","drought")))->
  fticr_pore_nosc

gg_pore_nosc_c_trt=
  ggplot(fticr_pore_nosc[fticr_pore_nosc$site=="CPCRW" &!fticr_pore_nosc$treatment=="baseline",], 
       aes(x = NOSC, fill = treatment, color = treatment))+
  geom_histogram(binwidth = 0.25, position = "identity", alpha = 0.2)+
  #geom_histogram(data = subset(fticr_pore_nosc, site = "CPCRW" | treatment=="field moist"), fill = "red", alpha = 0.2)+
  facet_grid(treatment~tension)+ #facet with two variables
  xlim(-2.5, 2.5)+
  ylim(0,4000)+
  ggtitle("CPCRW")+
  theme_bw()+
  theme(
    legend.position = "none",
    legend.title=element_blank(),
    legend.text=element_text(size=12),
    panel.border=element_rect(color="black",fill = NA, size=1.5),
    axis.text=element_text(size=12,color="black"),
    axis.title=element_text(size=14,color="black",face="bold")
  )


gg_pore_nosc_d_trt=
  ggplot(fticr_pore_nosc[fticr_pore_nosc$site=="DWP" &!fticr_pore_nosc$treatment=="baseline",], 
         aes(x = NOSC, fill = treatment, color = treatment))+
  geom_histogram(binwidth = 0.25, position = "identity", alpha = 0.2)+
  #geom_histogram(data = subset(fticr_pore_nosc, site = "CPCRW" | treatment=="field moist"), fill = "red", alpha = 0.2)+
  facet_grid(treatment~tension)+
  xlim(-2.5, 2.5)+
  ylim(0,4000)+
  ggtitle("DWP")+
  theme_bw()+
  theme(
    legend.position = "none",
    legend.title=element_blank(),
    legend.text=element_text(size=12),
    panel.border=element_rect(color="black",fill = NA, size=1.5),
    axis.text=element_text(size=12,color="black"),
    axis.title=element_text(size=14,color="black",face="bold")
  )


gg_pore_nosc_s_trt=
  ggplot(fticr_pore_nosc[fticr_pore_nosc$site=="SR" &!fticr_pore_nosc$treatment=="baseline",], 
         aes(x = NOSC, fill = treatment, color = treatment))+
  geom_histogram(binwidth = 0.25, position = "identity", alpha = 0.2)+
  #geom_histogram(data = subset(fticr_pore_nosc, site = "CPCRW" | treatment=="field moist"), fill = "red", alpha = 0.2)+
  facet_grid(treatment~tension)+
  xlim(-2.5, 2.5)+
  ylim(0,4000)+
  ggtitle("SR")+
  theme_bw()+
  theme(
    legend.position = "none",
    legend.title=element_blank(),
    legend.text=element_text(size=12),
    panel.border=element_rect(color="black",fill = NA, size=1.5),
    axis.text=element_text(size=12,color="black"),
    axis.title=element_text(size=14,color="black",face="bold")
  )

save_plot("output/fticr_pore_nosc_cpcrw.tiff", gg_pore_nosc_c_trt, 
          base_width = 7, base_height = 10)
save_plot("output/fticr_pore_nosc_dwp.tiff", gg_pore_nosc_d_trt, 
          base_width = 7, base_height = 10)
save_plot("output/fticr_pore_nosc_sr.tiff", gg_pore_nosc_s_trt, 
          base_width = 7, base_height = 10)


#
## 3: plot kendrick ----
fticr_pore_kendrick = read_csv("fticr/fticr_pore_kendrick.csv")

## plotting kmd with only three treatments

gg_pore_kendrick_c= 
  ggplot(fticr_pore_kendrick[fticr_pore_kendrick$site=="CPCRW"&
                             !fticr_pore_kendrick$treatment=="time zero saturation",], 
       aes(x = kmass, y = kdefect, color = treatment))+
  geom_point(size=0.5)+
  facet_wrap(~tension)+
  xlim(200,900)+
  ylim(0,1)+
  theme(legend.position = "top",
        legend.title = element_blank())+
  ggtitle("CPCRW")+
  theme_bw()+
  theme(
    legend.position = "none",
    legend.title=element_blank(),
    legend.text=element_text(size=12),
    panel.border=element_rect(color="black",fill = NA, size=1.5),
    axis.text=element_text(size=12,color="black"),
    axis.title=element_text(size=14,color="black",face="bold")
  )

gg_pore_kendrick_d= 
  ggplot(fticr_pore_kendrick[fticr_pore_kendrick$site=="DWP"&
                             !fticr_pore_kendrick$treatment=="time zero saturation",], 
       aes(x = kmass, y = kdefect, color = treatment))+
  geom_point(size=0.5)+
  facet_wrap(~tension)+
  xlim(200,900)+
  ylim(0,1)+
  theme(legend.position = "top",
        legend.title = element_blank())+
  ggtitle("DWP")+
  theme_bw()+
  theme(
    legend.position = "none",
    legend.title=element_blank(),
    legend.text=element_text(size=12),
    panel.border=element_rect(color="black",fill = NA, size=1.5),
    axis.text=element_text(size=12,color="black"),
    axis.title=element_text(size=14,color="black",face="bold")
  )

gg_pore_kendrick_s= 
  ggplot(fticr_pore_kendrick[fticr_pore_kendrick$site=="SR"&
                             !fticr_pore_kendrick$treatment=="time zero saturation",], 
       aes(x = kmass, y = kdefect, color = treatment))+
  geom_point(size=0.5)+
  facet_wrap(~tension)+
  xlim(200,900)+
  ylim(0,1)+
  theme(legend.position = "top",
        legend.title = element_blank())+
  ggtitle("SR")+
  theme_bw()+
  theme(
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.text=element_text(size=12),
    panel.border=element_rect(color="black",fill = NA, size=1.5),
    axis.text=element_text(size=12,color="black"),
    axis.title=element_text(size=14,color="black",face="bold")
  )


gg_pore_kendrick = plot_grid(gg_pore_kendrick_c,
                             gg_pore_kendrick_d,
                             gg_pore_kendrick_s,
                             nrow = 3, align = "hv", axis = "bt")

save_plot("output/fticr_pore_kendrick.tiff", gg_pore_kendrick, 
          base_width = 5, base_height = 10)

#


## 4: van krevelen plots ----
fticr_pore_hcoc = read_csv("fticr/fticr_pore_hcoc.csv")

fticr_pore_hcoc %>% 
  mutate(treatment = factor(treatment,
                            levels = c("baseline","time zero saturation", "field moist","saturation","drought")))->
  fticr_pore_hcoc

# 4a. baseline plot ----
## don't do for pores
gg_vankrev_base

ggplot(fticr_pore_hcoc[fticr_pore_hcoc$treatment=="baseline",],
       aes(x = OC, y = HC, color = site))+
  facet_wrap(~tension)+
  xlab("O/C")+
  ylab("H/C")+
  theme_bw()+
  theme(
    legend.title = element_blank()
    )+
  geom_point()+
  ggtitle("baseline")

gg_vankrev_base_marginal=ggMarginal(gg_vankrev_base,groupColour = TRUE,groupFill = TRUE)
save_plot("output/fticr_vankrev_baseline.tiff", gg_vankrev_base_marginal, base_width = 10, base_height = 10)



# 4b. treatment plots ----
gg_pore_vankrev_c=
  ggplot(fticr_pore_hcoc[fticr_pore_hcoc$site=="CPCRW" & 
                         fticr_pore_hcoc$treatment=="drought" | 
                         fticr_pore_hcoc$treatment=="saturation"|
                         fticr_pore_hcoc$treatment=="field moist",], 
       aes(x = OC, y = HC, color = treatment))+
  geom_point()+
  facet_wrap(~tension)+
  xlim(0,1.25)+
  ylim(0,3)+
  ggtitle("CPCRW")+
  theme_bw()+
  theme(
    legend.position = "none",
    legend.title=element_blank(),
    legend.text=element_text(size=12),
    panel.border=element_rect(color="black",fill = NA, size=1.5),
    axis.text=element_text(size=12,color="black"),
    axis.title=element_text(size=14,color="black",face="bold")
  )

gg_pore_vankrev_d=
  ggplot(fticr_pore_hcoc[fticr_pore_hcoc$site=="DWP" & 
                           fticr_pore_hcoc$treatment=="drought" | 
                           fticr_pore_hcoc$treatment=="saturation"|
                           fticr_pore_hcoc$treatment=="field moist",], 
         aes(x = OC, y = HC, color = treatment))+
  geom_point()+
  facet_wrap(~tension)+
  xlim(0,1.25)+
  ylim(0,3)+
  ggtitle("DWP")+
  theme_bw()+
  theme(
    legend.position = "none",
    legend.title=element_blank(),
    legend.text=element_text(size=12),
    panel.border=element_rect(color="black",fill = NA, size=1.5),
    axis.text=element_text(size=12,color="black"),
    axis.title=element_text(size=14,color="black",face="bold")
  )

gg_pore_vankrev_s=
  ggplot(fticr_pore_hcoc[fticr_pore_hcoc$site=="SR" & 
                           fticr_pore_hcoc$treatment=="drought" | 
                           fticr_pore_hcoc$treatment=="saturation"|
                           fticr_pore_hcoc$treatment=="field moist",], 
         aes(x = OC, y = HC, color = treatment))+
  geom_point()+
  facet_wrap(~tension)+
  xlim(0,1.25)+
  ylim(0,3)+
  ggtitle("SR")+
  theme_bw()+
  theme(
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.text=element_text(size=12),
    panel.border=element_rect(color="black",fill = NA, size=1.5),
    axis.text=element_text(size=12,color="black"),
    axis.title=element_text(size=14,color="black",face="bold")
  )

# marginal not working for facets?
# gg_pore_vankrev_c_marginal=ggMarginal(gg_pore_vankrev_c,groupColour = TRUE,groupFill = TRUE)
# gg_pore_vankrev_d_marginal=ggMarginal(gg_pore_vankrev_d,groupColour = TRUE,groupFill = TRUE)
# gg_pore_vankrev_s_marginal=ggMarginal(gg_pore_vankrev_s,groupColour = TRUE,groupFill = TRUE)

gg_pore_vankrev_combined = plot_grid(gg_pore_vankrev_c, gg_pore_vankrev_d,gg_pore_vankrev_s,
                                nrow = 3, align = "hv", axis = "bt"); gg_pore_vankrev_combined

save_plot("output/fticr_pore_vankrev_treatments.tiff", gg_pore_vankrev_combined, 
          base_width = 7, base_height = 15)

# 4c. new molecules ----
fticr_pore_newmolecules = read_csv("fticr/fticr_pore_newmolecules.csv")

# new molecules
gg_pore_vankrev_newmolecules_c=
  ggplot(
  fticr_pore_newmolecules[
    fticr_pore_newmolecules$newmolecules=="new" & 
      fticr_pore_newmolecules$site=="CPCRW",],
       aes(x = OC, y = HC, color = treatment))+
  facet_wrap(~tension)+
  geom_point()+
  xlim(0,1.25)+
  ylim(0,3)+
  ggtitle("new molecules: CPCRW")+
  theme_bw()+
  theme(
    legend.position = "none",
    legend.title=element_blank(),
    legend.text=element_text(size=12),
    panel.border=element_rect(color="black",fill = NA, size=1.5),
    axis.text=element_text(size=12,color="black"),
    axis.title=element_text(size=14,color="black",face="bold")
  )

gg_pore_vankrev_newmolecules_d=
  ggplot(
  fticr_pore_newmolecules[
    fticr_pore_newmolecules$newmolecules=="new" & 
      fticr_pore_newmolecules$site=="DWP",],
  aes(x = OC, y = HC, color = treatment))+
  facet_wrap(~tension)+
  geom_point()+
  xlim(0,1.25)+
  ylim(0,3)+
  ggtitle("new molecules: DWP")+
  theme_bw()+
  theme(
    legend.position = "none",
    legend.title=element_blank(),
    legend.text=element_text(size=12),
    panel.border=element_rect(color="black",fill = NA, size=1.5),
    axis.text=element_text(size=12,color="black"),
    axis.title=element_text(size=14,color="black",face="bold")
  )

gg_pore_vankrev_newmolecules_s=
  ggplot(
  fticr_pore_newmolecules[
    fticr_pore_newmolecules$newmolecules=="new" & 
      fticr_pore_newmolecules$site=="SR",],
  aes(x = OC, y = HC, color = treatment))+
  facet_wrap(~tension)+
  geom_point()+
  xlim(0,1.25)+
  ylim(0,3)+
  ggtitle("new molecules: SR")+
  theme_bw()+
  theme(
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.text=element_text(size=12),
    panel.border=element_rect(color="black",fill = NA, size=1.5),
    axis.text=element_text(size=12,color="black"),
    axis.title=element_text(size=14,color="black",face="bold")
  )


gg_pore_vankrev_newmolecules = plot_grid(gg_pore_vankrev_newmolecules_c,
                                         gg_pore_vankrev_newmolecules_d,
                                         gg_pore_vankrev_newmolecules_s,
                                         nrow = 3, align = "hv", axis = "bt")  

save_plot("output/fticr_pore_vankrev_newmolecules.tiff", gg_pore_vankrev_newmolecules, 
          base_width = 6, base_height = 12)


# lost molecules

gg_pore_vankrev_lostmolecules_c=
  ggplot(
    fticr_pore_newmolecules[
      fticr_pore_newmolecules$newmolecules=="lost" & 
        fticr_pore_newmolecules$site=="CPCRW",],
    aes(x = OC, y = HC, color = treatment))+
  facet_wrap(~tension)+
  geom_point()+
  xlim(0,1.25)+
  ylim(0,3)+
  ggtitle("lost molecules: CPCRW")+
  theme_bw()+
  theme(
    legend.position = "none",
    legend.title=element_blank(),
    legend.text=element_text(size=12),
    panel.border=element_rect(color="black",fill = NA, size=1.5),
    axis.text=element_text(size=12,color="black"),
    axis.title=element_text(size=14,color="black",face="bold")
  )

gg_pore_vankrev_lostmolecules_d=
  ggplot(
    fticr_pore_newmolecules[
      fticr_pore_newmolecules$newmolecules=="lost" & 
        fticr_pore_newmolecules$site=="DWP",],
    aes(x = OC, y = HC, color = treatment))+
  facet_wrap(~tension)+
  geom_point()+
  xlim(0,1.25)+
  ylim(0,3)+
  ggtitle("lost molecules: DWP")+
  theme_bw()+
  theme(
    legend.position = "none",
    legend.title=element_blank(),
    legend.text=element_text(size=12),
    panel.border=element_rect(color="black",fill = NA, size=1.5),
    axis.text=element_text(size=12,color="black"),
    axis.title=element_text(size=14,color="black",face="bold")
  )

gg_pore_vankrev_lostmolecules_s=
  ggplot(
    fticr_pore_newmolecules[
      fticr_pore_newmolecules$newmolecules=="lost" & 
        fticr_pore_newmolecules$site=="SR",],
    aes(x = OC, y = HC, color = treatment))+
  facet_wrap(~tension)+
  geom_point()+
  xlim(0,1.25)+
  ylim(0,3)+
  ggtitle("lost molecules: SR")+
  theme_bw()+
  theme(
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.text=element_text(size=12),
    panel.border=element_rect(color="black",fill = NA, size=1.5),
    axis.text=element_text(size=12,color="black"),
    axis.title=element_text(size=14,color="black",face="bold")
  )


gg_pore_vankrev_lostmolecules = plot_grid(gg_pore_vankrev_lostmolecules_c,
                                         gg_pore_vankrev_lostmolecules_d,
                                         gg_pore_vankrev_lostmolecules_s,
                                         nrow = 3, align = "hv", axis = "bt")  

save_plot("output/fticr_pore_vankrev_lostmolecules.tiff", gg_pore_vankrev_lostmolecules, 
          base_width = 6, base_height = 12)



# 4d. unique molecules ----
fticr_pore_uniquemolecules = read_csv("fticr/fticr_pore_uniquemolecules.csv")

gg_pore_vankrev_unique_c=
  ggplot(fticr_pore_uniquemolecules[fticr_pore_uniquemolecules$site=="CPCRW",],
         aes(x = OC, y = HC, color = unique))+
  facet_wrap(~tension)+
  geom_point()+
  xlim(0,1.25)+
  ylim(0,3)+
  ggtitle("unique:CPCRW")+
  theme_bw()+
  theme(
    legend.position = "none",
    legend.title=element_blank(),
    legend.text=element_text(size=12),
    panel.border=element_rect(color="black",fill = NA, size=1.5),
    axis.text=element_text(size=12,color="black"),
    axis.title=element_text(size=14,color="black",face="bold")
  )


gg_pore_vankrev_unique_d=
  ggplot(fticr_pore_uniquemolecules[fticr_pore_uniquemolecules$site=="DWP",],
       aes(x = OC, y = HC, color = unique))+
  facet_wrap(~tension)+
  geom_point()+
  xlim(0,1.25)+
  ylim(0,3)+
  ggtitle("unique: DWP")+
  theme_bw()+
  theme(
    legend.position = "none",
    legend.title=element_blank(),
    legend.text=element_text(size=12),
    panel.border=element_rect(color="black",fill = NA, size=1.5),
    axis.text=element_text(size=12,color="black"),
    axis.title=element_text(size=14,color="black",face="bold")
  )

gg_pore_vankrev_unique_s=
  ggplot(fticr_pore_uniquemolecules[fticr_pore_uniquemolecules$site=="SR",],
       aes(x = OC, y = HC, color = unique))+
  facet_wrap(~tension)+
  geom_point()+
  xlim(0,1.25)+
  ylim(0,3)+
  ggtitle("unique: SR")+
  theme_bw()+
  theme(
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.text=element_text(size=12),
    panel.border=element_rect(color="black",fill = NA, size=1.5),
    axis.text=element_text(size=12,color="black"),
    axis.title=element_text(size=14,color="black",face="bold")
  )



gg_pore_vankrev_unique = plot_grid(gg_pore_vankrev_unique_c,
                                   gg_pore_vankrev_unique_d,
                                   gg_pore_vankrev_unique_s,
                                       nrow = 3, align = "hv", axis = "bt")

save_plot("output/fticr_pore_vankrev_uniquemolecules.tiff", 
          gg_pore_vankrev_unique, base_width = 6, base_height = 12)




#
## 5: relative abundance bar graphs ----
# use file fticr_pore_relabundance_summary
# first, remove the `total` row
fticr_pore_relabund_summary2 = fticr_pore_relabundance_summary[!fticr_pore_relabundance_summary$group=="total",]

# set Other as last factor
old.lvl = levels(factor(fticr_pore_relabund_summary2$group))
fticr_pore_relabund_summary2$group = factor(fticr_pore_relabund_summary2$group, 
                                            levels=c(sort(old.lvl[old.lvl!="Other"]), "Other"))

gg_pore_relabund = 
ggplot(fticr_pore_relabund_summary2, aes(x = treatment, y = relabund, fill = group))+
  geom_bar(stat = "summary")+
  facet_grid(tension~site)+
  #scale_fill_brewer(palette = "Dark2")+
  scale_fill_viridis_d(option = "inferno")+
  xlab("")+
  ylab("% relative abundance")+
  theme_bw()+
  theme(panel.grid=element_blank(),
        legend.position="top",
        strip.background = element_rect(colour="white", fill="white"), #facet formatting
        panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
        strip.text.x = element_text(size=12, face="bold"), #facet labels
        strip.text.y = element_text(size=12, face="bold"), #facet labels
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border=element_rect(color="black",size=1),
        axis.text=element_text(size=12,color="black"),
        axis.title=element_text(size=14,color="black",face="bold"))

save_plot("output/fticr_pore_relabund.tiff", gg_pore_relabund,  base_height = 7, base_width = 7)
