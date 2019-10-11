### FTICR-soil output
### this script will create summary tables and graphs for the FTICR data.
### use the subset files created in scripts 3&4 as inputs here. Do not use the original data because those files are huge.

### Kaizad F. Patel
### September 2019

source("0-packages.R")

## do NOT source scripts 3 & 4.

## 1: aromatic peaks - summary ----
fticr_soil_aromatic = read_csv("fticr/fticr_soil_aromatic.csv")

fticr_soil_aromatic %>% 
  mutate(treatment = factor(treatment,
                            levels = c("baseline","time zero saturation", "field moist","saturation","drought")))->
  fticr_soil_aromatic

gg_aromaticpeaks = ggplot(fticr_soil_aromatic[fticr_soil_aromatic$aromatic=="aromatic",], 
       aes(x = site, y = arom_core_counts, color = treatment, fill = treatment))+
  geom_boxplot(fill = "white")+
  ylab("aromatic peaks")+
  geom_vline(xintercept = 1.5)+
  geom_vline(xintercept = 2.5)+
  theme_bw()+
  theme(
    legend.title=element_blank(),
    legend.text=element_text(size=12),
    panel.border=element_rect(color="black",size=1.5),
    axis.text=element_text(size=12,color="black"),axis.title=element_text(size=14,color="black",face="bold")
    )
save_plot("output/fticr_aromaticpeaks.tiff", gg_aromaticpeaks, 
          base_width = 12, base_height = 10)



#
## 2: NOSC plots ----
fticr_soil_nosc = read_csv("fticr/fticr_soil_nosc.csv")

gg_nosc_c = 
  ggplot(fticr_soil_nosc[fticr_soil_nosc$site=="CPCRW" &!fticr_soil_nosc$treatment=="baseline",], 
         aes(x = NOSC, fill = treatment))+
  geom_histogram(binwidth = 0.25, color = "black")+
  xlim(-2.5, 2.5)+
  ylim(0,25000)+
  theme(legend.position = "top")+
  ggtitle("CPCRW")

gg_nosc_d = 
  ggplot(fticr_soil_nosc[fticr_soil_nosc$site=="DWP" &!fticr_soil_nosc$treatment=="baseline",], 
         aes(x = NOSC, fill = treatment))+
  geom_histogram(binwidth = 0.25, color = "black")+
  xlim(-2.5, 2.5)+
  theme(legend.position = "none")+
  ylim(0,25000)+ggtitle("DWP")

gg_nosc_s = 
  ggplot(fticr_soil_nosc[fticr_soil_nosc$site=="SR" &!fticr_soil_nosc$treatment=="baseline",], 
         aes(x = NOSC, fill = treatment))+
  geom_histogram(binwidth = 0.25, color = "black")+
  xlim(-2.5, 2.5)+
  theme(legend.position = "none")+
  ylim(0,25000)+ggtitle("SR")

gg_nosc_combined = plot_grid(gg_nosc_c,gg_nosc_d,gg_nosc_s, ncol = 3, align = "h", axis = "bt");gg_nosc_combined
save_plot("output/fticr_nosc.tiff", gg_nosc_combined, 
          base_width = 20, base_height = 7)

#

## 3: plot kendrick ----
fticr_soil_kendrick = read_csv("fticr/fticr_soil_kendrick.csv")

## plotting kmd with only three treatments

gg_kendrick_c=
  ggplot(fticr_soil_kendrick[fticr_soil_kendrick$site=="CPCRW" & 
                             fticr_soil_kendrick$treatment=="drought" | 
                             fticr_soil_kendrick$treatment=="saturation"|
                             fticr_soil_kendrick$treatment=="field moist",], 
       aes(x = kmass, y = kdefect, color = treatment))+
  geom_point(size=0.5)+
  xlim(200,900)+
  ylim(0,1)+
  theme_bw()+
  theme(legend.position = "top",
        legend.title = element_blank())+
  ggtitle("CPCRW")


gg_kendrick_d=
  ggplot(fticr_soil_kendrick[fticr_soil_kendrick$site=="DWP"& 
                             fticr_soil_kendrick$treatment=="drought" | 
                             fticr_soil_kendrick$treatment=="saturation"|
                             fticr_soil_kendrick$treatment=="field moist",], 
       aes(x = kmass, y = kdefect, color = treatment))+
  geom_point(size=0.5)+
  xlim(200,900)+
  ylim(0,1)+
  theme_bw()+
  theme(legend.position = "top",
        legend.title = element_blank())+
  ggtitle("DWP")


gg_kendrick_s=
  ggplot(fticr_soil_kendrick[fticr_soil_kendrick$site=="SR" & 
                             fticr_soil_kendrick$treatment=="drought" | fticr_soil_kendrick$treatment=="saturation"|fticr_soil_kendrick$treatment=="field moist",], 
       aes(x = kmass, y = kdefect, color = treatment))+
  geom_point(size=0.5)+
  xlim(200,900)+
  ylim(0,1)+
  theme_bw()+
  theme(legend.position = "top",
        legend.title = element_blank())+
  ggtitle("SR")

gg_kendrick_combined = plot_grid(gg_kendrick_c,gg_kendrick_d,gg_kendrick_s,
                                 ncol = 3, align = "h", axis = "bt")

save_plot("output/fticr_kendrick.tiff", gg_kendrick_combined, 
          base_width = 20, base_height = 7)

#


## 4: van krevelen plots ----
fticr_soil_hcoc = read_csv("fticr/fticr_soil_hcoc.csv")

# 4a. baseline plot ----
gg_vankrev_base = 
  ggplot(fticr_soil_hcoc[fticr_soil_hcoc$treatment=="baseline",],
       aes(x = OtoC_ratio, y = HtoC_ratio, color = site))+
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
gg_vankrev_c=
  ggplot(fticr_soil_hcoc[fticr_soil_hcoc$site=="CPCRW" & 
                         fticr_soil_hcoc$treatment=="drought" | 
                         fticr_soil_hcoc$treatment=="saturation"|
                         fticr_soil_hcoc$treatment=="field moist",], 
       aes(x = OtoC_ratio, y = HtoC_ratio, color = treatment))+
  geom_point()+
  xlim(0,1.25)+
  ylim(0,3)+
  ggtitle("CPCRW")+
  theme_bw()+
  theme(
    legend.position = "top",
    legend.title = element_blank()
  )


gg_vankrev_d=
  ggplot(fticr_soil_hcoc[fticr_soil_hcoc$site=="DWP" & 
                         fticr_soil_hcoc$treatment=="drought" | 
                         fticr_soil_hcoc$treatment=="saturation"|
                         fticr_soil_hcoc$treatment=="field moist",], 
       aes(x = OtoC_ratio, y = HtoC_ratio, color = treatment))+
  geom_point()+
  xlim(0,1.25)+
  ylim(0,3)+
  ggtitle("DWP")+
  theme_bw()+
  theme(
    legend.position = "top",
    legend.title = element_blank()
  )


gg_vankrev_s=
  ggplot(fticr_soil_hcoc[fticr_soil_hcoc$site=="SR" & 
                         fticr_soil_hcoc$treatment=="drought" | 
                         fticr_soil_hcoc$treatment=="saturation"|
                         fticr_soil_hcoc$treatment=="field moist",], 
       aes(x = OtoC_ratio, y = HtoC_ratio, color = treatment))+
  geom_point()+
  xlim(0,1.25)+
  ylim(0,3)+
  ggtitle("SR")+
  theme_bw()+
  theme(
    legend.position = "top",
    legend.title = element_blank()
  )

gg_vankrev_c_marginal=ggMarginal(gg_vankrev_c,groupColour = TRUE,groupFill = TRUE)
gg_vankrev_d_marginal=ggMarginal(gg_vankrev_d,groupColour = TRUE,groupFill = TRUE)
gg_vankrev_s_marginal=ggMarginal(gg_vankrev_s,groupColour = TRUE,groupFill = TRUE)

gg_vankrev_combined = plot_grid(gg_vankrev_c_marginal, gg_vankrev_d_marginal,gg_vankrev_s_marginal,
                                ncol = 3, align = "h", axis = "bt"); gg_vankrev_combined

save_plot("output/fticr_vankrev_treatments.tiff", gg_vankrev_combined, base_width = 20, base_height = 7)

# 4c. new molecules ----
fticr_soil_newmolecules = read_csv("fticr/fticr_soil_newmolecules.csv")

gg_vankrev_newmolecules = 
  ggplot(
  fticr_soil_newmolecules[
    fticr_soil_newmolecules$newmolecules=="new" & 
      fticr_soil_newmolecules$site=="SR" &
      fticr_soil_newmolecules$treatment=="field.moist"|
      fticr_soil_newmolecules$treatment=="saturation"|
      fticr_soil_newmolecules$treatment=="drought",],
       aes(x = OtoC_ratio, y = HtoC_ratio, color = treatment))+
  geom_point()+
  ggtitle("new molecules")

gg_vankrev_newmolecules_marginal=ggMarginal(gg_vankrev_newmolecules,groupColour = TRUE,groupFill = TRUE)

gg_vankrev_lostmolecules = 
  ggplot(
    fticr_soil_newmolecules[
      fticr_soil_newmolecules$newmolecules=="lost" & 
        fticr_soil_newmolecules$site=="SR" &
        fticr_soil_newmolecules$treatment=="field.moist"|
        fticr_soil_newmolecules$treatment=="saturation"|
        fticr_soil_newmolecules$treatment=="drought",],
    aes(x = OtoC_ratio, y = HtoC_ratio, color = treatment))+
  geom_point()+
  ggtitle("SR lost molecules")

gg_vankrev_lostmolecules_marginal=ggMarginal(gg_vankrev_lostmolecules,groupColour = TRUE,groupFill = TRUE)

save_plot("output/fticr_vankrev_newmolecules.tiff", gg_vankrev_newmolecules_marginal, base_width = 10, base_height = 10)
save_plot("output/fticr_vankrev_lostmolecules.tiff", gg_vankrev_lostmolecules_marginal, base_width = 10, base_height = 10)


# 4d. unique molecules ----
fticr_soil_uniquemolecules = read_csv("fticr/fticr_soil_uniquemolecules.csv")

gg_vankrev_unique_c = 
  ggplot(fticr_soil_uniquemolecules[fticr_soil_uniquemolecules$site=="CPCRW" &
                                      fticr_soil_uniquemolecules$unique=="field_moist"|
                                      fticr_soil_uniquemolecules$unique=="saturation"|
                                      fticr_soil_uniquemolecules$unique=="drought",],
         aes(x = OtoC_ratio, y = HtoC_ratio, color = unique))+
  geom_point()+
  theme_bw()+
  theme(
    legend.position = "top",
    legend.title = element_blank())+
  ggtitle("CPCRW unique")


gg_vankrev_unique_d = 
  ggplot(fticr_soil_uniquemolecules[fticr_soil_uniquemolecules$site=="DWP" &
                                      fticr_soil_uniquemolecules$unique=="field_moist"|
                                      fticr_soil_uniquemolecules$unique=="saturation"|
                                      fticr_soil_uniquemolecules$unique=="drought",],
         aes(x = OtoC_ratio, y = HtoC_ratio, color = unique))+
  geom_point()+
  theme_bw()+
  theme(
    legend.position = "none")+
  ggtitle("DWP unique")


gg_vankrev_unique_s = 
  ggplot(fticr_soil_uniquemolecules[fticr_soil_uniquemolecules$site=="SR" &
                                    fticr_soil_uniquemolecules$unique=="field_moist"|
                                    fticr_soil_uniquemolecules$unique=="saturation"|
                                    fticr_soil_uniquemolecules$unique=="drought",],
       aes(x = OtoC_ratio, y = HtoC_ratio, color = unique))+
  geom_point()+
  theme_bw()+
  theme(
    legend.position = "none")+
  ggtitle("SR unique")

gg_vankrev_unique_c_marginal=ggMarginal(gg_vankrev_unique_c,groupColour = TRUE,groupFill = TRUE)
gg_vankrev_unique_d_marginal=ggMarginal(gg_vankrev_unique_d,groupColour = TRUE,groupFill = TRUE)
gg_vankrev_unique_s_marginal=ggMarginal(gg_vankrev_unique_s,groupColour = TRUE,groupFill = TRUE)

gg_vankrev_unique_combined = plot_grid(gg_vankrev_unique_c_marginal,
                                       gg_vankrev_unique_d_marginal,
                                       gg_vankrev_unique_s_marginal,
                                       ncol = 3, align = "h", axis = "bt")

save_plot("output/fticr_vankrev_uniquemolecules.tiff", 
          gg_vankrev_unique_combined, base_width = 20, base_height = 7)





#
## 5: relative abundance bar graphs ----
# use file fticr_pore_relabundance_summary
# first, remove the `total` row
fticr_soil_relabund_summary2 = fticr_soil_relabundance_summary[!fticr_soil_relabundance_summary$group=="total",]

# set Unnamed as last factor
old.lvl = levels(factor(fticr_soil_relabund_summary2$group))
fticr_soil_relabund_summary2$group = factor(fticr_soil_relabund_summary2$group, 
                                            levels=c(sort(old.lvl[old.lvl!="Unnamed"]), "Unnamed"))

gg_soil_relabund = 
  ggplot(fticr_soil_relabund_summary2, aes(x = treatment, y = relabund, fill = group))+
  geom_bar(stat = "summary")+
  facet_wrap(~site)+
  #scale_fill_brewer(palette = "Dark2")+
  scale_fill_viridis_d(option = "inferno")+
  xlab("")+
  ylab("% relative abundance")+
  ggtitle("soil")+
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

save_plot("output/fticr_soil_relabund.tiff", gg_soil_relabund,  base_height = 6, base_width = 7)
