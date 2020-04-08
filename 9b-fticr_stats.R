source("0b-packages.R")

# PART 1. LOAD FILES ----
# pore_relabund = read.csv(FTICR_PORE_RELABUND)# <- "fticr/fticr_pore_relabundance_groups2_hsd.csv"
# soil_relabund = read.csv(FTICR_SOIL_RELABUND)# <- "fticr/fticr_soil_relabundance_hsd.csv"

soil_raw_long = read.csv(FTICR_SOIL_RAW_LONG)# <- "fticr/fticr_soil_raw_longform.csv"
pore_raw_long = read.csv(FTICR_PORE_RAW_LONG)# <- "fticr/fticr_pore_raw_longform.csv"

#
## 1b. process files for analysis ----
# we want relative abundance for each core

soil_raw_long %>% 
  group_by(site, treatment,Class,core) %>% 
  dplyr::summarize(compounds = n()) %>% # sum all intensities for each Class
  # now calculate relative abundance for each Class for each core
  group_by(site, treatment, core) %>% 
  dplyr::mutate(total = sum(compounds),
                relabund = (compounds/total)*100)->
  soil_relabund_temp

relabund_temp = 
  pore_raw_long %>% 
  group_by(tension,site, treatment,Class,core) %>% 
  dplyr::summarize(compounds = n()) %>% # sum all COUNTS for each Class
  # now calculate relative abundance for each Class for each core
  group_by(tension,site, treatment, core) %>% 
  dplyr::mutate(total = sum(compounds),
                relabund = (compounds/total)*100)

relabund_pore = 
  relabund_temp %>% 
  dplyr::select(core, site, tension, treatment, Class, relabund) %>% 
  spread(Class, relabund)
  
  

#
# PART 2. FAMD ----










#
# PART VI: RELATIVE ABUNDANCE PCA ----

## a. pores ----
## native SOM pca
pore_relabund_pca=
  relabund_temp %>% 
  ungroup %>% 
  dplyr::select(core,tension, site, treatment, Class, relabund) %>% 
  filter(treatment=="time zero saturation") %>% 
  spread(Class, relabund) %>% 
  replace(.,is.na(.),0) %>% 
  dplyr::select(-1)

pore_relabund_pca_num = 
  pore_relabund_pca %>% 
  dplyr::select(.,-(1:3))

pore_relabund_pca_grp = 
  pore_relabund_pca %>% 
  dplyr::select(.,(1:3)) %>% 
  dplyr::mutate(row = row_number())

pca = prcomp(pore_relabund_pca_num, scale. = T)
summary(pca)

library(ggbiplot)
ggbiplot(pca, obs.scale = 1, var.scale = 1, 
         groups = pore_relabund_pca_grp$site, ellipse = TRUE, circle = F,
         var.axes = TRUE)+
  geom_point(size=2,stroke=2, aes(shape = pore_relabund_pca_grp$tension, color = groups))+
  scale_shape_manual(values = c(1,4))

bray_distance = vegdist(pore_relabund_pca_num, method="euclidean")
principal_coordinates = pcoa(bray_distance)

pcoa_plot = data.frame(principal_coordinates$vectors[,])
pcoa_plot_merged = merge(pcoa_plot, pore_relabund_pca_grp, by="row.names")

####### Calculate percent variation explained by PC1, PC2

PC1 <- 100*(principal_coordinates$values$Eigenvalues[1]/sum(principal_coordinates$values$Eigenvalues))
PC2 <- 100*(principal_coordinates$values$Eigenvalues[2]/sum(principal_coordinates$values$Eigenvalues))
PC3 <- 100*(principal_coordinates$values$Eigenvalues[3]/sum(principal_coordinates$values$Eigenvalues))

###### Plot PCoA

ggplot(data=pcoa_plot_merged,aes(x=Axis.1,y=Axis.2,color=treatment, shape=site)) + 
  geom_point(size=4)+
  facet_grid(tension~site)+
  stat_ellipse()+
  theme_kp()+
  theme(legend.position = "right")+
  labs(x = paste("PC1 - Variation Explained", round(PC1,2),"%"), y = paste("PC2 - Variation Explained", round(PC2,2),"%"))


## treatment PCA: all pores

pore_relabund_pca=
  relabund_temp %>% 
  ungroup %>% 
  dplyr::select(core,tension, site, treatment, Class, relabund) %>% 
  #filter(treatment=="time zero saturation") %>% 
  spread(Class, relabund) %>% 
  replace(.,is.na(.),0) %>% 
  dplyr::select(-1)

pore_relabund_pca_num = 
  pore_relabund_pca %>% 
  dplyr::select(.,-(1:3))

pore_relabund_pca_grp = 
  pore_relabund_pca %>% 
  dplyr::select(.,(1:3)) %>% 
  dplyr::mutate(row = row_number())

pca = prcomp(pore_relabund_pca_num, scale. = T)
summary(pca)

ggbiplot(pca, obs.scale = 1, var.scale = 1, 
         groups = pore_relabund_pca_grp$site, ellipse = F, circle = F,
         var.axes = TRUE)+
  geom_point(size=2,stroke=2, aes(shape = pore_relabund_pca_grp$tension, color = pore_relabund_pca_grp$treatment))+
  scale_shape_manual(values = c(19,4))+
  facet_wrap(~groups)

adonis(pore_relabund_pca_num ~ pore_relabund_pca$site+pore_relabund_pca$treatment+pore_relabund_pca$site:pore_relabund_pca$treatment, 
       method="bray", permutations=999)

## treatment PCA: fine pores

pore_relabund_pca=
  relabund_temp %>% 
  ungroup %>% 
  dplyr::select(core,tension, site, treatment, Class, relabund) %>% 
  #filter(treatment=="time zero saturation") %>% 
  filter(tension=="50 kPa") %>% 
  spread(Class, relabund) %>% 
  replace(.,is.na(.),0) 

pore_relabund_pca_num = 
  pore_relabund_pca %>% 
  dplyr::select(.,-(1:4))

pore_relabund_pca_grp = 
  pore_relabund_pca %>% 
  dplyr::select(.,(1:4)) %>% 
  dplyr::mutate(row = row_number())

pca = prcomp(pore_relabund_pca_num, scale. = T)
summary(pca)

ggbiplot(pca, obs.scale = 1, var.scale = 1, 
         groups = pore_relabund_pca_grp$site, ellipse = F, circle = F,
         var.axes = TRUE)+
  geom_point(size=2,stroke=2, aes(color = pore_relabund_pca_grp$treatment, shape = pore_relabund_pca_grp$site))+
  geom_text(label = pore_relabund_pca_grp$core)+
  scale_shape_manual(values = c(19,4,7))+
  #facet_wrap(~groups)+
  ggtitle("fine pores")

adonis(pore_relabund_pca_num ~ pore_relabund_pca$site+pore_relabund_pca$treatment+pore_relabund_pca$site:pore_relabund_pca$treatment, 
       method="bray", permutations=999)


## treatment PCA: coarse pores

pore_relabund_pca=
  relabund_temp %>% 
  ungroup %>% 
  dplyr::select(core,tension, site, treatment, Class, relabund) %>% 
  #filter(treatment=="time zero saturation") %>% 
  filter(tension=="1.5 kPa") %>% 
  spread(Class, relabund) %>% 
  replace(.,is.na(.),0) 

pore_relabund_pca_num = 
  pore_relabund_pca %>% 
  dplyr::select(.,-(1:4))

pore_relabund_pca_grp = 
  pore_relabund_pca %>% 
  dplyr::select(.,(1:4)) %>% 
  dplyr::mutate(row = row_number())

pca = prcomp(pore_relabund_pca_num, scale. = T)
summary(pca)

ggbiplot(pca, obs.scale = 1, var.scale = 1, 
         groups = pore_relabund_pca_grp$site, ellipse = F, circle = F,
         var.axes = TRUE)+
  geom_point(size=2,stroke=2, aes(color = pore_relabund_pca_grp$treatment, shape = pore_relabund_pca_grp$site))+
  scale_shape_manual(values = c(19,4,7))+
  geom_text(label = pore_relabund_pca_grp$core)+
  #facet_wrap(~groups)+
  ggtitle("coarse pores")






## b. soils ----
## native SOM pca
soil_relabund_pca=
  soil_relabund_temp %>% 
  ungroup %>% 
  dplyr::select(core, site, treatment, Class, relabund) %>% 
  filter(treatment=="time zero saturation") %>% 
  spread(Class, relabund) %>% 
  replace(.,is.na(.),0) %>% 
  dplyr::select(-1)

soil_relabund_pca_num = 
  soil_relabund_pca %>% 
  dplyr::select(.,-(1:2))

soil_relabund_pca_grp = 
  soil_relabund_pca %>% 
  dplyr::select(.,(1:2)) %>% 
  dplyr::mutate(row = row_number())

pca = prcomp(soil_relabund_pca_num, scale. = T)
summary(pca)

ggbiplot(pca, obs.scale = 1, var.scale = 1, 
         groups = soil_relabund_pca_grp$site, ellipse = TRUE, circle = F,
         var.axes = TRUE)+
  geom_point(size=2, stroke=2, aes(color=groups))+
  ylim(-4,4)+xlim(-4,4)+
  ggtitle("time zero saturation")

bray_distance = vegdist(pore_relabund_pca_num, method="euclidean")
principal_coordinates = pcoa(bray_distance)

pcoa_plot = data.frame(principal_coordinates$vectors[,])
pcoa_plot_merged = merge(pcoa_plot, pore_relabund_pca_grp, by="row.names")

####### Calculate percent variation explained by PC1, PC2

PC1 <- 100*(principal_coordinates$values$Eigenvalues[1]/sum(principal_coordinates$values$Eigenvalues))
PC2 <- 100*(principal_coordinates$values$Eigenvalues[2]/sum(principal_coordinates$values$Eigenvalues))
PC3 <- 100*(principal_coordinates$values$Eigenvalues[3]/sum(principal_coordinates$values$Eigenvalues))

###### Plot PCoA

ggplot(data=pcoa_plot_merged,aes(x=Axis.1,y=Axis.2,color=treatment, shape=site)) + 
  geom_point(size=4)+
  facet_grid(tension~site)+
  stat_ellipse()+
  theme_kp()+
  theme(legend.position = "right")+
  labs(x = paste("PC1 - Variation Explained", round(PC1,2),"%"), y = paste("PC2 - Variation Explained", round(PC2,2),"%"))


## treatment PCA

soil_relabund_pca=
  soil_relabund_temp %>% 
  ungroup %>% 
  dplyr::select(core, site, treatment, Class, relabund) %>% 
  filter(!treatment=="baseline") %>% 
  spread(Class, relabund) %>% 
  replace(.,is.na(.),0) %>% 
  dplyr::select(-1)

soil_relabund_pca_num = 
  soil_relabund_pca %>% 
  dplyr::select(.,-(1:2))

soil_relabund_pca_grp = 
  soil_relabund_pca %>% 
  dplyr::select(.,(1:2)) %>% 
  dplyr::mutate(row = row_number())

pca = prcomp(soil_relabund_pca_num, scale. = T)
summary(pca)

ggbiplot(pca, obs.scale = 1, var.scale = 1, 
         groups = soil_relabund_pca_grp$treatment, ellipse = T, circle = F,
         var.axes = TRUE)+
  geom_point(size=2, stroke=1,aes(shape = soil_relabund_pca_grp$site, color = groups))+
  scale_shape_manual(values = c(1,0,2))

adonis(soil_relabund_pca_num ~ soil_relabund_pca$site+soil_relabund_pca$treatment+soil_relabund_pca$site:soil_relabund_pca$treatment, 
       method="bray", permutations=999)
