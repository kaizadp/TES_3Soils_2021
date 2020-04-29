# 3Soils
# fticr_stats 
# Kaizad F. Patel
# March 2020

# this file takes the processed fticr files and runs statistics 


source("0b-packages.R")

# PART 1. LOAD FILES ----
# pore_relabund = read.csv(FTICR_PORE_RELABUND)# <- "fticr/fticr_pore_relabundance_groups2_hsd.csv"
# soil_relabund = read.csv(FTICR_SOIL_RELABUND)# <- "fticr/fticr_soil_relabundance_hsd.csv"

soil_raw_long = read.csv(FTICR_SOIL_RAW_LONG)# <- "fticr/fticr_soil_raw_longform.csv"
pore_raw_long = read.csv(FTICR_PORE_RAW_LONG)# <- "fticr/fticr_pore_raw_longform.csv"

#
## 1b. process files for analysis ----
# we want relative abundance for each core
## pores ----
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
  spread(Class, relabund) %>% 
  replace(is.na(.),0)

## soils ----
soil_relabund_temp = 
  soil_raw_long %>% 
  group_by(site, treatment,Class,core) %>% 
  dplyr::summarize(compounds = n()) %>% # sum all intensities for each Class
  # now calculate relative abundance for each Class for each core
  group_by(site, treatment, core) %>% 
  dplyr::mutate(total = sum(compounds),
                relabund = (compounds/total)*100)

relabund_soil = 
  soil_relabund_temp %>% 
  dplyr::select(core, site, treatment, Class, relabund) %>% 
  spread(Class, relabund) %>% 
  replace(is.na(.),0)  
  


  
  

#
# PART 2. FAMD ??? ----
library(FactoMineR)
library(factoextra)

res.famd = FAMD(relabund_pore, graph = TRUE)
summary(famd)

data("wine")
df <- wine[, c(1, 2, 16, 22, 29, 28, 30,31)]
res.famd <- FAMD(df, graph = FALSE)
summary(res.famd)
print(res.famd)
plot(res.famd, choix = "ind")
plot(res.famd, choix = "var")
plot(res.famd, choix = "quanti")
plot(res.famd, choix = "quali")




#
# PART 3. MANOVA ----
## pores ----
relabund_pore$DV = as.matrix(relabund_pore[,5:13])

# since the relative abundances are not strictly independent and all add to 100 %,
# use the isometric log ratio transformation
# http://www.sthda.com/english/wiki/manova-test-in-r-multivariate-analysis-of-variance#import-your-data-into-r

library(compositions)

pore_man = manova(ilr(clo(DV)) ~ site*tension*treatment, data = relabund_pore)
summary(pore_man)

## soils ----
relabund_soil$DV = as.matrix(relabund_soil[,4:12])
soil_man = manova(ilr(clo(DV)) ~ site*treatment, data = relabund_soil)
summary(soil_man)

soil_man = manova(ilr(clo(DV)) ~ treatment, data = relabund_soil[relabund_soil$site=="SR",])
summary(soil_man)


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


#
### tpc method ----
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





adonis(pore_relabund_pca_num ~ pore_relabund_pca$site+pore_relabund_pca$treatment+pore_relabund_pca$site:pore_relabund_pca$treatment, 
       method="bray", permutations=999)




#

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

# tpc method ----
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


## treatment PCA ----

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


# testing FAMD ----
## delete later
# https://www.r-bloggers.com/factoextra-r-package-easy-multivariate-data-analyses-and-elegant-visualization/

data("decathlon2")
df <- decathlon2[1:23, 1:10]
res.pca <- PCA(decathlon2[,-13],  graph = FALSE)
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))
var <- get_pca_var(res.pca)
head(var$coord)
head(var$contrib)
fviz_pca_var(res.pca, col.var = "black")
fviz_pca_biplot(res.pca, repel = TRUE)
fviz_pca_biplot(res.pca, 
                label = "none",
                habillage = decathlon2$Competition,
                addEllipses = TRUE,
                groups = decathlon2$Competition
                )




iris.pca <- PCA(iris[,-5], graph = FALSE)

# Visualize
# Use habillage to specify groups for coloring
fviz_pca_ind(iris.pca,
             label = "none", # hide individual labels
             habillage = iris$Species, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE # Concentration ellipses
)
