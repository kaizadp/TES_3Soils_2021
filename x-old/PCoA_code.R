rm(list=ls())
library("ggplot2")
library("vegan")
library("ape")


###################
######PCOA Analysis
###################
###################
setwd("~/Documents/3_soils/metaT_drought_analysis/")
g_tab = read.table("metaT_combined_0.001perc_removed.txt", sep="\t", header=TRUE, row.names=1)


#####Convert all the na's to zeros
g_tab[is.na(g_tab)] = 0



#####Make a matrix that excludes all the metadata and is just numbers with the sample numbers as the row names
NAMES = rownames(g_tab)
g_matrix = g_tab[,7:1210]
rownames(g_matrix) = NAMES
g_matrix = as.matrix(g_matrix)


NAMES = rownames(g_tab)
g_sample = g_tab[,1:6]
rownames(g_sample) = NAMES


#####relative abundance normalization (You may not want to do this for your data)

g_rel = make_relative(g_matrix)
merged = merge(g_sample, g_rel, by="row.names")


###### Calculate distances and put data in PCA format

bray_distance = vegdist(g_rel, method="euclidean")
principal_coordinates = pcoa(bray_distance)

pcoa_plot = data.frame(principal_coordinates$vectors[,])
pcoa_plot_merged = merge(pcoa_plot, g_sample, by="row.names")

####### Calculate percent variation explained by PC1, PC2

PC1 <- 100*(principal_coordinates$values$Eigenvalues[1]/sum(principal_coordinates$values$Eigenvalues))
PC2 <- 100*(principal_coordinates$values$Eigenvalues[2]/sum(principal_coordinates$values$Eigenvalues))
PC3 <- 100*(principal_coordinates$values$Eigenvalues[3]/sum(principal_coordinates$values$Eigenvalues))

###### Plot PCoA

ggplot(data=pcoa_plot_merged,aes(x=Axis.1,y=Axis.2,group=Treatment)) + geom_point(aes(fill=factor(Treatment)), shape=21,colour="black", size=6) + theme_bw()  +
  theme_bw(base_size=14) + 
  stat_ellipse(aes(color=Treatment))+
  theme(axis.text=element_text(size=14,color="black"),axis.title=element_text(size=14),legend.background = element_rect(colour = "black"),
        legend.text = element_text(size=18), legend.title=element_text(size=20)) + labs(fill = "Group")+theme(legend.title=element_blank())+
  labs(x = paste("PC1 - Variation Explained", round(PC1,2),"%"), y = paste("PC2 - Variation Explained", round(PC2,2),"%"))

###### Significance testing

adonis(g_matrix ~ g_tab$Site+g_tab$Treatment+g_tab$Site*g_tab$Treatment, method="bray", permutations=999)

