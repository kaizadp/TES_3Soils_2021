## metaG
## TPC
## 3Soils

# 1. load packages and files ----------------------------------------------

library("funrar")
library("vegan")
library("ape")
library("reshape2")
library("DESeq2")
library("preprocessCore")
library("ggplot2")
library("dplyr")


###################
######MetaG PCA Analysis
###################
###################

g_tab = read.table("data/metagt/metaG_combined_0.001perc.txt", sep="\t", header=TRUE, row.names=1)

#####Convert all the na's to zeros
g_tab[is.na(g_tab)] = 0

#####Remove all ribosomal genes
g_tab = subset(g_tab, select = -c(TIGR00001,	TIGR00002,	TIGR00009,		
                                  TIGR00030,	TIGR00059,	TIGR00060,	TIGR00061,	TIGR00062,	TIGR00105,	TIGR00158,	
                                  TIGR00165,	TIGR00717,	
                                  TIGR00731,	TIGR00855,	TIGR00952,	TIGR00981,	TIGR01009,	
                                  TIGR01011,	TIGR01017,	TIGR01021,	TIGR01022,	
                                  TIGR01023,	TIGR01024,	TIGR01029,	TIGR01030,	TIGR01031,	
                                  TIGR01032,	TIGR01044,	TIGR01049,	TIGR01050,	TIGR01066,	
                                  TIGR01067,	TIGR01071,	TIGR01079,	TIGR01125,	TIGR01164,	
                                  TIGR01169,	TIGR01171,	TIGR01632,	
                                  TIGR03631,	TIGR03632,	
                                  TIGR03635,	TIGR03654,	TIGR03953))

#####Make a matrix that excludes all the metadata and is just numbers with the sample numbers as the row names
NAMES = rownames(g_tab)
g_matrix = g_tab[,6:1537]
rownames(g_matrix) = NAMES
g_matrix = as.matrix(g_matrix)
quantile(g_matrix)

NAMES = rownames(g_tab)
g_sample = g_tab[,1:2]
rownames(g_sample) = NAMES

#####relative abundance normalization

g_rel = make_relative(g_matrix)
merged = merge(g_sample, g_rel, by="row.names")


#calculate edistances

e_distance = vegdist(g_rel, method="euclidean")
principal_coordinates = pcoa(e_distance)

pcoa_plot = data.frame(principal_coordinates$vectors[,])
pcoa_plot_merged = merge(pcoa_plot, g_sample, by="row.names")

# 3. Calculate percent variation explained by PC1, PC2

PC1 <- 100*(principal_coordinates$values$Eigenvalues[1]/sum(principal_coordinates$values$Eigenvalues))
PC2 <- 100*(principal_coordinates$values$Eigenvalues[2]/sum(principal_coordinates$values$Eigenvalues))
PC3 <- 100*(principal_coordinates$values$Eigenvalues[3]/sum(principal_coordinates$values$Eigenvalues))

# 4. Plot PCoA

pcoa_plot_merged$Treatment = factor(pcoa_plot_merged$Treatment, levels = c("Drought","Field_Moist","Sat_II","Sat_I"))
ggplot(data=pcoa_plot_merged,aes(x=Axis.1,y=Axis.2)) + geom_point(aes(fill=factor(Treatment),shape=factor(Site)),  size=6,alpha=0.95) + theme_bw()  +
  theme_kp() + 
  labs(x = paste("PC1 - Variation Explained", round(PC1,2),"%"), y = paste("PC2 - Variation Explained", round(PC2,2),"%")) +
  scale_fill_manual(values=c("Sat_II"= "#443a83ff","Field_Moist"="#35b779ff","Drought"="#fde725ff","Sat_I"="grey70"),
                    labels=c("Drought"="drought","Field_Moist"="field moist","Sat_II"="flood","Sat_I"="time zero saturation"))+
  scale_color_manual(values=c("Sat_II"="#35b779ff","Field_Moist"="#fde725ff","Drought"="#443a83ff","Sat_I"="grey70"))+
  scale_shape_manual(values=c(21,22,24),labels=c("CPCRW"="Alaska","DWP"="Florida","SR"="Washington"))+
  guides(fill=guide_legend(override.aes=list(shape=21)))

adonis(g_matrix ~ g_tab$Site+g_tab$Treatment+g_tab$Site*g_tab$Treatment, method="euclidean", permutations=999)



################

###########################################
#                                         #
#    metaG top ten heatmap per site       #
#                                         #
###########################################



library("funrar")
library("vegan")
library("ape")
library("reshape2")
library("DESeq2")
library("preprocessCore")
library("ggplot2")
#library("cividis")



g_tab = read.table("data/metagt/metaG_combined_0.001perc.txt", sep="\t", header=TRUE, row.names=1)


#####Convert all the na's to zeros
g_tab[is.na(g_tab)] = 0

###Remove base_line samples
g_tab = subset(g_tab, Treatment!="Base_Line")

#####Remove ribosomal proteins
g_tab = subset(g_tab, select = -c(TIGR03533,  TIGR00001,	TIGR00002,	TIGR00009,	
                                  TIGR00030,	TIGR00059,	TIGR00060,	TIGR00061,	TIGR00062,	TIGR00105,	TIGR00158,	
                                  TIGR00165,	TIGR00406,	TIGR00717,	
                                  TIGR00731,	TIGR00855,	TIGR00952,	TIGR00981,	TIGR01009,	
                                  TIGR01011,	TIGR01017,	TIGR01021,	TIGR01022,	
                                  TIGR01023,	TIGR01024,	TIGR01029,	TIGR01030,	TIGR01031,	
                                  TIGR01032,	TIGR01044,	TIGR01049,	TIGR01050,	TIGR01066,	
                                  TIGR01067,	TIGR01071,	TIGR01079,	TIGR01125,	TIGR01164,	
                                  TIGR01169,	TIGR01171,	TIGR01575,	TIGR01632,	
                                  TIGR03631,	TIGR03632,	
                                  TIGR03635,	TIGR03654,	TIGR03953))


#####Make a matrix that excludes all the metadata and is just numbers with the sample numbers as the row names
NAMES = rownames(g_tab)
g_matrix = g_tab[,6:1534]
rownames(g_matrix) = NAMES
g_matrix = as.matrix(g_matrix)
quantile(g_matrix)

NAMES = rownames(g_tab)
g_sample = g_tab[,1:2]
rownames(g_sample) = NAMES

############################
###########################
#####relative abundance normalization

g_rel = make_relative(g_matrix)

###########################
###########################
###########################

transposed_g_matrix = t(g_matrix)
dds = DESeqDataSetFromMatrix(countData = transposed_g_matrix,
                             colData = g_sample,
                             design = ~Site)
dds = estimateSizeFactors(dds)
dds = DESeq(dds)
res = results(dds)
resultsNames(dds)
res


############Coloring
library("viridis")
library("pheatmap")
npgpal=viridis_pal(option="viridis")(85)
ntd = normTransform(dds)

#########LDA heatmap based on all treatments --- Top ten genes per site
draw_colnames_90 <- function (coln, gaps, ...){
  coord = pheatmap:::find_coordinates(length(coln), gaps)
  x = coord$coord - 0.5 * coord$size
  res = textGrob(coln, x=x, y = unit(1, "npc") - unit(3,"bigpts"), vjust = 0.5, hjust = 1, rot =90, gp=gpar(...))
  return(res)}

assignInNamespace(x="draw_colnames", value="draw_colnames_90",
                  ns=asNamespace("pheatmap"))


map_colors = colorRampPalette(cividis(99))

####
combined_site = c("TIGR01818",	"TIGR01817",	"TIGR02915",	"TIGR02329",	"TIGR02974",	"TIGR02533",	"TIGR02538",	"TIGR01420",	"TIGR00786",	"TIGR02348",	"TIGR01804",	"TIGR00711",	"TIGR02299",	"TIGR02100",	"TIGR01780",	"TIGR03971",	"TIGR04284",	"TIGR03216",	"TIGR02891",	"TIGR02882",	"TIGR02956",	"TIGR03265",	"TIGR01187",	"TIGR04056",	"TIGR01184",	"TIGR02142",	"TIGR02314",	"TIGR02966",	"TIGR02073",	"TIGR02211")


levels(g_sample$Treatment)
levels(g_sample$Treatment) = c("drought", "field moist", "time zero saturation","flood")
levels(g_sample$Site)
levels(g_sample$Site) = c("Alaska","Florida","Washington")


kd_3soil_colors = list(
  Treatment = c("drought"="#fde725ff","field moist"="#35b779ff","flood"="#443a83ff","time zero saturation"="grey70"),
  Site = c("Alaska" = "#b84634","Florida"="#e6ab00","Washington"="#008cff"))


pheatmap(assay(ntd)[combined_site,],cluster_cols=FALSE,cluster_rows=FALSE,annotation_col = g_sample, color=cividis(99),
         annotation_colors=kd_3soil_colors, show_colnames = FALSE,
         labels_row = c("TIGR01818 Glutamine Synthase Regulator",	"TIGR01817 Nitrogen Fixation Regulation",	"TIGR02915 Transcription Factor",	"TIGR02329 Propionate Catabolism",	"TIGR02974 Phage Shock Protein Activator",	"TIGR02533 Type II Secretion System",	
                        "TIGR02538 Type IV-A Pilus",	"TIGR01420 Pilus Retraction Protein PilT",	"TIGR00786 TRAP Transporter",	"TIGR02348 Chaperonin GroL",	"TIGR01804 Betaine-Aldehyde Dehydrogenase",	"TIGR00711 EmrB Efflux Transporter",	"TIGR02299 5-carboxymethyl-2-hydroxymuconate semialdehyde dehyrogenase",
                        "TIGR02100 Glycogen Debranching Enzyme",	"TIGR01780 Succinate-Semialdehyde Dehydrogenase",	"TIGR03971 Mycofactocin-Dependent Oxidoreductase",	"TIGR04284 Aldehyde Dehydrogenase",	"TIGR03216 2-Hydroxymuconic Semialdehyde Dehydrogenase",	"TIGR02891 Cytochrome C oxidase",	
                        "TIGR02882 Cytochrome aa3 Quinol Oxidase",	"TIGR02956 TMAO Reductase System Sensor TorS",	"TIGR03265 2-Aminoethylphosphonate ABC Transporter",	"TIGR01187 Spermidine/Putrescine ABC Transporter",	"TIGR04056 Outer Member Protein",	"TIGR01184 Nitrate Transporter",	"TIGR02142 Molybdenum ABC Transporter",	
                        "TIGR02314 D-methionine ABC Transporter",	"TIGR02966 Phosphate Regulon Sensor Kinase PhoR",	"TIGR02073 Penicillin-Binding Protein 1C",	"TIGR02211 Lipoprotein Releasing System"))

##########
## OUTPUT ----

