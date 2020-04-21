## Functions
# Kaizad F. Patel

## packages ####
library(readxl)
library(ggplot2)       # 2.1.0
library(readr)         # 1.0.0
library(lubridate)     # 1.6.0
library(stringr)       # 1.1.0
library(luzlogr)       # 0.2.0
library(tidyr)
library(readr)
library(tidyverse)
library(Rmisc)
library(ggplot2)
library(data.table)
library(cowplot)
library(qwraps2)
library(knitr)
library(reshape2)
library(ggalt)
library(ggExtra)
library(stringi)
library(nlme)
library(car)
library(agricolae)
library(googlesheets)
library(gsheet)
library(multcomp)
library(DescTools)
library(dplyr)         # 0.5.0


# create a custom ggplot theme
theme_kp <- function() {  # this for all the elements common across plots
  theme_bw() %+replace%
    theme(legend.position = "top",
          legend.key=element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.key.size = unit(1.5, 'lines'),
          panel.border = element_rect(color="black",size=1.5, fill = NA),
          
          plot.title = element_text(hjust = 0.05, size = 14),
          axis.text = element_text(size = 14, color = "black"),
          axis.title = element_text(size = 14, face = "bold", color = "black"),
          
          # formatting for facets
          panel.background = element_blank(),
          strip.background = element_rect(colour="white", fill="white"), #facet formatting
          panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
          panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
          strip.text.x = element_text(size=12, face="bold"), #facet labels
          strip.text.y = element_text(size=12, face="bold", angle = 270) #facet labels
          )
}


# create a custom ggplot function for Van Krevelen plots
gg_vankrev <- function(data,mapping){
  ggplot(data,mapping)+
    # plot points
    geom_point(size=2, alpha = 0.4)+ # set size and transparency
    # axis labels
    ylab("H/C")+
    xlab("O/C")+
    # axis limits
    xlim(0,1.25)+
    ylim(0,2.5)+
    # add boundary lines for Van Krevelen regions
    geom_segment(x = 0.0, y = 1.5, xend = 1.2, yend = 1.5,color="black",linetype="longdash")+
#   geom_segment(x = 0.0, y = 2, xend = 1.2, yend = 2,color="black",linetype="longdash")+
#   geom_segment(x = 0.0, y = 1, xend = 1.2, yend = 0.75,color="black",linetype="longdash")+
#   geom_segment(x = 0.0, y = 0.7, xend = 1.2, yend = 0.5,color="black",linetype="longdash")+
    geom_segment(x = 0.0, y = 0.8, xend = 1.2, yend = 0.8,color="black",linetype="longdash")
    
}

## to make the Van Krevelen plot:
# replace the initial `ggplot` function with `gg_vankrev` and use as normal


# DATA_DIR               <- "data/"
# OUTPUT_DIR		         <- "outputs/"


## CREATE OUTPUT FILES
# CHARACTERIZATION
CHARACTERIZATION <- "processed/soil_characterization.csv"
PORE_DISTRIBUTION <- "processed/pore_distribution.csv"
  

CORE_WEIGHTS <- "processed/core_weights.csv"

# RESPIRATION
FLUX_DATA <- "processed/flux_data.csv"
FLUX_SUMMARY <- "processed/flux_summary.csv"

  
# WSOC
WSOC_PORE <- "processed/wsoc_pore_summary.csv"
WSOC_SOIL <- "processed/wsoc_soils_summary.csv"

# FTICR_PORE_INITIAL FILES
FTICR_SOIL_META <- "fticr/fticr_soil_meta.csv"
FTICR_SOIL_META_HCOC <- "fticr/soil_meta_hcoc.csv"
FTICR_SOIL_RAW_LONG <- "fticr/fticr_soil_raw_longform.csv"
FTICR_SOIL_LONG <- "fticr/fticr_soil_longform.csv"

FTICR_PORE_META <- "fticr/fticr_pore_meta.csv"
FTICR_PORE_LONG <- "fticr/fticr_pore_longform.csv"
FTICR_PORE_RAW_LONG <- "fticr/fticr_pore_raw_longform.csv"

#
FTICR_PORE_RELABUND <- "fticr/fticr_pore_relabundance_groups2_hsd.csv"
FTICR_PORE_PEAKS <- "fticr/fticr_pore_peakscount.csv"
FTICR_PORE_UNIQUE <- "fticr/fticr_pore_uniquemolecules.csv"
FTICR_PORE_UNIQUE_PEAKS <- "fticr/fticr_pore_unique_peakscount.csv"
FTICR_PORE_HCOC <- "fticr/fticr_pore_hcoc.csv"
FTICR_PORE_NOSC <- "fticr/fticr_pore_nosc.csv"
FTICR_PORE_AROMATIC <- "fticr/fticr_pore_aromatic_counts.csv"
FTICR_PORE_DIVERSITY <- "fticr/fticr_pore_shannondiversity.csv"

# FTICR_SOIL
FTICR_SOIL_RELABUND <- "fticr/fticr_soil_relabundance_hsd.csv"
FTICR_SOIL_UNIQUE <- "fticr/fticr_soil_uniquemolecules.csv" 
FTICR_SOIL_HCOC <- "fticr/fticr_soil_hcoc.csv"
FTICR_SOIL_NOSC <- "fticr/fticr_soil_nosc.csv"
FTICR_SOIL_AROMATIC <- "fticr/fticr_soil_aromatic_counts.csv"
FTICR_SOIL_PEAKS <- "fticr/fticr_soil_peakscount.csv"
FTICR_SOIL_UNIQUE_PEAKS <- "fticr/fticr_soil_unique_peakscount.csv"





